library(dplyr)
library(magrittr)
library(furrr)
.nf <- readRDS("rds/candidate.rds")
## ----allocate------------------------------------------------------------------
mod <- readRDS(paste0(c("rds", .nf, "mod.rds"), collapse = "/"))
predictions <- try({readRDS(paste0(c("rds", .nf, "predictions.rds"), collapse = "/"))})
if (class(predictions) == "try-error") predictions <- list()
rv <- "support_20"
.rv <- as.symbol(as.character(rv))
.formula <- as.formula(paste0(rv,' ~ .'))
## ----'create future cluster'---------------------------------------------------
start_cluster <- function(ac, timeout = 60*60*24*30, outfile){
  # Remove the previous outfile if it exists
  if (file.exists(outfile)) {
    file.remove(outfile)  
  }
  # create the cluster with timout and outfile
  cl <- parallel::makeCluster(ac, timeout = timeout, outfile = outfile)
  # Function that handles conditions and writes them to file
  write_to_file <- function(cond) {
    cond_class <- class(cond)[1]
    msg <- paste(cond_class, ":", cond$message)
    write(msg, file = outfile, append=TRUE)
  }
  
  catch <- catchr::make_catch_fn(
    warning   = c(write_to_file, muffle),
    message = c(write_to_file, muffle),
    error        = c(write_to_file, catchr::exit_with("Returned error!"))
  )
  
  return(list(cl = cl, catch = catch))
}

cl <- start_cluster(6, outfile = "out_log.txt")
future::plan(future::cluster, workers = cl$cl)
doParallel::registerDoParallel(cl$cl)
## ----'manual model build earth'------------------------------------------------
# Initial hyperparameter tuning for bagEarth:  Fri Feb 07 12:51:57 2020 ----
.tG <- expand.grid(#pmethod = c('exhaustive'),
  nprune = 5:10,
  #nfold = 2:5,
  degree = 1:3,
  stringsAsFactors = F)
.train <- caret::trainControl(
  method = 'cv',
  number = 4,
  repeats = 2,
  p = .2,
  search = 'grid',
  returnData = F,
  returnResamp = 'final',
  savePredictions = 'final')
# Find tuning parameters for bagEarth
timing <- new.env()
.mod <- try(readRDS(paste0(c("rds", .nf, "caretbagEarth.rds"), collapse = "/")))
assign("begin", lubridate::now(), envir = timing)
if (class(.mod) == "try-error") {
  message(paste0("Beginning bagEarth at ", e$begin))
  .mod <- caret::train(.formula, data = mod$train, trControl = .train, tuneGrid = .tG, method = 'bagEarth')
  saveRDS(.mod, paste0(c("rds", .nf, "caretbagEarth.rds"), collapse = "/"))
}
# Get the top performing tuning parameters:  Fri Feb 07 12:54:27 2020 ----
.top <- .mod$results %>% 
  arrange(desc(Rsquared)) %>% 
  head(3) 
.nprune <- sort(unique(x = .top[['nprune']]))
.degree <- sort(unique(x = .top[['degree']]))
# regenerate the grid
.tG <- expand.grid(#pmethod = c('cv'),
                   nprune = .nprune,
                   nfold = c(3,5,10),
                   degree = .degree,
                   stringsAsFactors = F) %>% 
  tibble::rownames_to_column("id") %>% 
  mutate_at(vars(id), as.numeric) %>% 
  # Move the column to last position
  select( - id, id)
.mods <- try(readRDS(paste0(c("rds", .nf, "bagEarth.rds"), collapse = "/")))
if (class(.mods) == "try-error") {
  message(paste0("Tuning iterations for bagEarth: ", nrow(.tG)))
  # rerun the model with the additional tuning parameters
  # .f0 <- furrr::future_options(globals = c("mod", ".formula"), packages = c("caret"))
  # .mods <- furrr::future_pmap(.tG, ~{
  #   cat(paste0("Now running tuningGrid iteration: ", ..5))
  #   .mod <- caret::bagEarth(formula = .formula, data = mod$train, pmethod = ..1, nprune = ..2, nfold = ..3, degree = ..4)
  # }, .progress = T, .options = .f0)
  # This ran for 5 hours with no progress indicated. Rebuilding with foreach to surface messages
  library(foreach)
  .i <- purrr::imap(.tG, ~return(.x))
  .mods <- foreach::foreach(
    nprune = .i$nprune,
    nfold = .i$nfold,
    degree = .i$degree,
    id = .i$id,
   #pmethod = .i$pmethod,
    .errorhandling = "pass",
    .packages = c("caret"),
    .export = c("mod", ".formula")
  ) %dopar% {
    message(paste0(timestamp(quiet = T), "\nbagEarth: Now running tuningGrid iteration: ", id))
    .mod <- caret::bagEarth(
      formula = .formula,
      data = mod$train,
      #pmethod = pmethod,
      nprune = nprune,
      nfold = nfold,
      degree = degree,
      keepX = F,
      B = 10,
      trace = .5
    )
    message(paste0(timestamp(quiet = T), "\nbagEarth: End tuningGrid iteration: ", id))
    .mod
  }
  saveRDS(.mods, paste0(c("rds", .nf, "bagEarth.rds"), collapse = "/"))
  # attach caret to the bagEarth future prediction
}
if (!HDA::go("predictions$earth")) {
  message(paste0("Conclude bagEarth build at ", lubridate::now(), "... predicting"))
  .fO <- furrr::future_options(packages = 'caret')
  .pred <- furrr::future_map(.mods, .options = .fO, ~cl$catch({
    predict(.x, newdata = mod$train)
  }), .progress = T)
  # get the RMSE
  .rmse <- purrr::map_dbl(.pred, ~{caret::RMSE(mod$train[[rv]], .x)})
  # Get the top 10 lowest error
  .top3 <- percent_rank(.rmse) %>%
    {
      # get the top 10%
      .x <- (. < .1)
      # if more than 10
      if (sum(.x) > 3) {
        # order and get only 3 indexes
        . %>% 
          order %>% 
          magrittr::extract(1:3)
      } else if (sum(.x) <= 3 & sum(.x) > 0) {
        # if between 10 & 1, just return the logical vector
        .x
      } else {
        # if none are above .90%, just return all
        rep(TRUE, length(.))
      }
    }
  message("bagEarth:s predicting on test data")
  .pred <- purrr::map(.mods[.top3], ~{
    predict(.x, newdata = mod$test)
  })
  
  predictions$earth <- .pred %>% do.call(cbind,.) %>% rowMeans()
  saveRDS(predictions, paste0(c("rds", .nf, "predictions.rds"), collapse = "/"))
}
assign("end", lubridate::now(), envir = timing)
message(paste0("Time elapsed: ", timing$end - timing$begin))


## ----'MARS predictors'---------------------------------------------------------
get.used.pred.names <- function(obj) # obj is an earth object
{
  any1 <- function(x) any(x != 0) # like any but no warning if x is double
  names(which(apply(obj$dirs[obj$selected.terms, , drop = FALSE], 2, any1)))
}
# (
#   .selected_terms <- purrr::imap(.mods[.top10], ~{
#     purrr::map(.x$fit, ~{
#       earth::evimp(.x) %>%
#         as.matrix %>%
#         row.names()}) %>%
#       unlist
#   }) %>%
#     unlist %>%
#     table %>%
#     sort %>%
#     tail %>%
#     data.table::fsort(decreasing = T)
# ) %>% kableExtra::kable('html') %>% kableExtra::kable_styling(position = 'center')


## ----'manual knn'-----------------------------------------------
assign("begin", lubridate::now(), envir = timing)
.tG <- expand.grid(ks = 2:5,
                   distance = 2 ^ -2:2,
                   kernel = c('gaussian', 'optimal'),
                   stringsAsFactors = F) %>% 
  tibble::rownames_to_column("id") %>% 
  mutate_at(vars(id), as.numeric) %>% 
  # Move the column to last position
  select( - id, id)
.f0 <- furrr::future_options(globals = c("mod", ".formula"), packages = 'kknn')
.mods <- try(readRDS(paste0(c("rds", .nf, "kknn.rds"), collapse = "/")))
if (class(.mods) == "try-error") {
  message(paste0("Beginning kknn at ", timing$begin))
  .mods <- furrr::future_pmap(.tG, ~{
    message(paste0(timestamp(quiet = T), "\nkknn: Now running tuningGrid iteration: ", ..4))
    .mod <- kknn::train.kknn(.formula, data = mod$train, ks = ..1, distance = ..2, kernel = ..3)
    message(paste0(timestamp(quiet = T), "\nkknn: End tuningGrid iteration: ", ..4))
    .mod
  }, .progress = T, .options = .f0)
  if (any(purrr::map_lgl(.mods, is.null))) stop("model returned NULL values")
  saveRDS(.mods, paste0(c("rds", .nf, "kknn.rds"), collapse = "/"))
}
if (!HDA::go("predictions$kknn")) {
  .pred <- furrr::future_map(.mods, ~{
    predict(.x, newdata = mod$train)
  }, .options = .f0)
  .rmse <- purrr::map_dbl(.pred, ~{caret::RMSE(mod$train[[rv]], .x)})
  .top3 <- percent_rank(.rmse) %>%
    {
      # get the top 10%
      .x <- (. < .1)
      # if more than 10
      if (sum(.x) > 3) {
        # order and get only 3 indexes
        . %>% 
          order %>% 
          extract(1:3)
      } else if (sum(.x) <= 3 & sum(.x) > 0) {
        # if between 10 & 1, just return the logical vector
        .x
      } else {
        # if none are above .90%, just return all
        rep(TRUE, length(.))
      }
    }
  library(kknn)
  .pred <- purrr::map(.mods[.top3], ~{
    predict(.x, newdata = mod$test)
  })
  HDA::unloadPkgs("kknn")
  predictions$kknn <- .pred %>% do.call(cbind,.) %>% rowMeans()
  saveRDS(predictions, paste0(c("rds", .nf, "predictions.rds"), collapse = "/"))
}
message(paste0("Conclude kknn at ", assign("end", lubridate::now(), envir = timing)))
message(paste0("Time elapsed: ", timing$end - timing$begin))

## ----'manual xgboost', eval = F---------------
.mod <- try(readRDS(paste0(c("rds", .nf, "caretxgbTree.rds"), collapse = "/")))
.tG <- expand.grid(
  nrounds = c(100),
  max_depth = c(3,6,10),
  eta = seq(.1,.4,.1),
  gamma = 10 ^ c(-1:-3),
  colsample_bytree = seq(0,1,.25),
  min_child_weight = 10 ^ c(-2:1),
  subsample = seq(0,1,.25),
  stringsAsFactors = F) %>% 
  tibble::rownames_to_column("id") %>% 
  mutate_at(vars(id), as.numeric) %>% 
  # Move the column to last position
  select( - id, id)
if (class(.mod) == "try-error") {
    .train <- caret::trainControl(
    method = 'cv',
    number = 4,
    p = .8,
    search = 'grid',
    returnData = F,
    returnResamp = 'final',
    savePredictions = 'final',
    verboseIter = T)
    .mod <- caret::train(.formula, data = mod$train, trControl = .train, tuneGrid = .tG[-length(.tG)], method = 'xgbTree', nthread = 2, verbose = 1)
    saveRDS(.mod, paste0(c("rds", .nf, "caretxgbTree.rds"), collapse = "/"))
}
.mods <- try(readRDS(paste0(c("rds", .nf, "xgboost.rds"), collapse = "/")))
assign("begin", lubridate::now(), envir = timing)
if (class(.mods) == "try-error") {
.top <- .mod$results %>% 
      arrange(desc(Rsquared)) %>% 
      head(3) 
.vars <- purrr::imap(names(.tG)[-length(.tG)] %>% set_names(., .), ~{sort(unique(x = .top[[.x]]))})
.tG <- expand.grid(.vars, stringsAsFactors = F) %>% 
      tibble::rownames_to_column("id") %>% 
      mutate_at(vars(id), as.numeric) %>% 
      # Move the column to last position
      select( - id, id)
message(paste0("Beginning xgBoost at ", timing$begin))
  .f0 <- furrr::future_options(packages = c('xgboost'), globals = c("mod", "rv"))
  .mods <- furrr::future_pmap(.tG, ~ cl$catch({
    message(paste0(timestamp(quiet = T), "\nxgboost: Now running tuningGrid iteration: ", ..8))
    # TODO Adding the extra column and passing all parameters as list could error the xgboost model build, or it could ignore unnecessary parameters.
    .mod <- xgboost::xgb.train(data = xgboost::xgb.DMatrix(data = as.matrix(mod$train[-1]), label = mod$train[[rv]]), params = list(...), nrounds = ..1)
    message(paste0(timestamp(quiet = T), "\nxgboost: End tuningGrid iteration: ", ..8))
    .mod
  }), .progress = T, .options = .f0)
  if (any(purrr::map_lgl(.mods, is.null))) stop("model returned NULL values")
  saveRDS(.mods, paste0(c("rds", .nf, "xgboost.rds"), collapse = "/"), compress = F)
  
}
## ----'manual xgboost cont'--------------------------------
if (!HDA::go("predictions$xgboost")) {
  .f0 <- furrr::future_options(packages = c('xgboost'), globals = c("mod", ".formula"))
  .pred <- furrr::future_map(.mods, ~{
    predict(.x, newdata = 
              xgboost::xgb.DMatrix(
                as.matrix(
                  mod$train[-1])))
  }, .progress = T, .options = .f0)
  .rmse <- purrr::map_dbl(.pred, ~{caret::RMSE(mod$train[[rv]], .x)})
  .top3 <- percent_rank(.rmse) %>%
    {
      
      
      # if more than 10
      if (sum((. < .1)) > 3) {
        # order and get only 3 indexes
        order(.) %>% 
          magrittr::extract(1:3)
      } else if (sum((. < .1)) <= 3 & sum((. < .1)) > 0) {
        # if between 10 & 1, just return the logical vector
        (. < .1)
      } else {
        # if none are above .90%, just return all
        rep(TRUE, length(.))
      }
    }
  .pred <- purrr::map(.mods[.top3], ~{
    predict(.x, newdata = 
              xgboost::xgb.DMatrix(
                as.matrix(mod$test[!names(mod$test) %in% rv])))
  })
  predictions$xgboost <- .pred %>% do.call(cbind,.) %>% rowMeans()
  saveRDS(predictions, paste0(c("rds", .nf, "predictions.rds"), collapse = "/"))
}

message(paste0("Conclude xgBoost at ", assign("end", lubridate::now(), envir = timing)))
message(paste0("Time elapsed: ", timing$end - timing$begin))

## ----'stop future cluster'-----------------------------------------------------
parallel::stopCluster(cl$cl)


RPushbullet::pbPost(title = "Model Complete", body = glue::glue("Model build completed successfully at {lubridate::now()}"))