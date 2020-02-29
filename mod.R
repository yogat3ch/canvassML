library(kknn)
library(magrittr)
mod <- readRDS("mod.rds")
    .rv <- "support_20" #enter response variable name
    .dv <- NULL  # IF not using all vars, enter independent Variables as vector (c("var1","var2"))
      .dat <- mod$train # Enter Data Variable name here
      .methodlist <- c("kknn", "xgbTree", "naive_bayes", "") # Add method names. See http://topepo.github.io/caret/available-models.html for details
      # If first run, use tune length. If final or subsequent runs with a refined tuning grid, provide tuneGrids in the lapply call (not in it's function).
      # .tunelist <- purrr::imap(.methodlist, simplify = F, USE.NAMES = T, FUN = function(.x, .y, tuneLength = NULL, tuneGrids = NULL){
      #   if (is.numeric(tuneLength)){
      #     out <- caretEnsemble::caretModelSpec(method = .x, tuneLength = tuneLength)
      #   } else {
      #     out <- caretEnsemble::caretModelSpec(method = .x, tuneGrid = tuneGrids[[.x]])
      #   }
      #   return(out)
      #   })
      
      .probs <- ifelse(class(.dat[[.rv]]) == "numeric", FALSE, TRUE)
      
      .mod.packages <- c("caret","doParallel","iterators","parallel","foreach","caretEnsemble") # [package dependencies]
      HDA::startPkgs(.mod.packages)
      .tunelist <- list(
        # kknn = expand.grid(
        #   kmax = 3,
        #   distance = 2 ^ seq(-1,1,1),
        #   kernel = "optimal"),
        # xgbTree = expand.grid(
        #   nrounds = c(100),
        #   max_depth = 6:10,
        #   eta = seq(0,.5,.1),
        #   gamma = 10 ^ (-1:5),
        #   colsample_bytree = seq(0,1,.2),
        #   min_child_weight = 10 ^ (-1:5),
        #   subsample = seq(0,1,.5)),
        bagEarth = expand.grid(nprune = 7, 
                               degree = 1:2)
      )
      .tunelist <- purrr::imap(.tunelist, ~{
        caretModelSpec(method = .y,
                       trace = T,
                       tuneGrid = .x)
        
      })
      CL <- parallel::makePSOCKcluster(4, outfile = "out.txt")
      registerDoParallel(CL)
      sysNN1_mod <- system.time({
        .data.train <- caret::createDataPartition(.dat[[.rv]], times = 2, p = .85)
        .data.train <- caret::trainControl(method = "repeatedcv",
                                  index  = .data.train, 
                                  number = 5,
                                  repeats = 0, 
                                  search = "grid",
                                  allowParallel = T,
                                  summaryFunction = caret::defaultSummary,
                                  classProbs = .probs, 
                                  savePredictions = "final",
                                  returnResamp = "final",
                                  returnData = F,
                                  verboseIter = T
                                  )
        if (is.null(.dv)) {
           .form <- as.formula(paste0(.rv," ~ ."))
        } else {
           .form <- as.formula(paste0(.rv, " ~ ", paste(iv, collapse="+")))
        }
               mod <- tryCatch(
                 {
                 mod_final <- caretEnsemble::caretList(form = .form,
                                                  data = .dat,
                                                  trControl = .data.train,
                                                  tuneList = .tunelist,
                                                  continue_on_fail = T,
                                                  linout = T)
                   },
                 error = function(cond) {
                   message("Here's the original error message:")
                   message(cond)
                  RPushbullet::pbPost(title = "Model Error", body = glue::glue("{lubridate::now()}:\n{cond}"))
                   return(NA)
                   },
                   warning=function(cond) {
                   message("Here's the original warning message:")
                   message(cond)
                   # Choose a return value in case of warning
                   
                   },
                   finally={
                   # NOTE:
                   # Here goes everything that should be executed at the end,
                   # regardless of success or error.
                   # If you want more than one expression to be executed, then you 
                   # need to wrap them in curly brackets ({...}); otherwise you could
                   # just have written 'finally=<expression>'
                   registerDoSEQ()
                   }
                 )
             })
             
             
             HDA::unloadPkgs(.mod.packages)
             rm(list = grep("^\\.", ls(all.names = T), perl = T, value = T))
      save(mod_final, sysNN1_mod, file = "finalmod.Rdata")
      RPushbullet::pbPost(title = "Model Complete", body = glue::glue("Model build completed successfully at {lubridate::now()}"))
      
# ------------------------------------------------------------


