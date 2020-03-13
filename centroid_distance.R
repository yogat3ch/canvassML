start_cluster <- function(){
  library(furrr)
  file.remove("out.txt")
  cl <- parallel::makeCluster(4L, timeout = 60, outfile = "out.txt")
  plan(cluster, workers = cl)
  return(cl)
}
.candidate<- try(readRDS("rds/candidate.rds"))
library(magrittr)
shp <- readRDS(paste0(c("rds", .candidate, "shapes.rds"), collapse = "/"))
cl <- start_cluster()
.fo <- furrr::future_options(packages = c("sf", "purrr"))
.dists <- purrr::map(shp, ~{
 
  .shp <- .x
  .out <- .x$zip$centroid %>%
    set_names(values = .x$zip$zip) %>%
    furrr::future_map(shp = .shp, function(.x, shp){
    .zip <- .x
    .wp <- shp$wp$centroid
    names(.wp) <- shp$wp$WP_NAME
    .out <- purrr::imap(.wp, ~{as.numeric(sf::st_distance(x = .zip, y = .x))})
    return(.out)
    },
    .options = .fo,
    .progress = T
    )
  return(.out)
})
saveRDS(.dists, paste0(c("rds", .candidate, "dists.rds"), collapse = "/"))
parallel::stopCluster(cl)
