start_cluster <- function(){
  library(furrr)
  file.remove("out.txt")
  cl <- parallel::makeCluster(4L, timeout = 60, outfile = "out.txt")
  plan(cluster, workers = cl)
  return(cl)
}
library(magrittr)
shp <- readRDS("rds/shapes.rds")
cl <- start_cluster()
.fo <- furrr::future_options(packages = c("sf", "purrr"))
.dists <- purrr::map(shp, ~{
  .shp <- .x
  .out <- .x$zip$centroid %>% set_names(values = .x$zip$zip) %>% furrr::future_imap(shp = .shp, function(.x, .y, shp){
    .zip <- .x
    .wp <- shp$wp$centroid
    names(.wp) <- shp$wp$WP_NAME
    .out <- purrr::imap(.wp, ~{as.numeric(sf::st_distance(x = .zip, y = .x))})
    
  }, .options = .fo, .progress = T)
  return(.out)
})
saveRDS(.dists, "rds/dists.rds")
parallel::stopCluster(cl)
