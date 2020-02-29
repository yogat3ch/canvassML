zip_compare <- readRDS("rds/zip_compare.rds")
library(mice)
zip_compare_mice <- mice::mice(zip_compare, m = 1, maxit = 2, method = "rf")
zip_compare <- mice::complete(zip_compare_mice)
# Remove all columns with remaining NA
zip_compare <- zip_compare[, !unlist(purrr::map(zip_compare, ~{any(is.na(.x))}))]
saveRDS(zip_compare, "rds/zip_compare.rds")
