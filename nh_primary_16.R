ss <- googlesheets4::sheets_get("https://docs.google.com/spreadsheets/d/1x9x8U1wTBOIvFU8McLPv-Yz77gbVXJcIlRmxnfM_Bm4/edit#gid=1481011078")
# Get the NH sheets
.ws <- googlesheets4::sheets_sheets("1x9x8U1wTBOIvFU8McLPv-Yz77gbVXJcIlRmxnfM_Bm4") %>% grep("^pp", ., value = T) %>%
  # Remove the summation sheet
  extract(-2)
.nh <- purrr::map(.ws, ~{
  googlesheets4::sheets_read("1x9x8U1wTBOIvFU8McLPv-Yz77gbVXJcIlRmxnfM_Bm4", sheet = .x, range = "A:H")
})
# Remove the county summary sheet
.nh[[2]] <- NULL
# if two counties are represented in the same table, split the table
.nh <- bind_rows(.nh) %>%
  # Remove blank first row and the republican cols
  extract(-1, - c(2:5)) %>% 
  # Give names
  {set_names(., value = c("City.Town", "Regular", "Absentee", "Total"))} %>% 
  # Extract the county name into a column
  mutate(County = ifelse(str_detect(City.Town, "County"), str_extract(City.Town, ".*(?=\\sCounty)"), NA)) %>% 
  # Fill this down
  tidyr::fill(one_of("County"), .direction = "down") %>% 
  # Drop all the null rows
  drop_na() %>% 
  # Remove the header and total rows
  filter(Regular != "Regular" & City.Town != "Totals") %>% 
  unnest(cols = c(City.Town, Regular, Absentee, Total))
write_csv(.nh, "data/party_aff_nh.csv")

.ws <- readxl::excel_sheets('data/nh_primary_16.xls') %>%
  subset(subset = !str_detect(., "Summary"))
.xls <- purrr::map(.ws, ~{
  .out <- readxl::read_xls('data/nh_primary_16.xls', sheet = .x)
    if (any(is.na(.out[1,, drop = T]))) {
    #if any NA in the first column, shift data from first row down to provide a row with the future column names
    .out[2, is.na(.out[2,] %>% unlist)] <- .out[1, !is.na(.out[1,] %>% unlist)]
    # then remove the first row
    .out <- .out[-1,]
  }
  if (anyDuplicated(.out[1,, drop = T] %>% unlist)) {
    # Remove the identifier column for the 2nd page in Coos County
    .out <- .out[!duplicated(.out[1,, drop = T] %>% unlist)]
  } 
  .out %<>% 
    #set the first line to the col names
    {set_names(., .[1,])} %>% 
    # remove the names row
    magrittr::extract(-1,) %>% 
    # coerce all columns except the first to numeric
    mutate_at(vars(- 1), as.numeric)
  .out
})

## ----'2020 Replace NA'-----------------------------------------------
.xls <- map(.xls, ~{
  .x %>%
    purrr::map_dfc(~{
      replace_na(.x, 0)
    })
})

## ----'Derive the percent support for candidate'----------------------
.per_support <- .xls %>% 
  purrr::map(~{
    # total for candidate by W/P
    .out <- {.x[str_which(.x[[1]], regex(.candidate, ignore_case = T)), -c(1, length(.x))] / colSums(.x[ ,-c(1, length(.x))])} %>% 
      # Account for divide by 0
      purrr::map_dfc(~{
        replace_na(.x, 0)
      }) %>% 
      t %>% 
      as.data.frame()
    .out2 <- colSums(.x[ ,-c(1, length(.x))]) %>% 
      as.data.frame()
    cbind.data.frame(.out, .out2) %>% 
      rownames_to_column("WP_NAME")
  }) %>% 
  bind_rows %>% 
  set_names(c('WP_NAME', 'support_16', 'total_16')) %>% 
  replace_na(list(`Bernie Sanders` = 0))
# Add the total number of voters as a feature for weighted.mean when precincts are aggregated by zip code or congressional district

#Transform county names to make them more amenable to joining with the shapefiles:  Tue Feb 25 17:18:58 2020 ----
# replace dashes
.per_support$WP_NAME <- str_replace(.per_support$WP_NAME, "\\-", "") %>%
  # replace multiple spaces
  str_replace("\\s{2,}", "\\s") %>% 
  # Use full words instead of abbreviations
  str_replace("Gt\\.?$", "Grant") %>% 
  str_replace("Pur\\.?$", "Purchase") %>% 
  str_replace("Loc\\.?$", "Location") %>% 
  str_replace("\\*$", "") 
  
write_csv(.per_support, "data/nh_primary_16_sanders.csv")

 
  
