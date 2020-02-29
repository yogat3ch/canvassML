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
