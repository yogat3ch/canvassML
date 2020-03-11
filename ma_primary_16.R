 .primary <- read_csv("data/ma_primary_16.csv")
# .primary %<>% 
#   replace_na(list(`Bernie Sanders` = 0, `Total Votes Cast` = 0))
# write_csv(.primary, "data/ma_primary_16.csv")
 .primary %<>% 
  # Remove the AE, AW from Chicopee ward names
  mutate_at(vars(Pct), ~{str_extract(., "^\\w")}) %>%
  # re-summarize
  group_by(`City/Town`, Ward, Pct) %>% 
  summarize_all(sum) 
# Acton Precinct 5 is missing. We'll replace it with the mean
 .primary <- .primary %>%
  filter(str_detect(`City/Town`, "Acton")) %>% 
  select( - Ward, - Pct) %>% 
  group_by(`City/Town`) %>% 
  summarise_all(~round(mean(.),0)) %>% 
  magrittr::inset("Ward", value = "-") %>% 
  magrittr::inset("Pct", value = "5") %>% 
  rbind.data.frame(.primary) %>% 
  ungroup %>%
  mutate(support_16 = `Bernie Sanders` / `Total Votes Cast`) %>% 
  select(City.Town = `City/Town`,
        Ward, Pct, support_16,
        total_16 = `Total Votes Cast`) %>%
  mutate_at(vars(City.Town, Ward, Pct), tolower)
# Manchester-by-the-sea is referred to as Manchester in the shapefile
.primary[str_detect(.primary$City.Town, "manchester"), "City.Town"] <- "manchester"
write_csv(.primary, "data/ma_primary_16_sanders.csv")
