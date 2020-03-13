# .primary <- read_csv("data/ma_primary_20.csv") %>% 
#   extract(-1,) %>% 
#   # Remove the AE, AW from Chicopee ward names
#   mutate_at(vars(Pct), ~{str_extract(., "^\\w")}) %>%
#   # re-summarize
#   group_by(`City/Town`, Ward, Pct) %>% 
#   summarize_all(sum)
# write_csv(.primary, "data/ma_primary_20.csv")

.primary <- read_csv("data/ma_primary_20.csv")
.c <- rlang::sym(names(.primary)[str_which(names(.primary), .candidate)])
.per_support <- .primary %>% 
  mutate_at(vars(`City/Town`, Ward, Pct), tolower) %>% 
  inset(str_which(.[["City/Town"]], "manchester"), "City/Town", value = "manchester") %>% 
  mutate(support_20 = (!! .c) / `Total Votes Cast`, WP_NAME = paste(`City/Town`, Ward, Pct)) %>% 
  select(
    WP_NAME,
    support_20, 
    total_20 = `Total Votes Cast`,
    )

