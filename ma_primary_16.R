.primary <- read_csv("data/ma_primary_16.csv")
.primary %<>% 
  replace_na(list(`Bernie Sanders` = 0, `Total Votes Cast` = 0))
write_csv(.primary, "data/ma_primary_16.csv")
