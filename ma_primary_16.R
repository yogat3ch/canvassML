.primary <- read_csv("data/ma_primary_16.csv")
.primary %<>% 
  extract(-1,) %>% 
  {set_names(., value = make.names(names(.)))}
