library(dplyr)
library(tidyr)
library(magrittr)
library(stringr)
library(readr)

tgk_db_raw <-
  read_delim(
    "C:/Users/nipper/Downloads/cz_downloads/titel-genre-koppelingen.tsv",
    "\t",
    escape_double = FALSE,
    trim_ws = TRUE
  ) %>% 
  rename(titel_NL_db = pgmTitle, gids_url_sfx = kvValue) %>% 
  mutate(titel_NL_db = tolower(titel_NL_db),
         titel_NL_db = if_else(titel_NL_db == "dr. klangendum", "dr-klangendum", titel_NL_db))

tgk_wp_raw <-
  read_delim(
    "C:/Users/nipper/Downloads/cz_downloads/Wordpress gids-info - gids-info.tsv",
    "\t",
    escape_double = FALSE,
    trim_ws = TRUE
  ) %>% 
  select(titel_NL_gd = `titel-NL`, mr_key = `key-modelrooster`, hoofdgenre = `genre-1-NL`) %>% 
  mutate(titel_NL_gd = tolower(titel_NL_gd),
         titel_NL_gd = if_else(titel_NL_gd == "dr. klangendum", "dr-klangendum", titel_NL_gd),
         hoofdgenre = tolower(hoofdgenre))

gidslinks <- tgk_wp_raw %>% 
  left_join(tgk_db_raw, by = c("titel_NL_gd" = "titel_NL_db")) %>% 
  mutate(select_this = (str_detect(gids_url_sfx, hoofdgenre) | str_detect(gids_url_sfx, titel_NL_gd))) %>% 
  filter(select_this) %>% 
  select(-titel_NL_gd) %>% 
  distinct() %>% 
  group_by(mr_key) %>% 
  mutate(title_rank = row_number(desc(n_episodes))) %>% 
  arrange(mr_key, title_rank) %>% 
  filter(title_rank == 1) %>% 
  select(mr_key, gids_url_sfx)

# hide these checks
#
# tgk_check.1 <- tgk_wp_raw %>% 
#   left_join(tgk_join.1, by = c("mr_key" = "mr_key")) %>% 
#   filter(!is.na(gids_url_sfx))
# 
# tgk_check.2 <- tgk_wp_raw %>% 
#   left_join(tgk_join.1, by = c("mr_key" = "mr_key")) %>% 
#   filter(is.na(gids_url_sfx))
