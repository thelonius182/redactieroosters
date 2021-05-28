library(dplyr)
library(tidyr)
library(magrittr)
library(stringr)
library(readr)
library(keyring)
library(RMySQL)
library(readtext)


# define wordpress connection ---------------------------------------------

get_wp_conn <- function() {
  db_type <- "prd"
  db_host <- key_get(service = paste0("sql-wp", db_type, "_host"))
  db_user <- key_get(service = paste0("sql-wp", db_type, "_user"))
  db_password <-
    key_get(service = paste0("sql-wp", db_type, "_pwd"))
  db_name <- key_get(service = paste0("sql-wp", db_type, "_db"))
  db_port <- 3306
  result <- tryCatch({
    grh_conn <-
      dbConnect(
        drv = MySQL(),
        user = db_user,
        password = db_password,
        dbname = db_name,
        host = db_host,
        port = db_port
      )
  },
  error = function(cond) {
    flog.error("Wordpress database onbereikbaar (1)", name = "rlsc_log")
    return("connection-error")
  })
  return(result)
}


# create plws-gidsweek ----------------------------------------------------

for (seg1 in 1:1) { # creates an exitable flow
  # Connect to database 
  wp_conn <- get_wp_conn()
  
  # connection type S4 indicates a valid connection; other types indicate failure
  if (typeof(wp_conn) != "S4") { 
    flog.error("Wordpress database onbereikbaar (2)", name = "rlsc_log")
    break
  }

  sql_cmd <- readtext(file = "src/sql_cmd_titel_genre_koppeling.txt", encoding = "UTF-8")
  
  tgk_db_rtv <- dbGetQuery(wp_conn, sql_cmd$text)
  
  tgk_db_raw <- tgk_db_rtv %>% 
    rename(titel_NL_db = pgmTitle, gids_url_sfx = kvValue) %>% 
    mutate(titel_NL_db = tolower(titel_NL_db),
           titel_NL_db = if_else(titel_NL_db == "dr. klangendum", "dr-klangendum", titel_NL_db))
  
  on.exit(dbDisconnect(wp_conn))
}

tgk_wp_raw <- tbl_wpgidsinfo %>% 
  # read_delim(
  #   "C:/Users/nipper/Downloads/cz_downloads/Wordpress gids-info - gids-info.tsv",
  #   "\t",
  #   escape_double = FALSE,
  #   trim_ws = TRUE
  # ) %>% 
  select(titel_NL_gd = titel_NL, mr_key = key_modelrooster, hoofdgenre = genre_NL1) %>% 
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
