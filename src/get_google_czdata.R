library(googledrive)
library(keyring)
library(readxl)
library(yaml)

cz_extract_sheet <- function(ss_name, sheet_name) {
  readxl::read_xlsx(ss_name,
            sheet = sheet_name, 
            .name_repair = ~ ifelse(nzchar(.x), .x, LETTERS[seq_along(.x)]))
}

cz_get_url <- function(cz_ss) {
  cz_url <- paste0("url_", cz_ss)
  paste0("https://", config$url_pfx, config[[cz_url]])
}

# downloads GD ------------------------------------------------------------

# aanmelden bij GD loopt via gargle, zoals beschreven in "Operation Missa > Voortgang > 4.Toegangsrechten GD".

# Roosters 3.0 ophalen bij GD
path_roosters <- paste0(config$gs_downloads, "/", "roosters.xlsx")
drive_download(file = cz_get_url("roosters"), overwrite = T, path = path_roosters)

# iTunes-cupboard ophalen bij GD
path_itunes_cupboard <- paste0(config$gs_downloads, "/", "itunes_cupboard.xlsx")
drive_download(file = cz_get_url("itunes_cupboard"), overwrite = T, path = path_itunes_cupboard)

# WP-gids-info ophalen bij GD
path_wp_gidsinfo <- paste0(config$gs_downloads, "/", "wordpress_gidsinfo.xlsx")
drive_download(file = cz_get_url("wordpress_gidsinfo"), overwrite = T, path = path_wp_gidsinfo)

# redacteur_carrousel
path_redacteur_carrousel <- paste0(config$gs_downloads, "/", "redacteur_carrousel.xlsx")
drive_download(file = cz_get_url("redacteur_carrousel"), overwrite = T, path = path_redacteur_carrousel)
# 
# # Nipper-spreadsheet ophalen bij GD
# path_wp_nipper_express <- paste0(config$gs_downloads, "/", "nipper_express.xlsx")
# drive_download(file = cz_get_url("nipper_express"), overwrite = T, path = path_wp_nipper_express)

# sheets als df -----------------------------------------------------------

tbl_raw_zenderschema <- cz_extract_sheet(path_roosters, sheet_name = paste0("modelrooster-", config$modelrooster_versie))
tbl_raw_presentatie <- cz_extract_sheet(path_roosters, sheet_name = "presentatie")
tbl_raw_montage <- cz_extract_sheet(path_roosters, sheet_name = "montage")
tbl_raw_itunes_cupboard <- cz_extract_sheet(path_itunes_cupboard, sheet_name = "playlist_names")
tbl_raw_wpgidsinfo <- cz_extract_sheet(path_wp_gidsinfo, sheet_name = "gids-info")
tbl_raw_wpgidsinfo_nl_en <- cz_extract_sheet(path_wp_gidsinfo, sheet_name = "vertalingen NL-EN")
tbl_raw_redacteur_carrousel <- cz_extract_sheet(path_redacteur_carrousel, sheet_name = config$tab_redacteur_carrousel)

# refactor raw tables -----------------------------------------------------

source("src/refactor_raw_tables.R")
