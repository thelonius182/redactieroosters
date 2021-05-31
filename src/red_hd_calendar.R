cal_start_ti <- redacteuren_hedendaags_lst %>% 
  filter(!is.na(start_op)) %>% 
  mutate(start_op = min(start_op)) %>% 
  select(start_op) %>% distinct()

cal_start <- cal_start_ti$start_op %>% as.Date.POSIXct()  


# create time series ------------------------------------------------------
hd_red_days <- seq(from = cal_start, to = current_run_stop, by = "days")
hd_red_hours <- seq(0, 23, by = 1)
hd_red_dates <- merge(hd_red_days, chron(time = paste(hd_red_hours, ":", 0, ":", 0)))
colnames(hd_red_dates) <- c("slot_date", "slot_time")
hd_red_dates$date_time <- as.POSIXct(paste(hd_red_dates$slot_date, hd_red_dates$slot_time), "GMT")
row.names(hd_red_dates) <- NULL

# + create cz-slot prefixes and combine with calendar ---------------------
hd_red_dates_raw <- as.data.frame(hd_red_dates) %>%
  select(date_time) %>%
  mutate(cz_week = 1 + (day(date_time) - 1) %/% 7L,
         hd_red_day = wday(date_time, label = T, abbr = T),
         hd_red_hour = str_pad(hour(date_time),
                               width = 2,
                               side = "left",
                               pad = "0"
                       )
  ) %>%
  arrange(date_time)

hd_red_wissels <- redacteuren_hedendaags_lst %>% 
  filter(!is.na(start_op)) %>% 
  mutate(itm = row_number())

hd_red_joins <- NULL

for (h1 in hd_red_wissels$itm) {
  wissel <- hd_red_wissels %>% filter(itm == h1)
  
  # redacteurencaroussel
  if (is.na(wissel$cz_week)) {
    
    redacteurnamen <- redacteuren_hedendaags_lst %>% 
      filter(is.na(cz_week)) %>% 
      mutate(rid = row_number()) %>% 
      select(rid, redacteur)
    
    hd_red_dates_appd <- hd_red_dates_raw %>% 
      filter(hd_red_day == wissel$dag
             & hd_red_hour == wissel$uur) %>% 
      mutate(cid = row_number(),
             rid = 1 + (cid -1) %% nrow(redacteurnamen)) %>% 
      left_join(redacteurnamen) %>% 
      select(date_time, redacteur)
    
    if (is.null(hd_red_joins)) {
      hd_red_joins <- hd_red_dates_appd
    } else {
      hd_red_joins %<>% bind_rows(hd_red_dates_appd)
    }
    
  } else {
    
    redacteurnamen <- redacteuren_hedendaags_lst %>% 
      filter(dag == wissel$dag
             & uur == wissel$uur
             & cz_week == wissel$cz_week) %>% 
      mutate(rid = row_number()) %>% 
      select(rid, redacteur)
    
    hd_red_dates_appd <- hd_red_dates_raw %>% 
      filter(hd_red_day == wissel$dag
             & hd_red_hour == wissel$uur
             & cz_week == wissel$cz_week) %>% 
      mutate(cid = row_number(),
             rid = 1 + (cid -1) %% nrow(redacteurnamen)) %>% 
      left_join(redacteurnamen) %>% 
      select(date_time, redacteur)
    
    if (is.null(hd_red_joins)) {
      hd_red_joins <- hd_red_dates_appd
    } else {
      hd_red_joins %<>% bind_rows(hd_red_dates_appd)
    }
    
  }
}
