
load(paste0(wd,"Input/df_date.rda"))

## prepare data for event study
# extract event day

df_event_dates <- df_date %>%
  filter(event == 1) %>%        
  select(place_id, date) %>%     
  mutate(event_date = date)

df_es <- df_date %>%
  left_join(df_event_dates)

df_es <- df_es %>%
  mutate(days_from_event = as.numeric(difftime(date, event_date, units = "days")))

