
load(paste0(wd,"Input/df_date.rda"))

n_events <- sum(df_date$event)

mean_events_preccint <- df_date %>%
  group_by(year,place_id) %>%
  summarise(n_events = sum(event)) %>%
  group_by(year) %>%
  summarise(mean_events = mean(n_events),
            sd_events = sd(n_events)) %>%
  round(2)

mean_events_preccint_month <- df_date %>%
  group_by(year,month,place_id) %>%
  summarise(n_events = sum(event)) %>%
  group_by(year,month) %>%
  summarise(mean_events = mean(n_events),
            sd_events = sd(n_events)) %>%
  round(2)


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

