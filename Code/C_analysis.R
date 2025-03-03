
load(paste0(wd,"Input/df_date.rda"))

n_events <- sum(df_date$event)

mean_events_preccint <- df_date %>%
  group_by(year,place_id) %>%
  summarise(n_events = sum(event)) %>%
  group_by(year) %>%
  summarise(mean_events = mean(n_events),
            sd_events = sd(n_events)) %>%
  round(2)
