
load(paste0(wd,"Input/df_date.rda"))

### Descriptives ####

n_events <- sum(df_date$event)

n_events_preccint <- df_date %>%
  group_by(year,month,place_id) %>%
  summarise(n_events = sum(event)) 

mean_events_preccint <- df_date %>%
  group_by(year,place_id) %>%
  summarise(n_events = sum(event)) %>%
  group_by(year) %>%
  summarise(mean_events = mean(n_events),
            sd_events = sd(n_events),
            max = max(n_events),
            min = min(n_events)) %>%
  round(2)

mean_events_preccint_month <- df_date %>%
  group_by(year,month,place_id) %>%
  summarise(n_events = sum(event)) %>%
  group_by(year,month) %>%
  summarise(mean_events = mean(n_events),
            sd_events = sd(n_events),
            max = max(n_events),
            min = min(n_events)) %>%
  round(2)
