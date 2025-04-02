
load(paste0(wd,"Input/df_date.rda"))

## prepare data for event study
# extract event day

events <- df_date %>%
  filter(event == 1) %>%        
  select(place_id, event_date = date) 

#### 7 day-window ####
df_event <- df_date %>%
  left_join(events, by = "place_id",relationship = "many-to-many") %>% # we expect a many to many
  mutate(relative_day = as.integer(date - event_date)) %>%
  filter(relative_day >= -7 & relative_day <= 7) %>%
  arrange(place_id, event_date, date)

# checking if all looks good
table(df_event$relative_day)

# simple summary
df_event_summary <- df_event %>%
  group_by(relative_day) %>%
  summarise(
    mean_crime = mean(retaliation_index, na.rm = TRUE),
    sd_crime   = sd(retaliation_index, na.rm = TRUE),
    n          = n(),
    ci_low = mean_crime - 1.96*sd_crime,
    ci_high = mean_crime + 1.96*sd_crime
  )

pd <- position_dodge(0.1)

ggplot(df_event_summary, aes(x = relative_day, y = mean_crime)) + 
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high),  colour= "#1e81b0",width=.1, position=pd) +
  #geom_line(position=pd, colour= "#76b5c5") +
  geom_point(position=pd, colour= "#76b5c5", size = 1.5) +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  geom_vline(xintercept=c(-1), linetype="solid", color = "red",
             alpha = 0.4) +
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(face = "bold")) + 
  scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) +
  labs(y = "Coefficient", x= "Time to Treat (Treatment = 0)")

# Using lm
reg <- lm(retaliation_index ~ factor(relative_day) - 1,df_event)
reg_test <- coeftest(reg, vcov = vcovHC(reg,"HC0",cluster = "place_id"))
reg_test[, "Estimate"] <- reg_test[,"Estimate"] - reg_test[8,"Estimate"]

es <- tibble(relative_day = -7:7,
                  coef = reg_test[1:15,"Estimate"],se = reg_test[1:15,"Std. Error"])

es <- es %>%
  mutate(ci_low = coef - 1.96*se,
         ci_high = coef + 1.96*se)

ggplot(es, aes(x = relative_day, y = coef)) + 
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high),  colour= "#1e81b0",width=.1, position=pd) +
  #geom_line(position=pd, colour= "#76b5c5") +
  geom_point(position=pd, colour= "#76b5c5", size = 1.5) +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  geom_vline(xintercept=c(-1), linetype="solid", color = "red",
             alpha = 0.4) +
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(face = "bold")) + 
  scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) +
  labs(y = "Coefficient", x= "Time to Treat (Treatment = 0)")


# 14 day-window
df_event_14 <- df_date %>%
  left_join(events, by = "place_id", relationship = "many-to-many") %>%
  mutate(relative_day = as.integer(date - event_date)) %>%
  filter(relative_day >= -14 & relative_day <= 14) %>%
  arrange(place_id, event_date, date)

table(df_event_14$relative_day)
