
load(file = paste0(wd,"Input/df_week.rda"))

##### retaliation index_1 #####

reg_1_0 <- feols(retaliation_index ~ police_killing_100k,
               cluster = ~place_id,
               data = df_week)

reg_1 <- feols(retaliation_index ~ police_killing_100k + lag(police_killing),
               cluster = ~place_id,
               data = df_week)

# now including fixed effects one at a time

reg_2_0 <- feols(retaliation_index ~ police_killing_100k | place_id,
               data = df_week)

reg_2 <- feols(retaliation_index ~ police_killing_100k + lag(police_killing) | place_id,
               data = df_week)

reg_3_0 <- feols(retaliation_index ~ police_killing_100k| place_id + year,
              data = df_week)

reg_3 <- feols(retaliation_index ~ police_killing_100k + lag(police_killing) | place_id + year,
               data = df_week)

reg_4_0 <- feols(retaliation_index ~ police_killing_100k | place_id + year + month(week_start),
               data = df_week)

reg_4 <- feols(retaliation_index ~ police_killing_100k + lag(police_killing) | place_id + year + month(week_start),
               data = df_week)

models <- list(
  reg_1_0,
  reg_1,
  reg_2_0,
  reg_2, 
  reg_3_0,
  reg_3, 
  reg_4_0,
  reg_4
)

# Compute mean of DV (assuming same DV across all models)
mean_dv <- df_week %>%
  filter(week(week_start) > 1) %>%
  summarise(mean = mean(retaliation_index)) %>%
  pull

t <- etable(
  models[c(2,4,6,8)],
  dict = c(
    "police_killing_100k" = "Trigger",
    "lag(police_killing)" = "Lag (Trigger)",
    "retaliation_index" = "Retaliation",
    "place_id" = "Precinct",
    "month(week_start)" = "Month",
    "year" = "Year"
  ),
  drop = "(Constant)", # remove the constant row
  extralines = list(
    "Mean of DV" = rep(round(mean_dv, 3), 4) ,# one entry per model
    "Cluster SE" = rep("Precinct",4)
  ),
  tex = TRUE,
  style.tex = style.tex("aer"),
  digits = 2,
  digits.stats = 2 ,
  fitstat = c("n", "ar2", "awr2")
)

cat(t, file = paste0(wd,"Output/reg_1_lag.tex"))

t <- etable(
  models,
  dict = c(
    "police_killing_100k" = "Trigger",
    "lag(police_killing)" = "Lag (Trigger)",
    "retaliation_index" = "Retaliation",
    "place_id" = "Precinct",
    "month(week_start)" = "Month",
    "year" = "Year"
  ),
  drop = "(Constant)", # remove the constant row
  extralines = list(
    "Mean of DV" = rep(round(mean_dv, 3), 8) #,# one entry per model
    #"Cluster SE" = rep("Precinct",8)
  ),
  tex = TRUE,
  style.tex = style.tex("aer"),
  digits = 2,
  digits.stats = 2 ,
  fitstat = c("n", "ar2", "awr2")
)

cat(t, file = paste0(wd,"Output/reg_1.tex"))

