load(paste0(wd,"Input/df_date.rda"))


##### 1. Table (Summary Statistics) #####

num_years <- unique(df_date$year) %>%
  length()

# summary_table <- function(data = df_date, var,indicator_100k = FALSE) {
# 
#   # total sum of var across all rows
#   total_sum <- sum(data[[var]], na.rm = TRUE)
# 
#   # max and min by precinct-year
#   max_min <- data %>%
#     group_by(year, place_id) %>%
#     summarise(sum = sum(.data[[var]], na.rm = TRUE), .groups = "drop") %>%
#     summarise(
#       max = max(sum, na.rm = TRUE),
#       min = min(sum, na.rm = TRUE),
#       .groups = "drop"
#     )
# 
#   # "total" row
#   ovr <- max_min %>%
#     mutate(
#       type   = "Total",
#       mean   = total_sum,
#       median = "",
#       sd     = ""
#     ) %>%
#     select(type, mean, median, sd, min, max)
# 
#   # summary: sum(var) per precinct, then average across precincts
#   summary_overall <- data %>%
#     group_by(place_id) %>%
#     summarise(var_sum = sum(.data[[var]], na.rm = TRUE), .groups = "drop") %>%
#     summarise(
#       mean   = mean(var_sum),
#       median = median(var_sum),
#       sd     = sd(var_sum),
#       min    = min(var_sum),
#       max    = max(var_sum)
#     ) %>%
#     mutate(type = "Overall") %>%
#     select(type, everything())
# 
#   # year summary: sum(var) per precinct / number of years, then average
#   summary_year <- data %>%
#     group_by(place_id) %>%
#     summarise(var_sum = sum(.data[[var]], na.rm = TRUE) / num_years, .groups = "drop") %>%
#     summarise(
#       mean   = mean(var_sum),
#       median = median(var_sum),
#       sd     = sd(var_sum),
#       min    = min(var_sum),
#       max    = max(var_sum)
#     ) %>%
#     mutate(type = "Year") %>%
#     select(type, everything())
# 
#   # combine everything
#   final_summary <- summary_overall %>%
#     rbind(summary_year) %>%
#     mutate(across(mean:max, ~ round(.x, 2))) %>%
#     rbind(ovr)
# 
#   return(final_summary)
# }
# 
# 
# # apply summary function to each variable of interest
# variables <- c("event","retaliation_index","retaliation_index_2",
#                "retaliation_index_100k","retaliation_index_2_100k")
# 
# summary_final <- list()
# i <- 1
# for (var in variables) {
#   if (var %in% c("retaliation_index_100k", "retaliation_index_2_100k")) {
#     temp <- summary_table(var = var,indicator_100k = T))
#   } else{
#     temp <- summary_table(var = var)
#   }
# 
#   summary_final[[i]] <- temp
#   i <- i + 1
# 
# }
# 
# # combine everything in a single table
# summary_final <- lapply(summary_final,"rbind")
# 
# # now prepare the latex table

### 2. Plots (Averages per precincts) ####

plot <- df_date %>%
  group_by(year,place_id) %>%
  summarise(pop_year = max(pop,na.rm=T),
            events = sum(event),
            ri_100k = sum(retaliation_index,na.rm=T)*100000/pop_year,
            ri_2_100k = sum(retaliation_index_2,na.rm=T)*100000/pop_year) %>%
  group_by(year) %>%
  summarise(mean_events = mean(events),
            sd_events = sd(events),
            max_events = max(events),
            min_events = min(events),
            mean_ri = mean(ri_100k),
            sd_ri = sd(ri_100k),
            max_ri = max(ri_100k),
            min_ri = min(ri_100k),
            mean_ri_2 = mean(ri_2_100k),
            sd_ri_2 = sd(ri_2_100k),
            max_ri_2 = max(ri_2_100k),
            min_ri_2 = min(ri_2_100k)) %>%
  round(2)

p_tidy <- plot %>%
  pivot_longer(
    cols = -year,                      # pivot everything except 'year'
    names_to = c("stat", "group"),     # split column names into 'stat' & 'group'
    names_pattern = "(mean|sd|max|min)_(.*)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = stat,      # create columns 'mean', 'sd', 'max', 'min'
    values_from = value
  ) %>%
  select(year, group, mean, sd, max, min) %>%
  mutate(group = case_when(
    group == "events" ~ "Events",
    group == "ri" ~ "RI",
    group == "ri_2" ~ "RI 2",
    TRUE ~ NA
  ))

p <-  p_tidy %>%
  filter(group != "Events") %>%
  ggplot(aes(x = year, y = mean, color = group, linetype = group)) + 
  geom_line() +
  geom_point(size = 0.5) +
  xlab("Year") + ylab("Average Retaliation (100k) by Precinct") +
  theme_bw() + 
  theme(
    legend.position = c(0.5, 0.8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(face = "bold")
  ) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) +
  scale_fill_manual(values=c("lightpink", "deeppink")) +
  scale_color_manual(values=c("lightpink", "deeppink"))


p
  

Cairo::CairoJPEG(paste0(wd,"Output/mean_ri.jpeg"), width = 8, height = 6,units = "in", dpi = 300)
print(p)
dev.off()

p <-  p_tidy %>%
  filter(group == "Events") %>%
  ggplot(aes(x = year, y = mean, color = group, linetype = group)) + 
  geom_line() +
  geom_point(size = 0.5) +
  xlab("Year") + ylab("Average Number of Events by Precinct") +
  theme_bw() + 
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(face = "bold")
  ) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) +
  scale_fill_manual(values=c("deeppink")) +
  scale_color_manual(values=c("deeppink"))


p


Cairo::CairoJPEG(paste0(wd,"Output/mean_events.jpeg"), width = 8, height = 6,units = "in", dpi = 300)
print(p)
dev.off()


##### 3. Maps (Average per year) #####

mean_place <- df_date %>%
  group_by(place_id) %>%
  summarise(mean_events = sum(event)/num_years,
            mean_events_100k = sum(event)*100000/(max(pop,na.rm=T) * num_years),
            mean_retaliation_100k = sum(retaliation_index_100k,na.rm = T)/num_years, # some preccints changed over time so they don't have all the population
            mean_retaliation_2_100k = sum(retaliation_index_2_100k,na.rm = T)/num_years,
            mean_retaliation = sum(retaliation_index,na.rm = T)/num_years, 
            mean_retaliation_2 = sum(retaliation_index_2,na.rm = T)/num_years)

# reading the shape file

sf <- st_read(paste0(wd_data,"limite_cisp_072024/lm_cisp_bd.shp"))

sf <- sf %>%
  rename(place_id = cisp)

# filtering only relevant precincts
places_id <- unique(mean_place$place_id)

sf <- sf %>%
  select(place_id,geometry) %>%
  filter(place_id %in% places_id) %>%
  left_join(mean_place)

# plotting maps

vars <- c("mean_events","mean_events_100k","mean_retaliation","mean_retaliation_2",
          "mean_retaliation_100k","mean_retaliation_2_100k")
total_iterations <- length(vars)
i <- 1
for (var in vars) {
  
  var_short <- case_when(
    var == "mean_events" ~ "events",
    var == "mean_events_100k" ~ "events_100k",
    var == "mean_retaliation" ~ "ri_1",
    var == "mean_retaliation_100k" ~ "ri_1_100k",
    var == "mean_retaliation_2" ~ "ri_2",
    var == "mean_retaliation_2_100k" ~ "ri_2_100k",
    TRUE ~ NA
  )
  
  title <- case_when(
    var == "mean_events" ~ "# of Events",
    var == "mean_events_100k" ~ "# of Events (100k)",
    var == "mean_retaliation" ~ "RI 1",
    var == "mean_retaliation_100k" ~ "RI 1 (100k)",
    var == "mean_retaliation_2" ~ "RI 2",
    var == "mean_retaliation_2_100k" ~ "RI 2 (100k)",
    TRUE ~ NA
  )
  
  p <-  sf %>%
   ggplot() +
   geom_sf(aes(fill=!!sym(var)),
           alpha = 0.95, color = "white") +
   scale_fill_gradient(low = "lightpink", high = "deeppink", name = "# of Events",na.value = "grey50") +
   theme_bw() +
   theme(panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         axis.title = element_blank(),
         #axis.text = element_blank(),
         axis.ticks = element_blank(),
         #panel.grid = element_blank(),
         plot.title = element_text(hjust = 0.5,face = "bold"),
         #strip.background = element_rect(fill="#1e81b0"),
         strip.text = element_text(colour="white"),
         legend.position = c(0.1,0.8),
         legend.title = element_text(size = 10),
         legend.text = element_text(size = 8),
         legend.key.size = unit(0.5, "cm"),
         legend.spacing = unit(0.2, "cm"),
   ) +
   guides(fill = guide_legend(title = title))
 
 Cairo::CairoJPEG(paste0(wd,"Output/map_",var_short,".jpeg"), width = 8, height = 6,units = "in", dpi = 300)
 print(p)
 dev.off()
 
 # progress print
 message(sprintf("Iteration %d/%d:  var = %s", 
                 i, total_iterations,var))
 i <- i + 1
}

# remove everything that is not the wd paths
rm(list=setdiff(ls(), c("wd","wd_data")))
