load(paste0(wd,"Input/df_date.rda"))

##### Number of events #####

num_years <- unique(df_date$year) %>%
  length()

n_events <- sum(df_date$event)

max_events_year <- df_date %>%
  group_by(year,place_id) %>%
  summarise(sum = sum(event)) %>%
  ungroup() %>%
  summarise(max = max(sum),
            min = min(sum))

ovr <- max_events_year %>%
  mutate(type = "Total", mean = n_events,median = "",sd = "") %>%
  select(type,mean,median,sd,max,min)

# summary over the whole period within precincts
summary <- df_date %>%
  group_by(place_id) %>%
  summarise(event = sum(event)) %>%
  ungroup() %>%
  summarise(mean = mean(event),
            median = median(event),
            sd = sd(event),
            min = min(event),
            max = max(event)) %>%
  mutate(type = "Overall") %>%
  select(type, everything())

# annual summary within precincts
summary_year <- df_date %>%
  group_by(place_id) %>%
  summarise(event = sum(event)/num_years) %>%
  ungroup() %>%
  summarise(mean = mean(event),
            median = median(event),
            sd = sd(event),
            min = min(event),
            max = max(event)) %>%
  mutate(type = "Year") %>%
  select(type, everything())

# putting everything in one table
summary <- summary %>%
  rbind(summary_year) %>%
  mutate(across(mean:max,~round(.x,2))) %>%
  rbind(ovr)

### Mean per year ####

teste <- df_date %>%
  group_by(year,place_id) %>%
  summarise(events = sum(event)) %>%
  group_by(year) %>%
  summarise(mean_events = mean(events),
            sd_events = sd(events),
            max = max(events),
            min = min(events)) %>%
  round(2)

##### Summary on retaliation #####



##### Map of precinct annual means #####

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
