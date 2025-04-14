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
  mutate(type = "Total", mean = num_events,median = "",sd = "") %>%
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
            mean_retaliation = sum(retaliation_index)/num_years)

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

# map of number of events
map_1 <- sf %>%
  ggplot() +
  geom_sf(aes(fill=mean_events),
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
  guides(fill = guide_legend(title = "# of Events "))

Cairo::CairoJPEG(paste0(wd,"Output/map_events.jpeg"), width = 8, height = 6,units = "in", dpi = 300)
print(map_1)
dev.off()

# map of retaliation
map_2 <- sf %>%
  ggplot() +
  geom_sf(aes(fill=mean_retaliation),
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
  guides(fill = guide_legend(title = "# of Retaliation"))

Cairo::CairoJPEG(paste0(wd,"Output/map_retaliation.jpeg"), width = 8, height = 6,units = "in", dpi = 300)
print(map_2)
dev.off()





