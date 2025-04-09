
load(file = paste0(wd,"Input/df_7.rda"))
load(file = paste0(wd,"Input/df_14.rda"))

##### Number of events #####

num_years <- unique(df_7$year) %>%
  length()

num_events <- df_7 %>%
  filter(event > 0) %>%
  nrow()

max_events_year <- df_7 %>%
  group_by(year,place_id) %>%
  summarise(sum = sum(event)) %>%
  ungroup() %>%
  summarise(max = max(sum),
            min = min(sum))

summary <- df_7 %>%
  group_by(place_id) %>%
  summarise(event = sum(event)) %>%
  ungroup() %>%
  summarise(mean = mean(event),
            sd = sd(event),
            min = min(event),
            max = max(event),
            median = median(event))

summary_year <- df_7 %>%
  group_by(place_id) %>%
  summarise(event = sum(event)/num_years) %>%
  ungroup() %>%
  summarise(mean = mean(event),
            sd = sd(event),
            min = min(event),
            max = max(event),
            median = median(event))

##### Map of higher means #####

mean_place <- df_7 %>%
  group_by(place_id) %>%
  summarise(mean = sum(event)/num_years)

# reading the shape file

sf <- st_read(paste0(wd_data,"limite_cisp_072024/lm_cisp_bd.shp"))

sf <- sf %>%
  rename(place_id = cisp)

mean_place <- mean_place %>%
  mutate(place_id = as.double(place_id)) %>%
  left_join(sf %>% select(place_id,geometry))

mean_place %>%
  ggplot() +
  geom_sf(aes(fill=mean),
          alpha = 0.83, color = "white") +
  scale_fill_gradient(low = "navajowhite", high = "red", name = "Share",na.value = "grey50") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(hjust = 0.5,face = "bold"),
        axis.text = element_text(face = "bold",size = 14),
        #strip.background = element_rect(fill="#1e81b0"),
        strip.background = element_rect(fill="red"),
        strip.text = element_text(colour="white"),
        legend.position = c(0.18,0.23)
  ) +
  guides(fill = guide_legend(title = "Share (%)"))

Cairo::CairoJPEG("figures/mapa.jpeg", width = 8, height = 6,units = "in", dpi = 300)
print(mapa)
dev.off()





