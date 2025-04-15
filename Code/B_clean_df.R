
# Cleaning Dataframe
## Changing crime names, month names and summing up

load(paste0(wd,"Input/df.rda"))

df <- df %>%
    mutate(crime = recode(crime,
                          "Apreensão de drogas" = "drugs",
                          "Furto de veículos" = "car_theft",
                          "Homicídio doloso" = "homicide",
                          "Latrocínio (Roubo seguido de morte)" = "Robbery followed by death",
                          "Morte por intervenção de agente do Estado" = "police_killing",
                          "Roubo a banco" = "Bank robbery",
                          "Roubo a estabelecimento comercial" = "Robbery of commercial establishment",
                          "Roubo a residência" = "Robbery of residence",
                          'Roubo a transeunte' = "Robbery of passerby",
                          "Roubo após saque em instituição financeira" = "Robbery after bank withdrawal",
                          "Roubo com condução da vítima para saque em instituição financeira" = "Robbery with victim taken to bank",
                          "Roubo de aparelho celular" = "Robbery of cell phone",
                          "Roubo de bicicleta" = "Bicycle robbery",
                          "Roubo de caixa eletrônico" = "Robbery of ATM",
                          "Roubo de carga" = "cargo_robbery",
                          "Roubo de veículo" = "car_robbery",
                          "Roubo em coletivo" = "Robbery in collective transport"
                          ))

# correcting date
df <- df %>%
    mutate(n =1,
            month = as.character(substr(date,6,7)),
            day = as.character(substr(date,9,10)),
           date = as.Date(paste(year,month,day,sep = "-"),format = "%Y-%m-%d")) 

# pivoting    
df <- df %>%
    group_by(date,year,month,day,place_id,crime) %>%
    summarise(crime_n = sum(n)) %>%
    pivot_wider(names_from = crime, values_from = crime_n) %>%
    select(date:place_id,police_killing,homicide,car_theft,cargo_robbery,car_robbery,contains("robbery", ignore.case = TRUE))

# replace na for 0
df <- df %>%
    mutate(across(police_killing:last_col(), ~ifelse(is.na(.x),0,.x))) 

# agreggating roberries
setDT(df)
start_col <- which(names(df) == "car_theft")

# create 'robbery' as the sum of all columns from 'car_theft' to the last column
df[, robbery := rowSums(.SD, na.rm = T), .SDcols = start_col:ncol(df)]
  
# construct retaliation indexes
df <- df %>%
    mutate(retaliation_index = homicide + car_theft + cargo_robbery + car_robbery,
            retaliation_index_2 = homicide + robbery) 

#  creating data frame with all dates within the period for all police precincts
dates <- tibble(date = seq(as.Date("2006-01-01"), as.Date("2020-12-30"), by = "day"))

dates <- expand_grid(date = dates$date, place_id = unique(df$place_id))
label(dates$place_id) <- "Police precinct"

dates <- dates %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date))

df <- df %>%
  mutate(month = month(date),
         day = day(date))

df_date <- dates %>%
    left_join(df)

# identifying the events
df_date <- df_date %>%
    mutate(event = ifelse(police_killing > 0,1,0))

# NA to 0
df_date <- df_date %>%
  mutate(across(police_killing:last_col(), ~ifelse(is.na(.x),0,.x))) 

##### Adjust crimes by precinct population #####

pop <- read.csv2(paste0(wd_data,"PopulacaoEvolucaoAnualCisp.csv"),header = T)

pop <- pop %>%
  rename(place_id = circ,year = ano)

# setting the class of one of the key variable to be equal in both dfs
df_date <- df_date %>% 
  mutate(place_id = as.double(place_id)) %>%
  left_join(pop)

# adjust every crime and police killing variable by 100k inhabitants
df_date <- df_date %>%
  mutate(across(homicide:retaliation_index_2,~.x*100000/pop,.names = "{.col}_100k"))

save(df_date, file = paste0(wd,"Input/df_date.rda"))


##### Prepare data for event study #####
# extract event day

events <- df_date %>%
  filter(event == 1) %>%        
  select(place_id, event_date = date) 

#### 7 day-window ####
df_7 <- df_date %>%
  left_join(events, by = "place_id",relationship = "many-to-many") %>% # we expect a many to many
  mutate(relative_day = as.integer(date - event_date)) %>%
  filter(relative_day >= -7 & relative_day <= 7) %>%
  arrange(place_id, event_date, date)

# number of overlaped observations
sum(duplicated(df_7[,c("date","place_id")]))

df_7 %>%
  mutate(n=1) %>%
  group_by(place_id,date,event) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  filter(n > 1) %>%
  nrow()

# 14 day-window
df_14 <- df_date %>%
  left_join(events, by = "place_id", relationship = "many-to-many") %>%
  mutate(relative_day = as.integer(date - event_date)) %>%
  filter(relative_day >= -14 & relative_day <= 14) %>%
  arrange(place_id, event_date, date)


save(df_7, file = paste0(wd,"Input/df_7.rda"))
save(df_14, file = paste0(wd,"Input/df_14.rda"))
  
  
##### Preparing data for regression (agregating by week) #####

df_week <- df_date %>%
  mutate(week_index = week(date)) %>%
  group_by(year,week_index, place_id) %>%
  summarise(
    week_start = min(date),
    across(police_killing:retaliation_index_2, ~ sum(.x, na.rm = TRUE)),
    event = sum(event, na.rm = TRUE) > 0,
    pop = max(pop)
  ) %>%
  ungroup() %>%
  mutate(across(police_killing:retaliation_index_2,~.x*100000/pop,.names = "{.col}_100k"))

save(df_week, file = paste0(wd,"Input/df_week.rda"))

# remove everything that is not the wd paths
rm(list=setdiff(ls(), c("wd","wd_data")))

