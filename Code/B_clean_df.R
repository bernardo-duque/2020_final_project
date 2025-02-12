
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

# pivoting
df <- df %>%
    pivot_wider(names_from = crime, values_from = date, values_fn = length, values_fill = 0) %>%
    select(everything(),car_theft,contains("(r|R)obbery"))

# agreggating roberries
df <- df %>%
    mutate(robbery = across(car_theft:last_col(),sum(.x))) %>%
    select(everything(),-contains("(r|R)obbery"),cargo_robbery,car_robbery) %>%
    mutate(retaliation_index = homicide + car_theft + cargo_robbery + car_robbery)

# creating data frame with all dates within the period
dates <- seq(as.Date("2006-01-01"), as.Date("2020-06-30"), by = "day")

df_date <- dates %>%
    left_join(df)

# identifying the events
df_date <- df_date %>%
    mutate(event = ifelse(police_killing > 0,1,0))