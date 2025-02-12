
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
                          "Roubo a estabelicimento comercial" = "Robbery of commercial establishment",
                          "Roubo a residência" = "Robbery of residence",
                          'Roubo a transeunte' = "Robbery of passerby",
                          "Roubo após saque em instituição financeira" = "Robbery after bank withdrawal",
                          "Roubo com condução da vítima para saque em instituição financeira" = "Robbery with victim taken to bank",
                          "Roubo de aparelho celular" = "Robbery of cell phone",
                          "Roubo de bicicleta" = "Bicycle robbery",
                          "Roubo de caixa eletrônico" = "Robbery of ATM",
                          "roubo de carga" = "Cargo robbery",
                          "Roubo de veículo" = "Vehicle robbery",
                          "Roubo em coletivo" = "Robbery in collective transport"
                          ))

# pivoting
df <- df %>%
    pivot_wider(names_from = crime, values_from = date, values_fn = length, values_fill = 0)

# agreggating roberries
df <- df %>%
    mutate(robbery = across(contains("(R|r)obbery"),sum(.x)))
