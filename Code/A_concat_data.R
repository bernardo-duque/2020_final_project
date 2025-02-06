### Selecting relevant variables and 
### Concatenating all CSVs into a .rda

setwd(wd_data)

years <- c("2006-2007","2007-2009","2009-2011","2011-2012",
            "2012-2014", "2014-2015","2015-2017","2017-2018","2018-2020-junho") 

df <- tibble()
crimes <- c("Apreensão de drogas","Furto de veículos","Homicídio doloso","Latrocínio (Roubo seguido de morte)",
            "Morte por intervenção de agente do Estado"," Tentativa de homicídio","Roubo")

for(year in years){
    temp <- read.csv2(paste0(year,".csv"),, fileEncoding = "latin1",header=T)
    temp <- temp %>%
         select(id = controle, year = ano, month = mes, crime=titulo_do,place = cisp,
                city = municipio_fato,neighborhood = bairro_fato, date = data_fato) %>%
         filter(city == "Rio de Janeiro (Capital)",
                            str_detect(crime,paste0(crimes,collapse="|")))

    df <- rbind(df,temp)
    rm(temp)
}

df <- df %>%
    mutate(place_id = substr(place,2,3)) %>% 
    select(id,year,month,crime,place_id, neighborhood, date)

# note that there can be more than one neighborhood in the same precint and one neighborhood can be in more than one precinct
label(df$place_id) <- "Police precinct"

save(df, file = paste0(wd,"Input/df.rda"))
