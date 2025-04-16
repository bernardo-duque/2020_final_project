### Selecting relevant variables and 
### Concatenating all CSVs into a .rda

setwd(wd_data)

# name of the files to iterate
years <- c("2006-2007","2007-2009","2009-2011","2011-2012",
            "2012-2014", "2014-2015","2015-2017","2017-2018","2018-2020-junho") 

df <- tibble()

# crimes we are interested in
crimes <- c("Apreensão de drogas","Furto de veículos","Homicídio doloso","Latrocínio (Roubo seguido de morte)",
            "Morte por intervenção de agente do Estado"," Tentativa de homicídio","Roubo")

i <- 1
total_iterations <- length(years)
# read and concatenate all csvs selecting the relevant variables
for(year in years){
    temp <- read.csv2(paste0(year,".csv"),, fileEncoding = "latin1",header=T)
    temp <- temp %>%
         select(id = controle, year = ano, month = mes, crime=titulo_do,place = cisp,
                city = municipio_fato,neighborhood = bairro_fato, date = data_fato) %>%
         filter(city == "Rio de Janeiro (Capital)",
                            str_detect(crime,paste0(crimes,collapse="|")))

    df <- rbind(df,temp)
    message(sprintf("Iteration %d/%d", 
                    i, total_iterations))
    i <- i + 1
}

# adjust some variables
df <- df %>%
    mutate(place_id = substr(place,2,3)) %>% 
    select(id,year,month,crime,place_id, neighborhood, date)

# note that there can be more than one neighborhood in the same precint and one neighborhood can be in more than one precinct
#label(df$place_id) <- "Police precinct"

# save the dataframe
save(df, file = paste0(wd,"Input/df.rda"))

# remove everything that is not the wd paths
rm(list=setdiff(ls(), c("wd","wd_data")))
