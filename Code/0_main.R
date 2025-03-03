library(tidyverse)
library(stringr)
library(Hmisc)
library(data.table)
library(lubridate)

# change the main wd (repository) here
wd <- "/Users/bernardoduque/Documents/Brown/Aulas/2nd semester/Applied Economics/2020_final_project/"

#change your data wd here
wd_data <- "/Users/bernardoduque/Documents/Brown/Aulas/2nd semester/Applied Economics/Final Project/Data/Input/"

source(paste0(wd,"Code/A_concat_data.R"))

source(paste0(wd,"Code/B_clean_df.R"))

source(paste0(wd,"Code/B_df_es_df.R"))

source(paste0(wd,"Code/C_analsysis.R"))