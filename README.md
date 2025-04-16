# 2020_final_project

## Data
- Download the main data from the [Google Drive link](https://drive.google.com/drive/folders/1lKwyqCeULDtQPDPZjV9jcbPxE9F62D7j?usp=drive_link)

- Download population and precinct shapefiles from: https://www.ispdados.rj.gov.br/Conteudo.html
    - Population: on "Divisão por Grupos" on the left click on "População" and then on the csv button under "População: série anual por área de delegacia (2000 a 2021)"
    - Shapefiles: on "Divisão por Grupos" on the left click on "Divisão Territorial de Segurança Pública" and then on the shapefile button under "Bases Cartográficas Digitais – Circunscrições Integradas de Segurança Pública (CISP) - Limites de 2019"


## Code
- The code is organized into the three series:

    - A: concatenation of the data

    - B: data cleaning (creates data frames for descriptives, event study and regression)

    - C: data analysis

- Series C: 

    - C_descriptives: generates descriptive statistics: plots and maps

    - C_event_study: generates event study plots

    - C_regression: generates regression tables

- Run the code as in the main file:
    - A_concat_data.R
    - B_clean_data.R
    - C_descriptives.R
    - C_event_study.R
    - C_regression.R

## Other Observations

    - Don't forget to use renv::restore() before running the code
    
    - Don't forget to set the main working directory and the working directory to the folder where the data is stored

    - My VS code bugs when I try to run the whole code, so i have to go chunk by chunk

    - First script takes a while to run, and second one a bit less

    - The rest of the scripts are pretty quick

    - There is supposed to have a warning message on the C_descriptives, but it is not a problem