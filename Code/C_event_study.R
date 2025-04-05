
load(file = paste0(wd,"Input/df_7.rda"))
load(file = paste0(wd,"Input/df_14.rda"))

##### Prepare event study function #####

run_es <- function(df,mode = c("avg","lm","lm_fe"),n_days){
  
  if (mode == "avg") {
    df_event %>%
      group_by(relative_day) %>%
      summarise(
        coef = mean(retaliation_index, na.rm = TRUE),
        sd_crime   = sd(retaliation_index, na.rm = TRUE),
        n          = n(),
        ci_low = coef - 1.96*sd_crime,
        ci_high = coef + 1.96*sd_crime
      )
  }else{
    if(mode == "lm"){
      reg <- lm(retaliation_index ~ factor(relative_day) - 1,df_event)
    }else{
      reg <- felm(
        crime_count ~ factor(relative_day) | place_id + month, 
        data = df_event
      )
    }
    # clustering se
    reg_test <- coeftest(reg, vcov = vcovHC(reg,"HC0",cluster = "place_id"))
    
    # standardize in relation to the last period before the treatment
    reg_test[, "Estimate"] <- reg_test[,"Estimate"] - reg_test[n_days,"Estimate"]
    
    es <- tibble(relative_day = -n_days:n_days,
                 coef = reg_test[1:(n_days*2 + 1),"Estimate"],
                 se = reg_test[1:(n_days*2 + 1),"Std. Error"])
    
    es <- es %>%
      mutate(ci_low = coef - 1.96*se,
             ci_high = coef + 1.96*se)
  }
  
  return(es)
  
}

##### Prepare plot function #####
plot_es <- function(df,plot_name){
  
  pd <- position_dodge(0.1)
  
  p <- ggplot(df, aes(x = relative_day, y = coef)) + 
    geom_errorbar(aes(ymin=ci_low, ymax=ci_high),  colour= "#1e81b0",width=.1, position=pd) +
    geom_point(position=pd, colour= "#76b5c5", size = 1.5) +
    geom_hline(yintercept=0, linetype="dashed", color = "red") +
    geom_vline(xintercept=c(-1), linetype="solid", color = "red",
               alpha = 0.4) +
    theme_bw() + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text = element_text(face = "bold")) + 
    scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) +
    labs(y = "Coefficient", x= "Time to Treat (Treatment = 0)")
  
  p
  
  ggsave(p,path = paste0(wd,"/Output/",plot_name,".pdf"))
}


##### Run ES #####

for (day in c(7,14)) {
  temp <- 
  for (mode in c("avg","lm","lm_fe")) {
    temp <- run_es()
  }
}



