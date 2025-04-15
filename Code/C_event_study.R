##### Prepare event study function #####

run_es <- function(df,dep_var,mode = c("avg","lm","lm_fe_1","lm_fe_2"),n_days){
  
  v_sym <- sym(dep_var)
  
  if (mode == "avg") { #computing this way is redundant with running a linear regression, i did it just to check if everything was correct
    es <- df %>%
      group_by(relative_day) %>%
      summarise(
        coef = mean(!!v_sym, na.rm = TRUE),
        sd_crime   = sd(!!v_sym, na.rm = TRUE),
        n          = n()) %>%
      ungroup() %>%
      mutate(coef = coef - coef[n_days],
        ci_low = coef - 1.96*(sd_crime / sqrt(n)),
        ci_high = coef + 1.96*(sd_crime / sqrt(n))
      ) 
  }else{
    
    if(mode == "lm"){
      formula <- as.formula(paste0(dep_var," ~ factor(relative_day) - 1"))
      reg <- lm(formula,df)
      
      # clustering se
      reg_test <- coeftest(reg, vcov = vcovHC(reg,"HC0",cluster = "place_id"))
      reg_test <- reg_test[1:(n_days*2 + 1),]
      
      # standardize in relation to the last period before the treatment
      reg_test[, "Estimate"] <- reg_test[,"Estimate"] - reg_test[n_days,"Estimate"]
      
      es <- tibble(relative_day = -n_days:n_days,
                   coef = reg_test[,"Estimate"],
                   se = reg_test[,"Std. Error"])
      
    }
      
      if(mode == "lm_fe_1") {
        formula <- as.formula(paste0(dep_var," ~ i(relative_day, ref = -1) | place_id"))
        reg <- feols(
          formula,
          cluster = ~place_id,
          data = df
        )
        
        es <- coeftable(reg) %>%
          as_tibble()
        
        # partitioning es to include time -1 (reference period)
        es_1 <- es[1:(n_days - 1),]
        es_2 <- es[n_days:(n_days*2),]
        
        es <- es_1 %>%
          rbind(c(0,0,0,0)) %>%
          rbind(es_2)
        
        es$relative_day <-  c(-n_days:n_days)
        es <- es %>%
          select(relative_day,coef = Estimate, se = `Std. Error`) %>%
          mutate(coef = coef - coef[n_days])
      }
      if(mode == "lm_fe_2") {
        formula <- as.formula(paste0(dep_var," ~ i(relative_day, ref = -1)| place_id + month"))
        reg <- feols(
          formula,
          cluster = ~place_id,
          data = df
        )
        
        es <- coeftable(reg) %>%
          as_tibble()
        
        # partitioning es to include time -1 (reference period)
        es_1 <- es[1:(n_days - 1),]
        es_2 <- es[n_days:(n_days*2),]
        
        es <- es_1 %>%
          rbind(c(0,0,0,0)) %>%
          rbind(es_2)
        
        es$relative_day <-  c(-n_days:n_days)
        es <- es %>%
          select(relative_day,coef = Estimate, se = `Std. Error`) %>%
          mutate(coef = coef - coef[n_days])
      }
    
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
  
  suppressMessages(ggsave(plot = p,filename = paste0(wd,"/Output/",plot_name,".pdf")))
}


##### Run ES #####

load(file = paste0(wd,"Input/df_7.rda"))
load(file = paste0(wd,"Input/df_14.rda"))

# defining the looping vectors
days <- c(7, 14)
deps <- c("retaliation_index_100k", "retaliation_index_2_100k")
modes <- c("avg", "lm", "lm_fe_1", "lm_fe_2")

# total number of iterations
total_iterations <- length(days) * length(deps) * length(modes)

i <- 1
# Loop
for (day in days) {
  temp <- if (day == 7) df_7 else df_14
  
  for (dep in deps) {
    for (mode in modes) {
      
      temp_es <- run_es(temp, dep, mode, day)
      dep_short <- if (dep == "retaliation_index_100k") "ri_1" else "ri_2"
      
      plot_name <- paste0("es_", day, "_", dep_short, "_", mode)
      plot_es(temp_es, plot_name)
      
      # progress print
      message(sprintf("Iteration %d/%d: day = %d | dep = %s | mode = %s", 
                      i, total_iterations, day, dep, mode))
      i <- i + 1
    }
  }
}



##### Checking how much overlap we have in the event study #####

check_7 <- df_7 %>%
  mutate(n = 1) %>%
  group_by(place_id,date,event) %>%
  summarise(n = sum(n))  %>%
  ungroup()

check_14 <- df_14 %>%
  mutate(n = 1) %>%
  group_by(place_id,date,event) %>%
  summarise(n = sum(n)) %>%
  ungroup()

for (days in c(7,14)) {
  if (days == 7) {
    temp <- check_7
  }else{
    temp <- check_14
  }
  for (num in 1:9) {
    a <- temp %>%
      filter(n > num) %>%
      summarise(n = sum(n)) %>%
      pull
    
    b <- temp %>%
      filter(n > num) %>%
      summarise(events = sum(event)) %>%
      pull
    
    print(paste0(days,"-day window: ",a," precinct-date observations and ",b," events that overlap ",num," time(s)"))
  }
}


# remove everything that is not the wd paths
rm(list=setdiff(ls(), c("wd","wd_data")))

