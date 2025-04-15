
load(file = paste0(wd,"Input/df_week.rda"))

##### Run Regressions #####

df_week <- df_week %>%
  mutate(time_id = paste0(year,"_",week_index))

# define covariates and fixed effects (introduced one at a time)
covars <- c("l(police_killing_100k)", "l(police_killing_100k,2)")
fe_list <- c("",
             "place_id",             
             "place_id + year",     
             "place_id + year + month(week_start)") 


for (dep_var in c("retaliation_index_100k","retaliation_index_2_100k")){
  
  var_short <- if (dep_var == "retaliation_index_100k") "ri_1" else "ri_2"
  
  models <- list()  
  idx <- 1            
  
  for (fe_spec in fe_list) {
    
    for (i in seq_along(covars)) {
      current_covars <- covars[1:i]
      
      # define the formula
      rhs_covars <- paste(current_covars, collapse = " + ")
      
      # if FE is not empty, append "| fe_spec" 
      if (fe_spec != "") {
        rhs_full <- paste(rhs_covars, "|", fe_spec)
      } else {
        rhs_full <- rhs_covars  # no FE
      }
      
      formula_str <- formula(paste(dep_var, "~", rhs_full))
      
      # fit the model (cluster by place_id)
      suppressMessages(
        reg <- feols(
          formula_str,
          data = df_week,
          cluster = ~place_id,
          panel.id = c("place_id","time_id")
        )
      )
      
      models[[idx]] <- reg
      idx <- idx + 1
    }
    
    
  }
  
  # save the table
  mean_dv <- df_week %>%
    filter(week(week_start) > 1) %>%
    summarise(mean = mean(!!sym(dep_var),na.rm=T)) %>%
    pull
  
  for (lag in c("","_lag")) {
    
    regs <- if (lag == "") models else models[c(2,4,6,8)]
    
    
    t <- etable(
      regs,
      dict = c(
        "police_killing_100k" = "Trigger",
        "l(police_killing_100k,1)" = "Trigger (w-1)",
        "l(police_killing_100k,2)" = "Trigger (w-2)",
        "retaliation_index_100k" = "Retaliation",
        "retaliation_index_2_100k" = "Retaliation (Alternative)",
        "place_id" = "Precinct",
        "month(week_start)" = "Month",
        "year" = "Year"
      ),
      drop = "(Constant)", # remove the constant row
      extralines = list(
        "Mean of DV" = rep(round(mean_dv, 3), length(regs)) #,# one entry per model
        #"Cluster SE" = rep("Precinct",4)
      ),
      tex = TRUE,
      style.tex = style.tex("aer"),
      digits = 2,
      digits.stats = 2 ,
      fitstat = c("n", "ar2", "awr2")
    )
    
    cat(t, file = paste0(wd,"Output/reg_",var_short,lag,".tex")) 
  }
  
}

rm(list = ls())
