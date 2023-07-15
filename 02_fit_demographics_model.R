setwd("D:\\Research2\\requestnum026\\Gregory\\rstan")
library(rstan)

######################################### Fit the model ######################################

####################
### Simple Model ###
####################
set.seed(1234)

today_date = Sys.Date()
n_chains = 4
nsample = 10000
for (time_of_day in 1:(24)){
  y_time_of_day = final_df_long %>% filter(hour == time_of_day) %>% select(y)
  X = final_df_long %>% filter(hour == time_of_day) %>% select(intercept, age_gp3, bmi_gp, gender)
  X = model.matrix(intercept ~., data= X)
  Nd = nrow(X)
  K = ncol(X)
  
  m_hier<-stan(file="Final_3075individuals_demographics/stan_files/hurdle_gamma6_initial_demographics.stan",data=list(Nd =Nd , K = K, y = y_time_of_day[,1], X = X), chains = n_chains, cores = n_chains,init = 1234,iter = nsample)
  filename = paste("Final_3075individuals_demographics/",type_of_week,"/Simple/",today_date,"_initial_hurdle_gamma_3075_indivs_4_chains_hr_",nsample,"sample_", time_of_day,".rds", sep = "")
  saveRDS(m_hier, file = filename)
  print(time_of_day)
}

########################
#### Complex Model #####
########################
set.seed(1234) #1st try

today_date = Sys.Date()
n_chains = 4
nsample = 10000
for (time_of_day in start_extension:24){
  y_time_of_day = final_df_long %>% filter(hour == time_of_day) %>% select(y)
  cumsum_lag_v = final_df_long %>% filter(hour == time_of_day) %>% select(cum_sum_lag)
  X = final_df_long %>% filter(hour == time_of_day) %>% select(intercept, cum_sum_lag_transform, age_gp3, bmi_gp, gender)
  X = model.matrix(intercept ~., data= X)
  Nd = nrow(X)
  K = ncol(X)

  m_hier<-stan(file="Final_3075individuals_demographics/stan_files/hurdle_gamma6_boost_5000_7500_10000_demographics.stan",data=list(Nd =Nd , y=y_time_of_day[,1],cumsum_lag = cumsum_lag_v[,1], K = K, X = X), chains = n_chains, cores = n_chains,init = 1234,iter = nsample)
  filename = paste("Final_3075individuals_demographics/",type_of_week,"/Extension/",today_date,"_boost_cumsum_hurdle_gamma_3075_indivs_4_chains_hr_",nsample,"sample_", time_of_day,".rds", sep = "")
  
  saveRDS(m_hier, file = filename)
  cat(analysis_type,time_of_day, "\n")
}

