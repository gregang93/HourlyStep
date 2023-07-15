library(data.table)

setwd("D:\\Research2\\requestnum026\\Gregory\\Data")
df_demo = fread("hourly_data/3075_demographics_8Jan2018_31Mar2018.csv")

## Demographics are factors ##
df_demo2 = filter(df_demo, drid %in% final_df_long$drid)
df_demo2$gender = factor(df_demo2$gender)
df_demo2$bmi_gp = factor(df_demo2$bmi_gp,levels = c("18.5-<23","<18.5" ,"23-<27.5",">=27.5"))
df_demo2$age_gp3 = factor(df_demo2$age_gp3, levels = c("[17,30)","[30,40)","[40,50)","[50,60)", "[60,1e+04]"))


setwd("D:\\Research2\\requestnum026\\Gregory\\rstan\\Final_3075individuals_demographics")
start_extension=13
library(rstan)

scale_factor = 10000

# Should results be saved? 
save_results = F

# Different dates for Weekday/Weekend model
if (type_of_week == "Weekday") run_date = "2022-04-04"
if (type_of_week == "Weekend") run_date = "2022-03-08"

################
# Simple Model #
################

# Load all the parameters for the simple model into one dataframe
parameters_list = list()
for (time_of_day in 1:(24)){
  
  filename = paste(type_of_week,"/Simple/",run_date,"_initial_hurdle_gamma_3075_indivs_4_chains_hr_10000sample_", time_of_day,".rds", sep = "")
  
  m_hier = readRDS(filename)
  print(time_of_day)
  mcmc_hier2 = as.matrix(m_hier)
  parameters_list[[time_of_day]] = mcmc_hier2
  mcmc_hier2_means = apply(mcmc_hier2,2,mean)
  if (time_of_day == 1){
    simple_hurdle_df = cbind(time_of_day,monitor(m_hier, digits_summary = 3))
    parameters_mat = matrix(0,length(mcmc_hier2_means),24)
  } else{
    simple_hurdle_df = rbind(simple_hurdle_df,cbind(time_of_day,monitor(m_hier, digits_summary = 3)))
  }
  
  parameters_mat[,time_of_day] = mcmc_hier2_means
  rm(m_hier)
}


rownames_simple_df = names(mcmc_hier2_means)
rownames(parameters_mat) = rownames_simple_df
if (save_results) write.csv(simple_hurdle_df, file = paste("parameters_csv/3075_indivs_Simple_hurdle_model_",type_of_week,"_parameters_demographics.csv",sep = ""))

#################
# Complex Model #
#################

parameters_list_thres = list()
for (time_of_day in start_extension:24){
  filename = paste(type_of_week,"/Extension/",run_date,"_boost_cumsum_hurdle_gamma_3075_indivs_4_chains_hr_10000sample_", time_of_day,".rds", sep = "")
  m_hier = readRDS(filename)
  print(time_of_day)
  mcmc_hier2 = as.matrix(m_hier)
  parameters_list_thres[[time_of_day]] = mcmc_hier2
  mcmc_hier2_means = apply(mcmc_hier2,2,mean)
  if (time_of_day == start_extension){
    simple_hurdle_df = cbind(time_of_day,monitor(m_hier, digits_summary = 3))
    parameters_mat_thres = matrix(0,length(mcmc_hier2_means),24)
  } else{
    simple_hurdle_df = rbind(simple_hurdle_df,cbind(time_of_day,monitor(m_hier, digits_summary = 3)))
  }
  
  parameters_mat_thres[,time_of_day] = mcmc_hier2_means
  rm(m_hier)
}
rownames_thres_df = names(mcmc_hier2_means)
rownames(parameters_mat_thres) = rownames_thres_df
if (save_results) write.csv(simple_hurdle_df, file = paste("parameters_csv/3075_indivs_split_boost_hurdle_model_",type_of_week,"_parameters_demographics.csv",sep = ""))

# Convert factor variables into dummy variables (one-hot encoding)
X_demo = model.matrix(drid ~., data = df_demo2)
colnames(X_demo)

# Generate a 24-hour trajectory based on simple model
gen_person_trajectory_simple <- function(df, df2 = NULL, seed ){
  set.seed(seed)
  sample_size = dim(df[[1]])[1]
  result = numeric(25)
  ### 1 person is randomly chosen, but keep the same person for different hours####
  person_draw = sample(N_unique,1)
  X_demo_person = X_demo[person_draw,]
  
  # theta_index = which(grepl("theta",rownames_simple_df,fixed = T))
  alpha_index = which(grepl("alpha",rownames_simple_df,fixed = T))
  beta_index = which(grepl("beta",rownames_simple_df,fixed = T))
  prec_index = which(grepl("prec",rownames_simple_df,fixed = T))
  
  
  for (i in 1:24){
    posterior_draw = sample(1:sample_size,1)
    lin_pred_p0 = sum((df[[i]][posterior_draw,alpha_index])*X_demo_person )
    prob_0 = sigmoid(lin_pred_p0)
    if (runif(1) > prob_0){
      a = df[[i]][posterior_draw,prec_index]
      mu = exp(sum((df[[i]][posterior_draw,beta_index]) *X_demo_person ))
      b = a/(mu)
      result[i+1] = rgamma(1,a,b)
    }
  }
  return(result[2:25])
}

# Generate a 24-hour trajectory based on complex model
gen_person_trajectory_complex <- function(df, df2, seed, start_extension){
  set.seed(seed)
  sample_size = dim(df[[1]])[1]
  result = numeric(25)
  
  ### 1 person is randomly chosen, but keep the same person for different hours####
  person_draw = sample(N_unique,1)
  

  
  X_demo_person = X_demo[person_draw,]
  
  # theta_index = which(grepl("theta",rownames_simple_df,fixed = T))
  alpha_index = which(grepl("alpha",rownames_simple_df,fixed = T))
  beta_index = which(grepl("beta",rownames_simple_df,fixed = T))
  prec_index = which(grepl("prec",rownames_simple_df,fixed = T))
  
  for (i in 1:(start_extension-1)){
    posterior_draw = sample(1:sample_size,1)
    lin_pred_p = sum((df[[i]][posterior_draw,alpha_index])*X_demo_person )
    prob = sigmoid(lin_pred_p)
    if (runif(1) > prob){
      a = df[[i]][posterior_draw,prec_index]
      mu = exp(sum((df[[i]][posterior_draw,beta_index]) * X_demo_person))
      b = a/(mu)
      result[i+1] = rgamma(1,a,b)
    }
  }
  
  # theta_thres_index = which(grepl("theta",rownames_thres_df,fixed = T))
  alpha0_thres_index = which(grepl("alpha0",rownames_thres_df,fixed = T))
  alpha1_thres_index = which(grepl("alpha1",rownames_thres_df,fixed = T))
  alpha2_thres_index = which(grepl("alpha2",rownames_thres_df,fixed = T))
  alpha3_thres_index = which(grepl("alpha3",rownames_thres_df,fixed = T))
  
  beta0_thres_index = which(grepl("beta0",rownames_thres_df,fixed = T))
  beta1_thres_index = which(grepl("beta1",rownames_thres_df,fixed = T))
  beta2_thres_index = which(grepl("beta2",rownames_thres_df,fixed = T))
  beta3_thres_index = which(grepl("beta3",rownames_thres_df,fixed = T))
  
  prec_thres_index = which(grepl("prec",rownames_thres_df,fixed = T))
  
  for (i in start_extension:24){
    # Update the cumulative steps in 2nd entry, 1st entry is always intercept
    
    X_demo_person_complex = c(1, sum(result)/scale_factor, X_demo_person[-1])
    sample_size = nrow(df2[[i]])
    posterior_draw = sample(1:sample_size,1)
    
    if (sum(result)<5000){
      lin_pred_p0 = sum((df2[[i]][posterior_draw,alpha0_thres_index])*X_demo_person_complex )
      prob_0 = sigmoid(lin_pred_p0)
      if (runif(1) > prob_0){
        a = df2[[i]][posterior_draw,prec_thres_index[1]]
        #mu = exp(df2[[i]][posterior_draw,beta0_thres_index[1]] + df2[[i]][posterior_draw,beta0_thres_index[2]] * sum(result) + sum(df2[[i]][posterior_draw,beta0_thres_index[-(1:2)]] * X_demo_person[-1]))
        lin_pred_mu = sum((df2[[i]][posterior_draw,beta0_thres_index])*X_demo_person_complex)
        mu = exp(lin_pred_mu)
        b = a/(mu)
        result[i+1] = rgamma(1,a,b)
      }
    } else if (sum(result) < 7500){
      lin_pred_p1 = sum((df2[[i]][posterior_draw,alpha1_thres_index])*X_demo_person_complex )
      prob_1 = sigmoid(lin_pred_p1)
      if (runif(1) > prob_1){
        a = df2[[i]][posterior_draw,prec_thres_index[2]]
        lin_pred_mu = sum((df2[[i]][posterior_draw,beta1_thres_index])*X_demo_person_complex)
        mu = exp(lin_pred_mu)
        b = a/(mu)
        result[i+1] = rgamma(1,a,b)
      }
    } else if (sum(result) < 10000){
      lin_pred_p2 = sum((df2[[i]][posterior_draw,alpha2_thres_index])*X_demo_person_complex )
      prob_2 = sigmoid(lin_pred_p2)
      if (runif(1) > prob_2){
        a = df2[[i]][posterior_draw,prec_thres_index[3]]
        #mu = exp(df2[[i]][posterior_draw,beta2_thres_index[1]] + df2[[i]][posterior_draw,beta2_thres_index[2]] * sum(result) + sum(df2[[i]][posterior_draw,beta2_thres_index[-(1:2)]] * X_demo_person[-1]))
        lin_pred_mu = sum((df2[[i]][posterior_draw,beta2_thres_index])*X_demo_person_complex)
        mu = exp(lin_pred_mu)
        b = a/(mu)
        result[i+1] = rgamma(1,a,b)
      }
    } else{
      lin_pred_p3 = sum((df2[[i]][posterior_draw,alpha3_thres_index])*X_demo_person_complex )
      prob_3 = sigmoid(lin_pred_p3)
      if (runif(1) > prob_3){
        a = df2[[i]][posterior_draw,prec_thres_index[4]]
        # mu = exp(df2[[i]][posterior_draw,beta3_thres_index[1]] + df2[[i]][posterior_draw,beta3_thres_index[2]] * sum(result) + sum(df2[[i]][posterior_draw,beta3_thres_index[-(1:2)]] * X_demo_person[-1]))
        lin_pred_mu = sum((df2[[i]][posterior_draw,beta3_thres_index])*X_demo_person_complex)
        mu = exp(lin_pred_mu)
        b = a/(mu)
        result[i+1] = rgamma(1,a,b)
      }
    }
  }
  return(result[2:25])
}

# Wrapper function
gen_person_daily_steps_simple = function(df,df2,seed) return(cumsum(gen_person_trajectory_simple(df,df2,seed)))
gen_person_daily_steps_complex = function(df,df2,seed,start_extension) return(cumsum(gen_person_trajectory_complex(df,df2,seed,start_extension)))

daily_steps_y = apply(y,1,cumsum)

# Simulate trajectories Nd times
simulated_sample_daily_steps_simple <- sapply(1:Nd, function(x) gen_person_daily_steps_simple(parameters_list,parameters_list_thres,x))
simulated_sample_daily_steps_complex <- sapply(1:Nd, function(x) gen_person_daily_steps_complex(parameters_list,parameters_list_thres,x,start_extension = 13))

# Save the results
if (save_results){
  save_output = list()
  save_output[[1]] = daily_steps_y
  save_output[[2]] = simulated_sample_daily_steps_simple
  save_output[[3]] = simulated_sample_daily_steps_complex
  saveRDS(save_output, file = paste("rds/",type_of_week,"_simulated_",N_unique,"_individuals_10000sample_simple_complex_demographics.rds" , sep = ""))
}

daily_steps_y = save_output[[1]]
simulated_sample_daily_steps_simple=save_output[[2]] 
simulated_sample_daily_steps_complex=save_output[[3]] 
