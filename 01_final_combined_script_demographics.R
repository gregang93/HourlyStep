### Either Weekday or Weekend
# type_of_week = "Weekend"
# type_of_week = "Weekday"

library(latex2exp)
library(data.table)
library(tidyverse)
library(rstan)
library(stats)
library(e1071)

# Set Working Directory #
setwd("D:\\Research2\\requestnum026\\Gregory\\Data")


# Load data #
df_demo = fread("hourly_data/3075_demographics_8Jan2018_31Mar2018.csv")
person_df = fread("hourly_data/1hr_steps_3075individuals_8Jan2018_31Mar2018.csv")


#Weekday/Weekend
if (type_of_week == "Weekday"){
  week_df = person_df %>% filter(week_no >= 10, day_of_week<= 5)
} else {
  week_df = person_df %>% filter(week_no >= 10, day_of_week> 5)
}
N_unique = length(unique(week_df$drid))


#### Reshape to compute y, ylag, cumulative sum of y, cumulative sum of y 
y = week_df %>% dplyr::select(-drid,-date_index,-day_of_week,-week_no)
Nd = nrow(y)
y_lag = cbind(numeric(Nd),y[,-24])
colnames(y_lag) = paste(0:23)
cum_sum_lag = t(apply(y_lag,1,cumsum))
cum_sum = t(apply(y,1,cumsum))

## Processed Data ##
## Long format ##
final_df_long = gather(week_df, hour, y,-drid,-date_index,-day_of_week,-week_no)
y_lag_long = gather(y_lag, hour, y_lag)
cum_sum_lag_long = gather(as.data.frame(cum_sum_lag), hour,cum_sum_lag)
cum_sum_long = gather(as.data.frame(cum_sum), hour,cum_sum)
final_df_long = data.frame(final_df_long,y_lag = y_lag_long$y_lag,cum_sum_lag = cum_sum_lag_long$cum_sum_lag,cum_sum = cum_sum_long$cum_sum)
final_df_long$hour = as.integer(final_df_long$hour)


final_df_long = final_df_long %>% left_join(df_demo, by = c("drid"))
final_df_long$intercept = 1
final_df_long$hour = final_df_long$hour + 1



# Change to factors

final_df_long$gender = factor(final_df_long$gender)
final_df_long$bmi_gp = factor(final_df_long$bmi_gp,levels = c("18.5-<23","<18.5" ,"23-<27.5",">=27.5"))
final_df_long$age_gp3 = factor(final_df_long$age_gp3, levels = c("[17,30)","[30,40)","[40,50)","[50,60)", "[60,1e+04]"))



if (type_of_week == "Weekday"){
  start_extension = 13 # Originallly 13
} else {
  start_extension = 13 # Originallly 13
}



# Scale down by 10,000
{
  
  scale_factor = 10000
  ## For the predictor, apply log(x+1) transformation
  #final_df_long$cum_sum_lag_transform = log(final_df_long$cum_sum_lag + 1)
  final_df_long$cum_sum_lag_transform = final_df_long$cum_sum_lag/scale_factor
  
}

## Prepare data for fitting ##
## Change demographics to factors ##
df_demo2 = filter(df_demo, drid %in% final_df_long$drid)
df_demo2$gender = factor(df_demo2$gender)

df_demo2$bmi_gp = factor(df_demo2$bmi_gp,levels = c("18.5-<23","<18.5" ,"23-<27.5",">=27.5"))
df_demo2$age_gp3 = factor(df_demo2$age_gp3, levels = c("[17,30)","[30,40)","[40,50)","[50,60)", "[60,1e+04]"))


table(df_demo2$bmi_gp)
table(df_demo2$age_gp3)

#### Stan takes up a lot of memory. Remove everything that is not needed.
rm(y_lag,week_df,person_df)
rm(cum_sum,cum_sum_lag_long,cum_sum_long, y_lag_long)
#rm(df_demo,df3_stata, df4_stata_one_obs)
rm(df_demo)

rm(cum_sum_lag,df_demo2)
#write.csv(df_demo2, file = paste(N_unique,"_indivs_demographics_",type_of_week,".csv",sep =""), row.names = F)