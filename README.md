# HourlyStep

The code for the paper, *Hourly step recommendations to achieve daily goals for working and older adults: evidence from a population-based intervention*, is available here.

There are two Stan files: 
  1. hurdle_gamma6_initial_demographics.stan is used for the first half of the day;
  2. hurdle_gamma6_boost_5000_7500_10000_demographics.stan is used for the second half of the day.

There are three R code files:
  1. 01_final_combined_script_demographics.R processes the data to be used as inputs to the models.
  2. 02_fit_demographics_model.R fits the data to the models.
  3. 03_process_demographics_model.R processes the results of the models.
