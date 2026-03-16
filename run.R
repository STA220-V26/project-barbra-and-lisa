
#see the flowchart of what we have an updated target on
tar_visnetwork()

#rerun all unupdated targets
tar_make()


#library(corrr)

#analysis_dataset %>%
#  select(where(is.numeric)) %>%
#  correlate() %>%
#  shave() %>% 
#  fashion()  


#get the dataset
tar_load(analysis_dataset)

#get the regression results
summary(regression_results)

#get the histogram
tar_read(expense_age_plot)

#get the scatterplot
tar_read(combined_trend_plot)

#summary table
tar_read(pretty_summary)
