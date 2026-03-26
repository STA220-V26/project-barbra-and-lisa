
#see the flowchart of what we have an updated target on
tar_visnetwork()

#rerun all unupdated targets
tar_make()


tar_load(analysis_dataset)

tar_read(combined_trend_plot)
