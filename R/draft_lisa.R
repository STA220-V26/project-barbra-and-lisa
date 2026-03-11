library(ggplot2)
library(caTools)
library(dplyr)


patients <-
  readr::read_csv(unz("data.zip", "data-fixed/patients.csv")) |>
  setDT() |>
  setkey(id)

patients <- patients %>%
  mutate(age = floor(interval(birthdate, today()) / years(1)))

plot(patients$age, patients$healthcare_expenses)



hist(patients$gender, patients$healthcare_expenses)
