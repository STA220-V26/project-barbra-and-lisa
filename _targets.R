# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

library(targets)
library(tarchetypes)

# Load packages required to define the pipeline:
pkgs <- c(
  "janitor", # data cleaning
  "labelled", # labeling data
  "pointblank", # data validation and exploration
  "rvest", # get data from web pages
  "tidyverse", # Data management
  "data.table", # fast data management
  "fs", # to work wit hthe file system
  "zip", # manipulate zip files
  "qs2"
)

invisible(lapply(pkgs, library, character.only = TRUE))


# Set target options:
tar_option_set(
  packages = c("tibble"), # Packages that your targets need for their tasks.
  format = "qs" # Optionally set the default storage format. qs is fast.
)
# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# We first download the data health care data of interest
if (!fs::file_exists("data.zip")) {
  message("Downloading data.zip from GitHub")
  curl::curl_download(
    "https://github.com/STA220/cs/raw/refs/heads/main/data.zip",
    "data.zip",
    quiet = FALSE
  )
}

# Replace the target list below with your own:
list(
  #define the zip file path
  tar_target(zipdata, "data.zip", format = "file"),

  #unzip the file
  tar_target(cvs_files, zip::unzip(zipdata)),

  tar_target(
  patients_clean,
  {
    patients <- setDT(patients_raw)
    setkey(patients, id)

    patients <- janitor::remove_empty(patients, quiet = TRUE)
    patients <- janitor::remove_constant(patients, quiet = TRUE)

    patients
  }
),

  #dynamically read all files found in data-fixed
  #I removed the fs::dir_map line because it was causing the error
  tar_map(
   values = tibble::tibble(path = dir("data-fixed", full.names = TRUE)) |> #looks inside data-fixed and gets the full path for every file inside
     dplyr::mutate(name = tools::file_path_sans_ext(basename(path))), #creates a clean name column by stripping away the folder path and the file extension
   tar_target(dt, fread(path)), #for every row in the table above, tar_map will generate a target named dt
   names = name, #tells tar_map how to name the resulting targets in the pipeline
   descriptions = NULL
  ),
 
  
  
  
  tar_target(
    name = data, #the name which you reference to this command with
    command = tibble(x = rnorm(100), y = rnorm(100))
    # format = "qs" # Efficient storage for general data objects.
  ),
  tar_target(
    name = model,
    command = coefficients(lm(y ~ x, data = data))
  )
)
