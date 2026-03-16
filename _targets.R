library(targets)
library(tarchetypes)
# Load packages required to define the pipeline:
pkgs <- c(
  "janitor",
  "labelled",
  "pointblank",
  "rvest",
  "tidyverse",
  "data.table",
  "fs",
  "zip",
  "qs2",
  "lubridate",
  "curl",
  "forcats"
)

invisible(lapply(pkgs, library, character.only = TRUE))

# Set target options:
tar_option_set(
  packages = c(
    "janitor",
    "tidyverse",
    "data.table",
    "lubridate",
    "forcats"
  ),
  format = "qs"
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Download the data if it does not exist
if (!fs::file_exists("data.zip")) {
  message("Downloading data.zip from GitHub")
  curl::curl_download(
    "https://github.com/STA220/cs/raw/refs/heads/main/data.zip",
    "data.zip",
    quiet = FALSE
  )
}

# The target list
list(
  # File target
  tar_target(
    zipdata,
    "data.zip",
    format = "file"
  ),

  # Unzip data
  tar_target(
    csv_files,
    zip::unzip(zipdata, exdir = "data-fixed"),
    format = "file"
  ),

  # Read patients data
  tar_target(
    patients_raw,
    fread("data-fixed/patients.csv")
  ),

  # Read encounters data
  tar_target(
    encounters_raw,
    fread("data-fixed/encounters.csv")
  ),

  # Clean patients data
  tar_target(
    patients_clean,
    {
      patients <- copy(patients_raw)

      keep_vars <- c(
        "id",
        "birthdate",
        "deathdate",
        "marital",
        "race",
        "ethnicity",
        "gender",
        "healthcare_expenses",
        "healthcare_coverage",
        "income"
      )

      patients <- patients[, ..keep_vars]

      # Remove empty rows and columns
      patients <- janitor::remove_empty(
        patients,
        which = c("rows", "cols"),
        quiet = TRUE
      )

      # Convert dates
      patients[, birthdate := as.IDate(birthdate)]
      patients[, deathdate := as.IDate(deathdate)]

      # Standardize text variables
      patients[, marital := trimws(marital)]
      patients[, race := trimws(tolower(race))]
      patients[, ethnicity := trimws(tolower(ethnicity))]
      patients[, gender := trimws(toupper(gender))]

      # Recode marital status
      patients[
        ,
        marital := factor(
          marital,
          levels = c("S", "M", "D", "W"),
          labels = c("Single", "Married", "Divorced", "Widowed")
        )
      ]

      # Convert categorical variables to factors
      patients[, gender := factor(gender, levels = c("F", "M"))]
      patients[, race := as.factor(race)]
      patients[, ethnicity := as.factor(ethnicity)]

      # Lump rare race categories
      patients[, race := forcats::fct_lump_prop(race, prop = 0.05)]

      # Clean impossible numeric values
      patients[healthcare_expenses < 0, healthcare_expenses := NA_real_]
      patients[healthcare_coverage < 0, healthcare_coverage := NA_real_]
      patients[income < 0, income := NA_real_]

      # Remove rows with missing patient ID
      patients <- patients[!is.na(id)]

      # Remove duplicate IDs
      patients <- unique(patients, by = "id")

      # Set key
      setkey(patients, id)

      patients
    }
  ),

  # Clean encounters data
  tar_target(
    encounters_clean,
    {
      encounters <- copy(encounters_raw)

      keep_vars <- c(
        "id",
        "start",
        "stop",
        "patient",
        "encounterclass",
        "description",
        "base_encounter_cost",
        "total_claim_cost",
        "payer_coverage",
        "reasondescription"
      )

      encounters <- encounters[, ..keep_vars]

      # Remove empty rows and columns
      encounters <- janitor::remove_empty(
        encounters,
        which = c("rows", "cols"),
        quiet = TRUE
      )

      # Convert date-time columns
      encounters[, start := as.POSIXct(start, tz = "UTC")]
      encounters[, stop := as.POSIXct(stop, tz = "UTC")]

      # Standardize text columns
      encounters[, encounterclass := trimws(tolower(encounterclass))]
      encounters[, description := trimws(description)]
      encounters[, reasondescription := trimws(reasondescription)]

      # Clean impossible numeric values
      encounters[base_encounter_cost < 0, base_encounter_cost := NA_real_]
      encounters[total_claim_cost < 0, total_claim_cost := NA_real_]
      encounters[payer_coverage < 0, payer_coverage := NA_real_]

      # Remove rows with missing patient ID
      encounters <- encounters[!is.na(patient)]

      # Remove duplicate encounter IDs
      encounters <- unique(encounters, by = "id")

      # Set key
      setkey(encounters, patient)

      encounters
    }
  ),

  # Recalculate age from birthdate
  tar_target(
    patients_age,
    {
      patients <- copy(patients_clean)
      reference_date <- as.IDate("2025-12-31")

      patients[
        ,
        age := floor(
          lubridate::interval(birthdate, reference_date) /
            lubridate::years(1)
        )
      ]

      # Replace impossible ages with NA
      patients[age < 0 | age > 120, age := NA_real_]

      patients
    }
  ),

  # Create age groups
  tar_target(
    patients_age_groups,
    {
      patients <- copy(patients_age)

      patients[
        ,
        age_group := fifelse(
          age <= 18, "0-18",
          fifelse(
            age <= 35, "19-35",
            fifelse(
              age <= 50, "36-50",
              fifelse(age <= 65, "51-65", "66+")
            )
          )
        )
      ]

      patients[is.na(age), age_group := NA_character_]

      patients[
        ,
        age_group := factor(
          age_group,
          levels = c("0-18", "19-35", "36-50", "51-65", "66+")
        )
      ]

      patients
    }
  ),

  # Count encounters per patient
  tar_target(
    encounter_counts,
    {
      encounters <- copy(encounters_clean)

      encounter_counts <- encounters[
        ,
        .(n_encounters = .N),
        by = patient
      ]

      setkey(encounter_counts, patient)

      encounter_counts
    }
  ),

  # Join patient data with encounter counts
  tar_target(
    analysis_dataset,
    {
      patients <- copy(patients_age_groups)
      counts <- copy(encounter_counts)

      analysis_dataset <- merge(
        patients,
        counts,
        by.x = "id",
        by.y = "patient",
        all.x = TRUE
      )

      # Patients with no encounters get 0
      analysis_dataset[is.na(n_encounters), n_encounters := 0L]

      setDT(analysis_dataset)
      setkey(analysis_dataset, id)

      setcolorder(
        analysis_dataset,
        c(
          "id",
          "birthdate",
          "deathdate",
          "age",
          "age_group",
          "gender",
          "marital",
          "race",
          "ethnicity",
          "income",
          "healthcare_expenses",
          "healthcare_coverage",
          "n_encounters"
        )
      )

      analysis_dataset
    }
  )
)