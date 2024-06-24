# Load packages required to define the pipeline:
library(targets)


# Set target options:
tar_option_set(
  packages = c("dplyr", "janitor") 
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Target list:
list(
# Data formatting
  tar_target(
    icb_pop,
    ICB_population_data_2022("Data/ICB Population estimates 2022.csv")
  ),
  tar_target(
    england_pop,
    England_population_data("Data/England population estimates 2017-2023.csv")
  ),
  tar_target(
    paed_fractures,
    formatting_sus_data("Z:/Strategic Analytics/Projects 2024/Paediatric fracture management/peads_fractures.csv")
  ),
  tar_target(
    epidemiology_agegroups,
    formatting_for_epidemiology_agegroups(paed_fractures, 
                                          england_pop)
  )
  ,
  tar_target(
    epidemiology_icb,
    formatting_for_epidemiology_icb(paed_fractures,
                                    "Data/old_ccg_codes_to_new.csv",
                                    icb_pop,
                                    "Data/icb_codes.csv")
  )
  

  
  
)
