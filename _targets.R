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

  tar_target(
    icb_pop,
    ICB_population_data_2022("Data/ICB Population estimates 2022.csv")
  ),
  tar_target(
    england_pop,
    England_population_data("Data/England population estimates 2018-2022.csv")
  )

  
  
)
