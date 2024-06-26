# Load packages required to define the pipeline:
library(targets)


# Set target options:
tar_option_set(
  packages = c("dplyr", "janitor", "sf", "flextable") 
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
  ),
  tar_target(
    icb_shapefile,
    load_icb_shapfile("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Integrated_Care_Boards_April_2023_EN_BFC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
  ),
  
  # Maps
  tar_target(
    incidence_rate_map,
    total_incidence_rate_map(icb_shapefile,
                             epidemiology_icb)
  ),
  
  tar_target(
    elbow_incidence_rate_icb,
    incidence_maps_by_fracture_type( epidemiology_icb,
                                    "Elbow", 
                                    "Elbow fracture incidence rate")
  ),
  tar_target(
    clavicle_incidence_rate_icb,
    incidence_maps_by_fracture_type( epidemiology_icb,
                                     "Clavicle", 
                                    "Clavicle fracture incidence rate")
  ),
  tar_target(
    forearm_incidence_rate_icb,
    incidence_maps_by_fracture_type( epidemiology_icb,
                                     "Forearm", 
                                    "Forearm fracture incidence rate")
  ),
  tar_target(
    tibfib_incidence_rate_icb,
    incidence_maps_by_fracture_type( epidemiology_icb,
                                     "Tibia/Fibula", 
                                    "Tibia/Fibula fracture incidence rate")
  ),
  tar_target(
    toe_incidence_rate_icb,
    incidence_maps_by_fracture_type( epidemiology_icb,
                                     "Toe", 
                                    "Toe fracture incidence rate")
  ),
  
  
  #Tables
  
  tar_target(
    table_total_incidence_rate,
    table_of_total_incidence_rate(epidemiology_agegroups)
  ),
  tar_target(
    table_icb_incidence_rate,
    table_of_icb_incidence_rate(epidemiology_icb)
  ),
  tar_target(
    table_most_common_fractures,
    table_of_most_common_fractures(paed_fractures)
  )
  
  
  
  
  
  
  
  
  
  
  
)
