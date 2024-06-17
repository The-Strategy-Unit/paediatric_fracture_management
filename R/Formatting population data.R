library(tidyverse)
library(janitor)

#"ICB Population estimates 2022.csv"

ICB_population_data_2022<- function(filename) {
  
  data <- read.csv(filename) |>
  clean_names()|>
  group_by(sicbl_2023_name)|>
  mutate(male0_4=sum(m0+m1+m2+m3+m4))|>
  mutate(male5_10=sum(m5+m6+m7+m8+m9+m10))|>
  mutate(male11_16=sum(m11+m12+m13+m14+m15+m16))|>
  mutate(female0_4=sum(f0+f1+f2+f3+f4))|>
  mutate(female5_10=sum(f5+f6+f7+f8+f9+f10))|>
  mutate(female11_16=sum(f11+f12+f13+f14+f15+f16))|>
  select(sicbl_2023_name,sicbl_2023_code, icb_2023_name, icb_2023_code,male0_4,male5_10,male11_16,
         female0_4, female5_10, female11_16 )|>
  group_by(icb_2023_name)|>
summarise(across(where(is.numeric), sum, na.rm=T))
  
  return(data)
}
  
#"England population estimates 2018-2022.csv"
England_population_data<- function(filename) {

  data <- read.csv(filename) |>
  clean_names()|>
  mutate(male0_4=sum(m0+m1+m2+m3+m4))|>
  mutate(male5_10=sum(m5+m6+m7+m8+m9+m10))|>
  mutate(male11_16=sum(m11+m12+m13+m14+m15+m16))|>
  mutate(female0_4=sum(f0+f1+f2+f3+f4))|>
  mutate(female5_10=sum(f5+f6+f7+f8+f9+f10))|>
  mutate(female11_16=sum(f11+f12+f13+f14+f15+f16))|>
  select(male0_4,male5_10,male11_16,
         female0_4, female5_10, female11_16 )
  
  return(data)
}