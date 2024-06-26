library(tidyverse)
library(janitor)

#ICB Population estimates 2022

ICB_population_data_2022<- function(filename) {
  
  data <- read.csv("Data/ICB Population estimates 2022.csv") |>
  clean_names()|>
  group_by(sicbl_2023_name, icb_2023_name)|>
  mutate(male0_4=sum(m0+m1+m2+m3+m4))|>
  mutate(male5_10=sum(m5+m6+m7+m8+m9+m10))|>
  mutate(male11_16=sum(m11+m12+m13+m14+m15+m16))|>
  mutate(female0_4=sum(f0+f1+f2+f3+f4))|>
  mutate(female5_10=sum(f5+f6+f7+f8+f9+f10))|>
  mutate(female11_16=sum(f11+f12+f13+f14+f15+f16))|>
  select(sicbl_2023_name,sicbl_2023_code, icb_2023_name, icb_2023_code,male0_4,male5_10,male11_16,
         female0_4, female5_10, female11_16 )|>
  group_by(sicbl_2023_name, icb_2023_name)|>
summarise(across(where(is.numeric), sum, na.rm=T))|>
    gather(key=group, value=pop_count, -c(sicbl_2023_name, icb_2023_name))|>
    mutate(sicb_code= stringr::str_extract(sicbl_2023_name, '\\b\\w+$'))|>
    group_by(sicb_code, icb_2023_name)|>
    summarise(pop_count=sum(pop_count))
  
    
  
  return(data)
}
  
#England population estimates 2018-2023 (using 2022 data for 2023)

England_population_data<- function(filename) {

  data <- read.csv(filename) |>
  clean_names()|>
  group_by(year)|>
  mutate(male0_4=sum(m0+m1+m2+m3+m4))|>
  mutate(male5_10=sum(m5+m6+m7+m8+m9+m10))|>
  mutate(male11_16=sum(m11+m12+m13+m14+m15+m16))|>
  mutate(female0_4=sum(f0+f1+f2+f3+f4))|>
  mutate(female5_10=sum(f5+f6+f7+f8+f9+f10))|>
  mutate(female11_16=sum(f11+f12+f13+f14+f15+f16))|>
  select(year, male0_4,male5_10,male11_16,
         female0_4, female5_10, female11_16 )|>
    gather(key=group, value=pop_count, !year)
  
  return(data)
}

# Initial formatting of SUS data

formatting_sus_data<- function(filename) {
  
    data <- read.csv(filename) |>
    clean_names()|>
      mutate(type=ifelse(type=="Wrist", "Forearm", type))|>
  mutate(der_activity_month= paste0(der_activity_month, "01")) |>
  mutate(der_activity_month= as.Date(der_activity_month, format="%Y%m%d")) |> #Format date
  mutate(age_sex_groups=case_when(der_age_at_cds_activity_date<=4 & sex==1 ~ "male0_4",
                                  der_age_at_cds_activity_date>=5 & der_age_at_cds_activity_date<=10 & sex==1 ~ "male5_10",
                                  der_age_at_cds_activity_date>=11 & sex==1 ~ "male11_16",
                                  der_age_at_cds_activity_date<=4 & sex==2 ~ "female0_4",
                                  der_age_at_cds_activity_date>=5 & der_age_at_cds_activity_date<=10 & sex==2 ~ "female5_10",
                                  der_age_at_cds_activity_date>=11 & sex==2 ~ "female11_16",
  )) |> #Add age/sex groupings
     mutate(group_labels=case_when(age_sex_groups == "male0_4" ~"Male 0-4 yrs",
                                     age_sex_groups== "male5_10" ~"Male 5-10 yrs",
                                     age_sex_groups== "male11_16"~"Male 11-16 yrs",
                                     age_sex_groups== "female0_4"~"Female 0-4 yrs",
                                     age_sex_groups== "female5_10"~"Female 5-10 yrs",
                                  age_sex_groups== "female11_16"~"Female 11-16 yrs",
      )) |>
      mutate(mua_in_theatre=ifelse(der_primary_procedure_code=="NULL", 0, 1))
    

 return(data)
}


# Epidemiology by age/sex groups over time for England

formatting_for_epidemiology_agegroups<- function(paed_fractures, england_pop) {
  
  data<-  paed_fractures|> 
  filter(sex==1 | sex==2)|> #remove records without sex recorded
  filter(der_financial_year!="2017/18")|> #remove as incomplete yr
  mutate(year=as.numeric(stringr::str_extract(der_financial_year, "^.{4}")))|>
  summarise(frac_no=n(), .by=c(type, year, der_financial_year, age_sex_groups,group_labels))|>
  left_join(england_pop, by=c("year", "age_sex_groups"="group"))|>
  mutate(incidence=(frac_no/pop_count)*100000)
  
  return(data)
}

# Epidemiology by ICB

formatting_for_epidemiology_icb<- function(fractures, old_ccg_codes, population, icb_codes) {
  
  old_ccg_codes <- read.csv(old_ccg_codes)
  icb_codes <- read.csv(icb_codes)
  
  data<-fractures|> 
    filter(der_financial_year=="2023/24")|>
    summarise(frac_no=n(), .by=c(type, der_commissioner_code))|>
    left_join(old_ccg_codes[,c("ccg","icb22cdh")], by=c("der_commissioner_code"="ccg"))|>
    full_join(population, 
              by=c("der_commissioner_code"="sicb_code"))|>
    mutate(der_commissioner_code=ifelse(!is.na(icb_2023_name),icb22cdh, der_commissioner_code))|>
    left_join(icb_codes[,c("ICB23CDH", "ICB23NM")], by=c("der_commissioner_code"="ICB23CDH"))|>
    mutate(icb_2023_name=ifelse(is.na(icb_2023_name),ICB23NM, icb_2023_name))|>
    group_by(icb_2023_name ,type)|>
    summarise(frac_no=sum(frac_no), pop_count=sum(pop_count, na.rm=TRUE))|>
    mutate(incidence=(frac_no/pop_count)*100000)
  
  return(data)
}

# Load ICB shapefile

load_icb_shapfile<- function(file) {

  data<-st_read(file)
  
  return(data)
}



