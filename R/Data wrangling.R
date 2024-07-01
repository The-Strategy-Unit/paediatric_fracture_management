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

  ethnicity_lookup<-read.csv("Data/ethnicity_lookup.csv")
  imd_lookup<-read.csv("Data/imd2019lsoa.csv")|>
    clean_names()
  
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
      mutate(mua_in_theatre=ifelse(der_primary_procedure_code=="NULL", 0, 1))|>
      mutate(mua=case_when(manipulation_in_ed=="1" ~ "Manipulation in ED", 
                           mua_in_theatre=="1"~ "Manipulation in theatre", 
                           TRUE~"0"))|>
      mutate(sex=case_when(sex=="1" ~ "Male",
                           sex=="2"~ "Female",
                           TRUE ~ "Missing/Unknown"))|>
      mutate(age=case_when(der_age_at_cds_activity_date<=4 ~ "0-4 yrs",
                           der_age_at_cds_activity_date>=5 & der_age_at_cds_activity_date<=10 ~ "5-10 yrs",
                           der_age_at_cds_activity_date>=11 ~ "11-16 yrs"))|>
      mutate(age=factor(age, levels=c("0-4 yrs", "5-10 yrs","11-16 yrs" )))|>
      mutate(day=case_when(day_of_week=="Sunday" | day_of_week=="Saturday"  ~ "Weekend",
                           day_of_week=="Monday" | day_of_week=="Tuesday"|day_of_week=="Wednesday" | day_of_week=="Thursday"|
                           day_of_week=="Friday" ~ "Weekday"))|>
      mutate(time=case_when(arrival_time>='07H 00M 0S' & arrival_time<='19H 00M 0S'  ~ " Daytime 7am to 7pm",
                            arrival_time>'19H 00M 0S' | arrival_time<'07H 00M 0S' ~ "Nighttime 7pm to 7am"))|>
      mutate(dept_type=case_when(ec_department_type==1 ~ "Major Emergency Dept",
                           ec_department_type==2 ~ "Mono-specialty Emergency Dept",
                           ec_department_type==3|ec_department_type==4 ~ "Urgent Treatment Centre/Walk in centre",
                           ec_department_type==5 ~ "Same Day Emergency Care"))|>
      mutate(dept_type=factor(dept_type, levels=c("Major Emergency Dept", "Urgent Treatment Centre/Walk in centre",
                                                  "Mono-specialty Emergency Dept","Same Day Emergency Care")))|>
      left_join(ethnicity_lookup[,c("Code", "ethnicity_broad")], by=c("ethnic_category"="Code"))|>
      mutate(ethnicity_broad=factor(ethnicity_broad, levels=c("Asian or Asian British", "Black or Black British","Mixed",
                                                              "Other Ethnic Groups", "White","Missing/Unknown")))|>
      left_join(imd_lookup[,c("lsoa_code_2011", "imd")], by=c("der_postcode_lsoa_2011_code"="lsoa_code_2011"))|>
      mutate(imd_quintiles=case_when(imd=="1"|imd=="2"~ "1",
                                     imd=="3"|imd=="4"~ "2",
                                     imd=="5"|imd=="6"~ "3",
                                     imd=="7"|imd=="8"~ "4",
                                     imd=="9"|imd=="10"~ "5"))|>
      mutate(imd_quintiles=ifelse(is.na(imd_quintiles), "Missing/Outside England", imd_quintiles))|>
      mutate(imd_quintiles=factor(imd_quintiles, levels=c("1","2","3","4", "5", "Missing/Outside England")))
    

 return(data)
}


# Epidemiology by age/sex groups over time for England

formatting_for_epidemiology_agegroups<- function(paed_fractures, england_pop) {
  
  data<-  paed_fractures|> 
  filter(sex=="Male" | sex=="Female")|> #remove records without sex recorded
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

# File with only those providers with more than 10 fracture attendances per month
removing_low_no_trusts<-function(data){

  trusts_with_120_attendances<-data|>
  filter(der_financial_year=="2022/23")|>
  group_by(der_provider_code)|>
  summarise(ed_attendances=n())|>
  filter(ed_attendances>=120)
  
  return(trusts_with_120_attendances)
  
}

# Calculating the number of follow ups by trust
calculating_f_up_by_trust<-function(data, trusts_included){
  
  provider_names<-read.csv("Data/provider_names.csv")
  
  f_up_by_trust<-data|>
    filter(der_financial_year=="2022/23")|>
    group_by(type, der_provider_code, outpat_attendance)|>
    summarise(count=n())|>
    group_by(type, der_provider_code)|>
    left_join(provider_names, by=c("der_provider_code"="code"))|>
    filter((der_provider_code %in% trusts_included$der_provider_code) & !is.na(name))|>
    group_by(der_provider_code, type)|>
    mutate(Percentage = round(count / sum(count)*100, 2), )|>
    filter(outpat_attendance=="1" )
  
  
}

# Calculating the number of manipulations by trust
calculating_manipulations_by_trust<-function(data, trusts_included){
  
  provider_names<-read.csv("Data/provider_names.csv")
  
  manipulation_by_trust<-data|>
    filter(der_financial_year=="2022/23")|>
    group_by(type, der_provider_code, mua)|>
    summarise(count=n())|>
    left_join(provider_names, by=c("der_provider_code"="code"))|>
    filter((der_provider_code %in% trusts_included$der_provider_code) & !is.na(name))|>
    group_by(der_provider_code, type)|>
    mutate(Percentage = round(count / sum(count)*100, 2))
  
}

# Calculating the number of manipulations in theatre vs ED
calculating_manipulations_theatre_vs_ed<-function(data, trusts_included){
  
  proportion_mua_in_theatre_vs_ED<-data|>
    filter(mua!="0" & der_financial_year=="2022/23" & der_provider_code %in% trusts_included$der_provider_code)|>
    group_by(type, mua, der_provider_code)|>
    summarise(count=n())|>
    group_by(type, der_provider_code)|>
    mutate(Percentage = round(count / sum(count)*100, 2))
  
  return(proportion_mua_in_theatre_vs_ED)
}
