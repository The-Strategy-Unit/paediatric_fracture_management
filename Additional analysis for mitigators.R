
library(dplyr)
library(tidyr)
library(janitor)
library(targets)
library(stringr)
library(StrategyUnitTheme)
library(ggplot2)
library(basemapR)
library(sf)
library(flextable)
library(egg)
library(table1)
library(lubridate)
library(forcats)
library(scales)
library(patchwork)
library(tibble)
library(ggpubr)
library(plotly)

windowsFonts(`Segoe UI` = windowsFont('Segoe UI'))

paed_fractures<-tar_read(paed_fractures)
total_ed_attendances<-tar_read(total_ed_attendances)
f_up_by_trust<-tar_read(f_up_by_trust)
manipulations_by_trust<-tar_read(manipulations_by_trust)
trusts_with_120_attendances<-tar_read(trusts_with_120_attendances)

provider_names<-read.csv("Data/provider_names.csv")

#INPATIENT MITIGATOR

inpat_from_ED<-paed_fractures|>
  filter(apce_ident!="NULL")|>
  filter(type=="Forearm"| type=="Elbow")|>
  # mutate(shorten_dx=substr(der_primary_diagnosis_code, start = 1, stop = 3))|>
  group_by(der_primary_diagnosis_code)|>
  summarise(count=n())

#OUTPATIENT MITIGATOR

paed_fractures|>
  group_by(treatment_function_code)|>
  summarise(total=n())

paed_fractures|>
  filter(appointment_date!="NULL")|>
  filter(der_attendance_type=="Attend" )|>
  group_by(opa_referral_source)|>
  summarise(total=n())|>
  ungroup()|>
  reframe(percent=(total/sum(total))*100, total, opa_referral_source)

all_outpat_TO<-read.csv("all_outpat.csv")|>
  clean_names()|>
  filter(der_financial_year=="2022/23")|>
  # filter(der_attendance_type=="Attend")|>
  group_by(der_provider_code)|>
  summarise(total=sum(number))


new_fup_by_trust<-  paed_fractures|>
  mutate(outpat_attendance=ifelse(treatment_function_code=="420" |
                                    treatment_function_code=="650", 0,outpat_attendance))|>
  filter(der_financial_year=="2022/23")|>
  group_by(type, der_provider_code, outpat_attendance,der_attendance_type)|>
  summarise(count=n())|>
  group_by(der_provider_code, type)|>
  mutate(Percentage = round(count / sum(count)*100, 2) )|>
  left_join(provider_names, by=c("der_provider_code"="code"))|>
  filter((der_provider_code %in% trusts_with_120_attendances$der_provider_code) & !is.na(name))|>
  #    filter(der_attendance_type=="Attend"|der_attendance_type=="NULL")|>
  filter(outpat_attendance=="1")


f_up_by_trust_OUTPAT<-new_fup_by_trust|>
  ungroup()|>
  group_by(der_provider_code)|>
  summarise(frac_1st_appt_number=sum(count))

fup_1st_appt_percent<-all_outpat_TO|>
  left_join(f_up_by_trust_OUTPAT, by=c("der_provider_code"))|>
  filter(!is.na(frac_1st_appt_number))|>
  mutate(percent_TO_for_1st_fup=(frac_1st_appt_number/total)*100)

a<- new_fup_by_trust|>
  group_by(type, der_provider_code)|>
  reframe(count=sum(count), Percentage=sum(Percentage))|>
  group_by(type)|>
  mutate(total=round((1/Percentage)*count*100))

b<-a|>
  ungroup()|>
  group_by(type)|>
  summarise(value=round(quantile(Percentage, probs = c(0.1)),1))

c<-a|>
  left_join(b,by=c("type"))|>
  mutate(could_save=ifelse(Percentage>value,1,0 ))|>
  mutate(number_allowed=total*(value/100))|>
  mutate(number_reduced=count-number_allowed)|>
  filter(could_save==1)|>
  group_by(der_provider_code)|>
  reframe(number_reduced=round(sum(number_reduced),0))


options(scipen=999)

f_up_by_trust_OUTPAT<-all_outpat_TO|>
  full_join(c,by=c("der_provider_code"))|>
  mutate(potential_reduction_percent=(number_reduced/total)*100)|>
  filter(!is.na(number_reduced)&!is.na(total))|>
  ungroup()|>
  summarise(total_reduction=sum(number_reduced), total_appt=sum(total) )|>
  mutate(percent=(total_reduction/total_appt)*100)

# Attend rate
test_paed_fracture<-paed_fractures|>
  filter(der_attendance_type!="NULL")|>
  group_by(der_attendance_type)|>
  summarise(count=n())|>
  ungroup()|>
  reframe(count, der_attendance_type, total=sum(count))|>
  mutate(percent=(count/total)*100)

test_all_TO_outpat<-read.csv("all_outpat.csv")|>
  clean_names()|>
  filter(der_financial_year=="2022/23")|>
  filter(der_attendance_type!="NULL")|>
  group_by(der_attendance_type)|>
  summarise(count=sum(number))|>
  ungroup()|>
  reframe(count, der_attendance_type, total=sum(count))|>
  mutate(percent=(count/total)*100)