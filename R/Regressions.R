# Formatting data for regression analysis

formatting_data_for_regression<-function(data){

  regression_data<-data|>
  filter(imd_quintiles!="Missing/Outside England")|>
  filter(dept_type=="Major Emergency Dept" | dept_type=="Urgent Treatment Centre/Walk in centre")|>
  filter(sex!="Missing/Unknown")|>
  mutate(imd_quintiles= relevel(factor(imd_quintiles, ordered = FALSE ), ref = "3"))|>
  mutate(ethnicity_broad= relevel(ethnicity_broad, ref = "White"))|>
  mutate(type= relevel(factor( type, ordered = FALSE ), ref = "Forearm"))|>  
  mutate(day_of_week= relevel(factor( day_of_week, ordered = FALSE ), ref = "Tuesday"))|>
  mutate(age= relevel(age, ref = "5-10 yrs"))|>
  mutate(der_financial_year= relevel(factor(der_financial_year, ordered = FALSE ), ref = "2021/22"))|> 
  mutate(month=month(der_activity_month))|>
  mutate(season=case_when(month==12| month==1| month==2~ "Winter",
                          month>=3 & month<=5 ~ "Spring",
                          month>=6 & month<=8~ "Summer",
                          month>=9 & month<=11~ "Autumn",))
  
  return(regression_data)

}





# Manipulation of fractures in emergency department

manipulation_regression<-function(data, frac_type){
  
  regression_data<-formatting_data_for_regression(data)|>
  filter(type==frac_type)|>
  filter(mua=="Manipulation in theatre"| mua=="Manipulation in ED")|>
    mutate(mua_in_theatre=as.numeric(mua_in_theatre))
  
  manipulation_model<- glm(mua_in_theatre ~ sex + age + ethnicity_broad + imd_quintiles+ dept_type + day
                                   + time + season +der_financial_year ,family=binomial(link='logit'),data=regression_data)
  
  summary(manipulation_model)
  
  pval <- summary(manipulation_model)$coefficients[,4]
  Allsummary <- cbind(Estimate = coef(manipulation_model), confint(manipulation_model))
  Allsummary<-exp(Allsummary)
  Allsummary<-cbind(Allsummary, pvalues = pval)
  Allsummary<-as.data.frame(Allsummary)|>
    mutate(across(where(is.numeric), round, digits=2))|>
    mutate( CI=paste0(`2.5 %`, " to ", `97.5 %`))|>
    mutate(`P values`=ifelse(pvalues<0.05, (paste0(`pvalues`, "*")),`pvalues`))|>
    mutate(`P values`=ifelse(`P values`=="0*", "<0.001*",`P values`))|>
    tibble::rownames_to_column("Group")|>
    dplyr::select(-pvalues, -'2.5 %', -'97.5 %')|>
    mutate(Category=ifelse(grepl('sex', Group), "Sex",ifelse(grepl('age', Group), "Age",ifelse(grepl('ethnicity', Group), "Ethnicity",ifelse(grepl('imd', Group), "IMD Quintiles",ifelse(grepl('dept', Group), "Department type",ifelse(grepl('day', Group), "Day of the week",ifelse(grepl('time', Group), "Time of day",ifelse(grepl('season', Group), "Time of year",ifelse(grepl('der_', Group), "Year",""))))))))))
  
  Allsummary$Category<-factor(Allsummary$Category, levels=c('','Sex', 'Age', 'Ethnicity', 'IMD Quintiles', 'Department type' , 'Day of the week', 'Time of day', 'Time of year' , 'Year'))
  
  Allsummary<- Allsummary|>
    dplyr::add_row(Group = "Female", Estimate=1, CI="", `P values`="Reference", Category="Sex", .before = 2)|>
    dplyr::add_row(Group = "5-10 yrs", Estimate=1, CI="", `P values`="Reference", Category="Age", .before = 4)|>
    dplyr::add_row(Group = "White", Estimate=1, CI="", `P values`="Reference", Category="Ethnicity", .before = 7)|>
    dplyr::add_row(Group = "3", Estimate=1, CI="", `P values`="Reference", Category="IMD Quintiles", .before = 13)|>
    dplyr::add_row(Group = "Major Emergency Department", Estimate=1, CI="", `P values`="Reference", Category="Department type", .before =18)|>
    dplyr::add_row(Group = "Weekday", Estimate=1, CI="", `P values`="Reference", Category="Day of the week", .before =20)|>
    dplyr::add_row(Group = "Daytime 7am-7pm", Estimate=1, CI="", `P values`="Reference", Category="Time of day", .before = 22)|>
    dplyr::add_row(Group = "Autumn", Estimate=1, CI="", `P values`="Reference", Category="Time of year", .before = 24)|>
    dplyr::add_row(Group = "2021/22", Estimate=1, CI="", `P values`="Reference", Category="Year", .before =28)|>
    mutate(Group = str_remove_all(Group, "sex"))|>
    mutate(Group = str_remove_all(Group, "age"))|>
    mutate(Group = str_remove_all(Group, "ethnicity_broad"))|>
    mutate(Group = str_remove_all(Group, "dept_type"))|>
    mutate(Group = str_remove_all(Group, "imd_quintiles"))|>
    mutate(Group = str_remove_all(Group, "day"))|>
    mutate(Group = str_remove_all(Group, "time"))|>
    mutate(Group = str_remove_all(Group, "season"))|>
    mutate(Group = str_remove_all(Group, "der_financial_year"))
  
  # table 
  regression_manipulation_table<-as_grouped_data(x=Allsummary, groups = c("Category"), columns=NULL) %>%
    as_flextable( hide_grouplabel = TRUE)%>%
    set_header_labels(
                      Group= "",
                      Estimate="Odds Ratio",
                      CI="Confidence Intervals",
                      `P values`= "P value" )   %>%
    align(part="all", align="right")|>
    align(j=1, part="all", align= "left")|>
    align(i = 3,  align = "left")%>%
    align(i = 6,  align = "left")%>%
    align( i = 10,  align = "left")%>%
    align( i = 17,  align = "left")%>%
    align( i = 23,  align = "left")%>%
    align( i = 26,  align = "left")%>%
    align( i = 29,  align = "left")%>%
    align( i = 32,  align = "left")%>%
    align( i = 37,  align = "left")%>%
    bold( i = 3, bold = TRUE)%>%
    bold( i = 6, bold = TRUE)%>%
    bold( i = 10, bold = TRUE)%>%
    bold(i = 17, bold = TRUE)%>%
    bold(i = 23, bold = TRUE)%>%
    bold( i = 26, bold = TRUE)%>%
    bold(i = 29, bold = TRUE)%>%
    bold(i = 32, bold = TRUE)%>%
    bold(i = 37, bold = TRUE)%>%
    bg( bg = "gold", part = "header")  %>%
    padding(padding.top = 0, padding.bottom=0, part = "all") |>
    line_spacing(space = 0.9, part = "body")|>
    autofit()|>
    htmltools_value(ft.align = "left")  
  
  
}

# Follow up appointment regression

f_up_regression<-function(data){
  
  regression_data<-formatting_data_for_regression(data)|>
    mutate(outpat_attendance=as.numeric(outpat_attendance))
  
  manipulation_model<- glm(outpat_attendance ~ sex + age + ethnicity_broad + imd_quintiles+ dept_type + day
                           + time + season +der_financial_year+type  ,family=binomial(link='logit'),data=regression_data)
  
  summary(manipulation_model)
  
  pval <- summary(manipulation_model)$coefficients[,4]
  Allsummary <- cbind(Estimate = coef(manipulation_model), confint(manipulation_model))
  Allsummary<-exp(Allsummary)
  Allsummary<-cbind(Allsummary, pvalues = pval)
  Allsummary<-as.data.frame(Allsummary)|>
    mutate(across(where(is.numeric), round, digits=2))|>
    mutate( CI=paste0(`2.5 %`, " to ", `97.5 %`))|>
    mutate(`P values`=ifelse(pvalues<0.05, (paste0(`pvalues`, "*")),`pvalues`))|>
    mutate(`P values`=ifelse(`P values`=="0*", "<0.001*",`P values`))|>
    tibble::rownames_to_column("Group")|>
    dplyr::select(-pvalues, -'2.5 %', -'97.5 %')|>
    mutate(Category=ifelse(grepl('sex', Group), "Sex",ifelse(grepl('age', Group), "Age",ifelse(grepl('ethnicity', Group),
                                                                                               "Ethnicity",ifelse(grepl('imd', Group), "IMD Quintiles",ifelse(grepl('dept', Group), "Department type",
                                                                                                                                                              ifelse(grepl('day', Group), "Day of the week",ifelse(grepl('time', Group), "Time of day",
                                                                                                                                                                                                                   ifelse(grepl('season', Group), "Time of year",ifelse(grepl('der_', Group), "Year",
                                                                                                                                                                                                                                                                        ifelse(grepl('type', Group), "Fracture type","")))))))))))
  
  Allsummary$Category<-factor(Allsummary$Category, levels=c('','Sex', 'Age', 'Ethnicity', 'IMD Quintiles', 'Department type' , 'Day of the week', 'Time of day', 'Time of year' , 'Year', 'Fracture type'))
  
  Allsummary<- Allsummary|>
    dplyr::add_row(Group = "Female", Estimate=1, CI="", `P values`="Reference", Category="Sex", .before = 2)|>
    dplyr::add_row(Group = "5-10 yrs", Estimate=1, CI="", `P values`="Reference", Category="Age", .before = 4)|>
    dplyr::add_row(Group = "White", Estimate=1, CI="", `P values`="Reference", Category="Ethnicity", .before = 7)|>
    dplyr::add_row(Group = "3", Estimate=1, CI="", `P values`="Reference", Category="IMD Quintiles", .before = 13)|>
    dplyr::add_row(Group = "Major Emergency Department", Estimate=1, CI="", `P values`="Reference", Category="Department type", .before =18)|>
    dplyr::add_row(Group = "Weekday", Estimate=1, CI="", `P values`="Reference", Category="Day of the week", .before =20)|>
    dplyr::add_row(Group = "Daytime 7am-7pm", Estimate=1, CI="", `P values`="Reference", Category="Time of day", .before = 22)|>
    dplyr::add_row(Group = "Autumn", Estimate=1, CI="", `P values`="Reference", Category="Time of year", .before = 24)|>
    dplyr::add_row(Group = "2021/22", Estimate=1, CI="", `P values`="Reference", Category="Year", .before =28)|>
    dplyr::add_row(Group = "Forearm", Estimate=1, CI="", `P values`="Reference", Category="Fracture type", .before =34)|>
    mutate(Group = str_remove_all(Group, "sex"))|>
    mutate(Group = str_remove_all(Group, "age"))|>
    mutate(Group = str_remove_all(Group, "ethnicity_broad"))|>
    mutate(Group = str_remove_all(Group, "dept_type"))|>
    mutate(Group = str_remove_all(Group, "imd_quintiles"))|>
    mutate(Group = str_remove_all(Group, "day"))|>
    mutate(Group = str_remove_all(Group, "time"))|>
    mutate(Group = str_remove_all(Group, "season"))|>
    mutate(Group = str_remove_all(Group, "der_financial_year"))|>
    mutate(Group = str_remove_all(Group, "type"))
  
  # table 
  regression_manipulation_table<-as_grouped_data(x=Allsummary, groups = c("Category"), columns=NULL) %>%
    as_flextable( hide_grouplabel = TRUE)%>%
    set_header_labels(
      Group= "",
      Estimate="Odds Ratio",
      CI="Confidence Intervals",
      `P values`= "P value" )   %>%
    align( part="all", align="right")|>
    align(j=1, part="all", align= "left")|>
    align(i = 3,  align = "left")%>%
    align(i = 6,  align = "left")%>%
    align( i = 10,  align = "left")%>%
    align(i = 17,  align = "left")%>%
    align( i = 23,  align = "left")%>%
    align( i = 26,  align = "left")%>%
    align( i = 29,  align = "left")%>%
    align( i = 32,  align = "left")%>%
    align( i = 37,  align = "left")%>%
    align( i = 44,  align = "left")%>%
    bold( i = 3, bold = TRUE)%>%
    bold( i = 6, bold = TRUE)%>%
    bold( i = 10, bold = TRUE)%>%
    bold(i = 17, bold = TRUE)%>%
    bold(i = 23, bold = TRUE)%>%
    bold(i = 26, bold = TRUE)%>%
    bold( i = 29, bold = TRUE)%>%
    bold( i = 32, bold = TRUE)%>%
    bold( i = 37, bold = TRUE)%>%
    bold( i = 44, bold = TRUE)%>%
    bg( bg = "gold", part = "header")  %>%
    fontsize( size = 9.5, part = "body")|>
    padding(padding.top = 0, padding.bottom=0, part = "all") |>
    line_spacing(space = 0.8, part = "body")|>
    autofit()|>
    htmltools_value(ft.align = "left")    
  
  
  
}
#







