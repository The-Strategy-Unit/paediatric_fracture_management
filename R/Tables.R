# Tables

## Table of total incidence rate
table_of_total_incidence_rate<-function(data){
  
  data<- data|>
  filter(der_financial_year=="2023/24")|>
  mutate(incidence=round(incidence,0))|>
  select(type, group_labels, incidence)|>
  spread(key=group_labels, value=incidence)|>
  left_join(
    ( data|>
        filter(der_financial_year=="2023/24")|>
        group_by(type)|>
        summarise(frac_no=sum(frac_no), pop_count=sum(pop_count))|>
        mutate(Total=round((frac_no/pop_count)*100000,0)))[,c("type", "Total")], 
    by=c("type")
  )|>
  flextable() |>
  set_header_labels(type="Type", 
                    `Male 0-4 yrs`= "Male\n 0-4 yrs",
                    `Male 5-10 yrs`="Male\n 5-10 yrs",
                    `Male 11-16 yrs`="Male\n 11-16 yrs",
                    `Female 0-4 yrs`="Female\n 0-4 yrs",
                    `Female 5-10 yrs`="Female\n 5-10 yrs",
                    `Female 11-16 yrs`="Female\n 11-16 yrs")|>
  align(part = "header", align = "center")|>
  align(j=1, align = "left")|>
  align(part = "body", align = "center")|>
  align(j=1, part="body", align="left")|>
  align(j=1, part="header", align="left")|>
  bg(bg = "#f9bf07", part = "header") |>
  bold(i = 1, bold = TRUE, part="header")|>
  fontsize(size = 14, part = "all")|>
  padding(padding = 3, part = "all", padding.top=NULL) |>
  autofit()|>
  htmltools_value(ft.align = "left")    
}

# Table of incidence rate by ICB

table_of_icb_incidence_rate<-function(data){
  
 data|>
as.data.frame()|>
  select(icb_name, type, incidence)|>
  mutate(incidence=round(incidence,0))|>
  spread(key=type, value=incidence)|>
  left_join(
    ( data|>
        as.data.frame()|>  
        group_by(icb_name)|>
        summarise(frac_no=sum(frac_no), pop_count=mean(pop_count))|>
        mutate(Total=round((frac_no/pop_count)*100000,0)))[,c("icb_name", "Total")], 
    by=c("icb_name")
  )|>
  filter(!is.na(icb_name))|>
  arrange(desc(Total))|>
  flextable() |>
  set_header_labels(icb_name="ICB")|>
  align(part = "header", align = "center")|>
  align(j=1, align = "left")|>
  align(part = "body", align = "center")|>
  align(j=1, part="body", align="left")|>
  align(j=1, part="header", align="left")|>
  bg(bg = "#f9bf07", part = "header") |>
  bold(i = 1, bold = TRUE, part="header")|>
  fontsize(size = 11, part = "all")|>
  padding(padding = 0, part = "all", padding.top=NULL) |>
  autofit()|>
  htmltools_value(ft.align = "left")   
  
}
## Most common fracture types

table_of_most_common_fractures<-function(data){

 data|>
  filter(der_financial_year=="2023/24")|>
  group_by(description, type)|>
  summarise(Number=n() )|>
   ungroup()|>
   summarise(Percentage=((Number/sum(Number))*100), Number, description, type)|>  
   mutate(Percentage=round(Percentage,1))|>
   arrange(desc(Number))|>
   filter(Number>10)|>
   select(description, type, Number, Percentage)|>
  flextable() |>
  set_header_labels(description="SNOMED description",
                    type="")|>
  align(part = "header", align = "center")|>
  align(j=1, align = "left")|>
  align(part = "body", align = "center")|>
  align(j=1, part="body", align="left")|>
  align(j=1, part="header", align="left")|>
  bg(bg = "#f9bf07", part = "header") |>
  bold(i = 1, bold = TRUE, part="header")|>
  fontsize(size = 14, part = "all")|>
  padding(padding = 3, part = "all", padding.top=NULL) |>
  autofit()|>
  htmltools_value(ft.align = "left")    

}


# Summary of min, median, max
summary_values_by_trust<-function(data){
  
  data_to_summarise<-data
  
  measure <- c('Min', '1st quartile', 'Median', '3rd quartile', 'Max')
  value <- c(min(data_to_summarise$Percentage, na.rm=TRUE),
             quantile(data_to_summarise$Percentage, probs = c(0.25), na.rm=TRUE),
             median(data_to_summarise$Percentage, na.rm=TRUE),
             quantile(data_to_summarise$Percentage, probs = c(0.75), na.rm=TRUE),
             max(data_to_summarise$Percentage, na.rm=TRUE) )
  
  
  summary_table <- data.frame(measure, value)|>
    mutate(value=paste0(round(value,1), " %"))

  summary_table|>
    flextable()|>
    delete_part(part="header")|>
    border_remove()|>
    fontsize(size = 13, part = "all")|>
    padding(padding = 0, part = "all", padding.top=NULL, padding.left=45 ) |>
    autofit()|>
    htmltools_value(ft.align = "left")  
   
}

# Summary values by trust- manipulations
summary_values_by_trust_mua<-function(frac_type){
  
  data_to_summarise<-manipulations_by_trust|>
    filter(mua=="Manipulation in theatre" & type==frac_type)
  
  summary_values_by_trust(data_to_summarise)
  
}

# Summary values by trust- proportion in ed
summary_values_by_trust_prop_in_ed<-function(frac_type){
  
  data_to_summarise<-manipulations_by_trust|>
    filter(type==frac_type & mua!="0" )|>
    group_by(der_provider_code)|>
    summarise(Percentage=round((count/sum(count))*100,1), mua)|>
    filter(mua=="Manipulation in theatre")

  
  measure <- c('Min', '1st quartile', 'Median', '3rd quartile', 'Max')
  value <- c(min(data_to_summarise$Percentage, na.rm=TRUE),
             quantile(data_to_summarise$Percentage, probs = c(0.25), na.rm=TRUE),
             median(data_to_summarise$Percentage, na.rm=TRUE),
             quantile(data_to_summarise$Percentage, probs = c(0.75), na.rm=TRUE),
             max(data_to_summarise$Percentage, na.rm=TRUE) )
  
  
  summary_table <- data.frame(measure, value)|>
    mutate(value=paste0(round(value,1), " %"))
  
  summary_table|>
    flextable()|>
    set_header_labels(measure="",
                      value="")|>
    add_header_lines(values = c("Manipulation in theatre"))|>
    border_remove()|>
    bold(part="header")|>
    fontsize(size = 13, part = "all")|>
    padding(padding = 0, part = "all", padding.top=NULL, padding.left=50 ) |>
    autofit()|>
    htmltools_value(ft.align = "left")  
  
}

# Summary values by trust- follow up
summary_values_by_trust_fup<-function(data, frac_type){
  
  data_to_summarise<-data|>
    filter(type==frac_type)|>
    ungroup()|>
    summarise(Percentage=sum(Percentage), .by=c(der_provider_code))
  
  summary_values_by_trust(data_to_summarise)
  
}

# Summary values by trust- xray
summary_values_by_trust_xray<-function(data, frac_type){
  
  data_to_summarise<-data|>
    filter(type==frac_type)
  
  summary_values_by_trust(data_to_summarise)
  
}


## SNOMED code tables
snomed_code_tables<-function(data, frac_type, number1, number2){
  
  data|>
    filter(type==frac_type)|>
    select(code, description)|>
    flextable()|>
    delete_part(part="header")|>
    border_remove()|>
    align(j=1, align = "left", part="all")|>
   # bg(bg = "#f9bf07", part = "header") |>
   # bold(i = 1, bold = TRUE, part="header")|>
    fontsize(size =number1, part = "all")|>
    padding(padding = 0, part = "all") |>
    line_spacing(space = number2, part = "body")|>
    autofit()|>
    htmltools_value(ft.align = "left")   
  
}

