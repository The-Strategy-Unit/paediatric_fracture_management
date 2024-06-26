# Tables

## Table of total incidence rate
table_of_total_incidence_rate<-function(data){
  
  data<- data|>
  filter(der_financial_year=="2023/24")|>
  mutate(incidence=round(incidence,1))|>
  select(type, group_labels, incidence)|>
  spread(key=group_labels, value=incidence)|>
  left_join(
    ( data|>
        filter(der_financial_year=="2023/24")|>
        group_by(type)|>
        summarise(frac_no=sum(frac_no), pop_count=sum(pop_count))|>
        mutate(Total=round((frac_no/pop_count)*100000,1)))[,c("type", "Total")], 
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
  
  data<- data|>
as.data.frame()|>
  select(icb_2023_name, type, incidence)|>
  mutate(incidence=round(incidence,1))|>
  spread(key=type, value=incidence)|>
  left_join(
    ( data|>
        as.data.frame()|>  
        group_by(icb_2023_name)|>
        summarise(frac_no=sum(frac_no), pop_count=sum(pop_count))|>
        mutate(Total=round((frac_no/pop_count)*100000,1)))[,c("icb_2023_name", "Total")], 
    by=c("icb_2023_name")
  )|>
  filter(!is.na(icb_2023_name))|>
  arrange(desc(Total))|>
  flextable() |>
  set_header_labels(icb_2023_name="ICB")|>
  align(part = "header", align = "center")|>
  align(j=1, align = "left")|>
  align(part = "body", align = "center")|>
  align(j=1, part="body", align="left")|>
  align(j=1, part="header", align="left")|>
  bg(bg = "#f9bf07", part = "header") |>
  bold(i = 1, bold = TRUE, part="header")|>
  fontsize(size = 12, part = "all")|>
  padding(padding = 2, part = "all", padding.top=NULL) |>
  autofit()|>
  htmltools_value(ft.align = "left")    
}
## Most common fracture types

table_of_most_common_fractures<-function(data){

data|>
  filter(der_financial_year=="2023/24")|>
  group_by(description, type)|>
  summarise(Number=n() )|>
  arrange(desc(Number))|>
  filter(Number>10)|>
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




