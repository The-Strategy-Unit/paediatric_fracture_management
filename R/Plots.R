
# Total ICB incidence rate map
total_incidence_rate_map<-function(shapefile, data){
  
  icb_shapefile<-st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Integrated_Care_Boards_April_2023_EN_BFC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
  
  
  epidemiology_icb<-data|>
    left_join(shapefile, by=c("icb_name"="ICB23NM"))|>
    st_as_sf()
  
  
  color2<-c("#4575b4", "#91bfdb","#e0f3f8", "#ffffbf" , "#fee090"  ,"#fc8d59" ,"#d73027")
  breaks2 <- c(0, 800, 1000, 1200, 1400, 1600, 1800, 2000)
  
  ggplot()+
    geom_sf(data=icb_shapefile, fill=NA, linewidth=0.8) +
    geom_sf(data = (epidemiology_icb|>group_by(icb_name)|>summarise(incidence=sum(incidence))),
            aes(fill =incidence), colour = NA,alpha = 0.8) +
    scale_fill_stepsn(breaks=breaks2, colors=color2)+
    theme_void() +
    theme(legend.text = element_text(size=15),
          legend.title = element_text(size=18),
          legend.position=c(0.89,0.8),
          legend.key.height = unit(1.2, "cm"),
          title=element_text(size=28)) +
    labs(title="Total fracture incidence rate" ) +
    guides(fill=guide_coloursteps(title="Incidence\n /100,000"))
  
  

}


#ICB incidence rate by fracture type map

incidence_maps_by_fracture_type<-function(data, fracture_type, title){
  
  icb_shapefile<-st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Integrated_Care_Boards_April_2023_EN_BFC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
  
  data<-data|>
    left_join(icb_shapefile, by=c("icb_name"="ICB23NM"))|>
    st_as_sf()
  
  color2<-c("#4575b4", "#91bfdb","#e0f3f8", "#ffffbf" , "#fee090"  ,"#fc8d59" ,"#d73027")

ggplot()+
  # base_map(bbox = my_bbox, basemap = 'voyager') +
  geom_sf(data=icb_shapefile, fill=NA, linewidth=0.8) +
  geom_sf(data = (data|>filter(type==fracture_type)|>group_by(icb_name)|>summarise(incidence=sum(incidence))),
          aes(fill =incidence), colour = NA,alpha = 0.8) +
  scale_fill_stepsn(n.breaks =7, colors=color2)+
  theme_void() +
  theme(legend.text = element_text(size=22),
        legend.title = element_text(size=26),
        legend.position=c(0.89,0.78),
        legend.key.height = unit(1.5, "cm"),
        title=element_text(size=28)) +
  labs(title=title) +
  guides(fill=guide_coloursteps(title="Incidence\n /100,000"))
}

# Incidence graph
plotting_incidence<-function(data){
  
  data|> 
    summarise(frac_no=sum(frac_no), pop_count=sum(pop_count), .by=c(type,  der_financial_year))|>
    mutate(incidence=(frac_no/pop_count)*100000)|>
    ggplot()+
    geom_line(aes(x=der_financial_year, y=incidence,group=type,colour=type), size=1.2)+
    su_theme()+
    labs(x ="", y = "Annual incidence per 100,000")+
    theme(legend.title=element_blank(),
          legend.position="top",
          legend.text=element_text(size=16),
          axis.text=element_text(size=14),
          axis.title=element_text(size=16),
          strip.background = element_rect(fill = "NA", colour = "NA"),
          strip.text = element_text(face = "bold", size=14))+
    scale_colour_manual(values=c("#ec6555", "#f9bf07","#5881c1","#BCBAB8","black"))+
    scale_y_continuous(limits=c(0,NA)) 
 
}


# Incidence by age graph
plotting_incidence_by_age<-function(data){
  
  data|>
    mutate(group_labels=factor(group_labels, levels = c("Female 0-4 yrs", "Male 0-4 yrs", "Female 5-10 yrs", "Male 5-10 yrs", "Female 11-16 yrs", "Male 11-16 yrs")))|>
    ggplot()+
    geom_line(aes(x=der_financial_year, y=incidence,group=group_labels,colour=group_labels, linetype=group_labels), size=1.2 )+
    #   geom_point(aes(x=der_financial_year, y=incidence,group=group_labels,colour=group_labels), size=2.5 )+
    facet_wrap(~type, ncol=3, scales='free')+
    su_theme()+
    labs(x ="", y = "Annual incidence per 100,000")+
    theme(legend.title=element_blank(),
          legend.position=c(0.8,0.2),
          legend.text=element_text(size=14),
          axis.text=element_text(size=13),
          axis.title=element_text(size=16),
          strip.background = element_rect(fill = "NA", colour = "NA"),
          strip.text = element_text(face = "bold", size=16),
          axis.text.x = element_text(angle = 30, vjust = 0.6, hjust=0.5))+
    scale_y_continuous(limits=c(0,NA))+
    scale_linetype_manual(values=c("twodash","solid","twodash", "solid","twodash","solid"))+
    scale_colour_manual(values=c("#f9bf07", "#f9bf07" ,"#ec6555","#ec6555", "black", "black" )) 
  
}


# Seasonal trends graph
plotting_seasonal_trends<-function(ref_data, data){
  
  pop<-ref_data|>
    group_by(year)|>
    summarise(pop_count=sum(pop_count))
  
  data|>
    mutate(year=as.numeric(stringr::str_extract(der_financial_year, "^.{4}")))|>
    summarise(frac_no=n(), .by=c(type,  der_activity_month, year))|>
    left_join(pop, by=c("year"))|>
    mutate(incidence=(frac_no/pop_count)*100000)|>
    ggplot()+
    annotate("rect", xmin=as.Date('2019-06-01'), xmax=as.Date('2019-08-31'), ymin=0, ymax=Inf, alpha=0.5, fill="#f9bf07")+
    annotate("rect", xmin=as.Date('2020-06-01'), xmax=as.Date('2020-08-31'), ymin=0, ymax=Inf, alpha=0.5, fill="#f9bf07")+
    annotate("rect", xmin=as.Date('2021-06-01'), xmax=as.Date('2021-08-31'), ymin=0, ymax=Inf, alpha=0.5, fill="#f9bf07")+
    annotate("rect", xmin=as.Date('2022-06-01'), xmax=as.Date('2022-08-31'), ymin=0, ymax=Inf, alpha=0.5,fill="#f9bf07")+
    annotate("rect", xmin=as.Date('2023-06-01'), xmax=as.Date('2023-08-31'), ymin=0, ymax=Inf, alpha=0.5, fill="#f9bf07") +
    geom_line(aes(x=der_activity_month, y=incidence), linewidth=1.2)+
    facet_wrap(~type, ncol=3, scale="free")+
    su_theme()+
    labs(x ="", y = "Monthly incidence rate/ 100,000")+
    theme(legend.title=element_blank(),
          legend.position=c(0.8,0.2),
          legend.text=element_text(size=16),
          axis.text=element_text(size=14),
          axis.title=element_text(size=16),
          strip.background = element_rect(fill = "NA", colour = "NA"),
          strip.text = element_text(face = "bold", size=16))+
    scale_y_continuous(limits=c(0,NA), expand=c(0.01,0))
  
}


# Seasonality cornwall vs England
plotting_seasonality_cornwall_england<-function(data, provider_data, population_data){
  
  data|>
    left_join(provider_data, by=c("der_provider_code"))|>
    group_by(der_activity_month, icb_name)|>
    summarise(frac_no=n())|>
    full_join(population_data|>group_by(icb_2023_name)|>summarise(pop_count=sum(pop_count)), 
              by=c("icb_name"="icb_2023_name"))|>
    group_by(der_activity_month)|>
    summarise(frac_no=sum(frac_no), pop_count=sum(pop_count, na.rm=TRUE))|>
    mutate(england=(frac_no/pop_count)*100000)  |>
    select(der_activity_month, england)|>
    left_join(
      data|>
        left_join(provider_data, by=c("der_provider_code"))|>
        group_by(der_activity_month, icb_name)|>
        summarise(frac_no=n())|>
        full_join(population_data|>group_by(icb_2023_name)|>summarise(pop_count=sum(pop_count)), 
                  by=c("icb_name"="icb_2023_name"))|>
        filter(icb_name=="NHS Cornwall and the Isles of Scilly Integrated Care Board")|>
        mutate(cornwall=(frac_no/pop_count)*100000)|>
        select(der_activity_month, cornwall),
      by=c("der_activity_month")
    )|>
    filter(der_activity_month>'2021-04-01')|>
    ggplot()+
    annotate("rect", xmin=as.Date('2021-06-01'), xmax=as.Date('2021-08-31'), ymin=0, ymax=Inf, alpha=0.5, fill="#f9bf07")+
    annotate("rect", xmin=as.Date('2022-06-01'), xmax=as.Date('2022-08-31'), ymin=0, ymax=Inf, alpha=0.5,fill="#f9bf07")+
    annotate("rect", xmin=as.Date('2023-06-01'), xmax=as.Date('2023-08-31'), ymin=0, ymax=Inf, alpha=0.5, fill="#f9bf07") +
    geom_line(aes(x=der_activity_month, y=england, colour="England" ), linewidth=1.2)+
    geom_line(aes(x=der_activity_month, y=cornwall, colour="Cornwall"), linewidth=1.2)+
    su_theme()+
    labs(x ="", y = "Monthly incidence rate/ 100,000")+
    theme(legend.title=element_blank(),
          legend.position="top",
          legend.text=element_text(size=14),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14))+
    scale_y_continuous(limits=c(0,NA), expand=c(0,0))+
    scale_color_manual(values=c("#ec6555","#686f73" ))
  
  
}

# Proportion of A&E attendances for fractures

plotting_proportion_of_ED_attendances_for_fracture<-function(ed_numbers, data){

  proportion_per_provider<-ed_numbers|>
  left_join((data|>
               filter(der_financial_year=="2022/23")|>
               group_by(der_provider_code)|>
               summarise(frac_number=n())), by=c("der_provider_code"))|>
  filter(str_starts(der_provider_code, "R"))|>
  mutate(Proportion=(frac_number/count)*100)|>
  filter(!is.na(Proportion))

proportion_per_provider|>
  ggplot(aes(x=reorder(der_provider_code, -Proportion), Proportion))+
  geom_col(fill="#686f73", colour="black")+
  su_theme()+
  theme(title=element_text(size=18, colour="black"),
        plot.title.position = "plot",
        axis.text=element_text(size=16, colour="black"),
        axis.title=element_text(size=18, colour="black"),
        axis.text.x=element_blank())+
  labs(x="Providers", y="Percentage"  , title=NULL, subtitle=NULL)+
  scale_y_continuous(expand=c(0,0), limits=c(0,NA))
}

# Fracture type layout formating

fracture_type_layout<-function(data){
  
  data|>ggplot(aes(x=der_activity_month, y=Percentage))+
    geom_smooth(aes(group=der_financial_year, colour="#f9bf07"),formula=y~1,method="lm",se=FALSE, size=1)+
    geom_line(linewidth=1.2)+
    facet_wrap(~type, ncol=3, scale="free")+
    su_theme()+
    labs(x ="", y = "Percentage")+
    theme(legend.position=c(0.8,0.2),
          legend.text=element_text(size=16),
          axis.text=element_text(size=14),
          axis.title=element_text(size=16),
          strip.background = element_rect(fill = "NA", colour = "NA"),
          strip.text = element_text(face = "bold", size=16))+
    scale_colour_manual(name="", values=c("#f9bf07"), labels=c("Annual average %"))
  
}

# Trends in xrays
plotting_trends_in_xrays<-function(data){
  
  data|>
    group_by(type, xray, der_activity_month,der_financial_year)|>
    summarise(count=n())|>
    group_by(type, der_activity_month,der_financial_year)|>
    mutate(Percentage = round(count / sum(count)*100, 2))|>
    filter(xray==1)|>
    fracture_type_layout()+
    scale_y_continuous(limits=c(0,100))
}

# Trends in follow up
plotting_trends_in_fracture_fup<-function(data){

  data|>
    group_by(type, outpat_attendance, der_activity_month, der_financial_year)|>
    summarise(count=n())|>
    group_by(type, der_activity_month)|>
    mutate(Percentage = round(count / sum(count)*100, 2))|>
    filter(outpat_attendance==1)|>
    fracture_type_layout()+
    scale_y_continuous(limits=c(0,100))
}
# Trends in f2f follow-up
plotting_trends_in_f2f_fup<-function(data){

  data|>
    filter(der_contact_type=="F2F" | der_contact_type=="NF2F")|>
    mutate(der_contact_type=case_when(der_contact_type=="F2F" ~ "Face-to-Face",
                                      der_contact_type=="NF2F" ~ "Virtual"))|>
    group_by(type, outpat_attendance, der_contact_type, der_activity_month)|>
    summarise(count=n())|>
    group_by(type, outpat_attendance, der_activity_month)|>
    mutate(Percentage = round(count / sum(count)*100, 2))|>
    filter(outpat_attendance==1)|>
    ggplot(aes(x=der_activity_month, y=Percentage, group=rev(der_contact_type), fill=der_contact_type))+
    geom_area(position = 'fill')+
    facet_wrap(~type, ncol=3, scales="free")+
    su_theme()+
    labs(x ="", y = "Proportion")+
    theme(legend.title=element_blank(),
          legend.position=c(0.86,0.2),
          legend.text=element_text(size=16),
          axis.text=element_text(size=14),
          axis.title=element_text(size=16),
          strip.background = element_rect(fill = "NA", colour = "NA"),
          strip.text = element_text(face = "bold", size=16))+
    scale_fill_manual(values=c("#f9bf07" , "#686f73"))
  
}

# Number of follow up appointment
plotting_number_of_fups<-function(data){
  
  data|>
    filter(der_financial_year=="2022/23")|>
    group_by(type, outpat_attendance_number )|>
    summarise(count=n())|>
    group_by(type)|>
    reframe(total=sum(count), outpat_attendance_number, count)|>
    mutate(Percentage=(count/total)*100)|>
    filter(outpat_attendance_number<=10)|>
    mutate(outpat_attendance_number=as.factor(outpat_attendance_number))|>
    ggplot(aes(x=outpat_attendance_number, y=count ))+
    geom_bar(stat="identity", position="stack" )+
    facet_wrap(~type, ncol=3, scale="free")+
    su_theme()+
    theme(axis.text=element_text(size=12, colour="black"),
          axis.title=element_text(size=16, colour="black"),
          strip.background = element_rect(fill = "NA", colour = "NA"),
          strip.text = element_text(face = "bold", size=16))+
    labs(x="Number of follow-up appts", y="Number of fractures", title=NULL, subtitle=NULL)+ scale_fill_manual(values=c("#686f73","#f9bf07" ))+
    scale_y_continuous(expand=c(0,0.01), limits=c(0,NA))


}

# Trends in manipulation in ED
plotting_trends_manipulation_in_ED<-function(data){
  data|>
    group_by(type, manipulation_in_ed, der_activity_month, der_financial_year)|>
    summarise(count=n())|>
    group_by(type, der_activity_month, der_financial_year)|>
    mutate(Percentage = round(count / sum(count)*100, 3))|>
    filter(manipulation_in_ed==1)|>
    fracture_type_layout()+
    scale_y_continuous(limits=c(0,6.2))+
    scale_x_date(limits=c(min(data$der_activity_month),max(data$der_activity_month)))
}



# Trends in manipulation in theatre
plotting_trends_manipulation_in_theatre<-function(data){
  
  data|>
    group_by(type, mua_in_theatre, der_activity_month, der_financial_year)|>
    summarise(count=n())|>
    group_by(type, der_activity_month, der_financial_year)|>
    mutate(Percentage = round(count / sum(count)*100, 3))|>
    filter(mua_in_theatre==1)|>
    fracture_type_layout()+
    scale_y_continuous(limits=c(0,13))
  
}

# Manipulation in ED vs theatre
plotting_manipulation_in_ED_vs_theatre<-function(data){
  
  data|>
    filter(type!="Clavicle" & type!="Toe")|>
    filter(manipulation_in_ed=="1"|mua_in_theatre=="1")|>
    group_by(type, mua, der_activity_month)|>
    summarise(count=n())|>
    group_by(type, der_activity_month)|>
    mutate(Percentage = round(count / sum(count)*100, 2))|>
    ggplot(aes(x=der_activity_month, y=Percentage, group=mua, fill=mua))+
    geom_area(position = 'fill')+
    facet_wrap(~type, ncol=2, scales="free")+
    su_theme()+
    labs(x =NULL, title=NULL, subtitle=NULL, y = "Proportion")+
    theme(legend.title=element_blank(),
          legend.position=c(0.72,0.2),
          legend.text=element_text(size=16),
          axis.text=element_text(size=14),
          axis.title=element_text(size=16),
          strip.background = element_rect(fill = "NA", colour = "NA"),
          strip.text = element_text(face = "bold", size=16))+
    scale_fill_manual(values=c("#f9bf07", "black" , "#686f73"))
  
}


# Trend in total manipulations 
plotting_trend_in_total_manipulations<-function(data){
  
  
  data|>
    filter(type!="Clavicle" & type!="Toe")|>
    mutate(manipulation_total=ifelse(manipulation_in_ed=="1"|mua_in_theatre=="1", "1", "0"))|>
    group_by(type, manipulation_total, der_activity_month,der_financial_year)|>
    summarise(count=n())|>
    group_by(type, der_activity_month,der_financial_year )|>
    mutate(Percentage = round(count / sum(count)*100, 2))|>
    filter(manipulation_total=="1")|>
    ggplot(aes(x=der_activity_month, y=Percentage))+
    geom_smooth(aes(group=der_financial_year,colour="#f9bf07" ),formula=y~1,method="lm", se=FALSE, size=1)+
    geom_line( linewidth=1.2)+
    facet_wrap(~type, ncol=2, scale="free")+
    su_theme()+
    labs(x ="", y = "Percentage", title="Percentage of fractures manipulated (theatre & ED)")+
    theme(legend.title=element_blank(),
          legend.position=c(0.8,0.2),
          legend.text=element_text(size=16),
          title=element_text(size=16, colour="black"),
          axis.text=element_text(size=14),
          axis.title=element_text(size=16),
          strip.background = element_rect(fill = "NA", colour = "NA"),
          strip.text = element_text(face = "bold", size=16))+
    scale_y_continuous(limits=c(0,15))+
    scale_colour_manual(name="", values=c("#f9bf07"), labels=c("Annual average %"))
  
  
  
}

# Plotting differences between ed vs utc

plotting_ed_vs_utc<- function(data, variable, title, scale){
  
  
  ed_vs_utc|>
    filter(der_financial_year=="2022/23" )|>
    group_by(department, {{variable}}, type)|>
    summarise(count=n())|>
    group_by(department, type)|>
    summarise(Percentage=round((count/sum(count))*100,1), {{variable}}, type)|>
    filter({{variable}}=="1")|>
    spread(key=department, value=Percentage)|>
    mutate_if(is.numeric, ~replace_na(., 0))|>
    gather(key=department, value=Percentage, -{{variable}}, -type)|>
    ggplot(aes(x=type, y=Percentage, group=department, fill=department))+
    geom_bar(stat="identity", position = "dodge")+
    su_theme()+
    labs(x =NULL, y = "Percentage", title=title)+
    theme(legend.title=element_blank(),
          legend.position="top",
          legend.text=element_text(size=14),
          axis.text=element_text(size=14),
          axis.title=element_text(size=16),
          title=element_text(size=18))+
    scale_fill_manual(values=c("#686f73", "#f9bf07"))+
    scale_y_continuous(expand=c(0.01,0), limits=c(0,scale))+
    geom_text(aes(label = Percentage), vjust = -0.2, colour = "black",
              position = position_dodge(.9), size=4)
}


# Proportion plots by trust and type FOLLOW UP
plots_of_proportion_by_trust_fup<-function(data, fracture_site, title, scale, number, decile_quartile){
  
  data1<-data|>
    filter(type==fracture_site & !is.na(count))|>
    filter(der_contact_type!="N/A")|>
    reframe(total_percent=sum(Percentage), der_contact_type, name, Percentage, outpat_attendance, outpat_procedure_done, count)|>
    mutate(der_contact_type=factor(der_contact_type, levels=c("Virtual", "Face-to-Face")))
  
  data2<-data1|>
    group_by(name)|>
    summarise(count=sum(count))
   
   no_of_providers<-nrow(data2) 
  
data1|>
    ggplot(aes(x=reorder(der_provider_code, -total_percent), Percentage, group=der_contact_type, fill=der_contact_type))+
    geom_bar(position = "stack", stat="identity")+
    su_theme()+
    theme(legend.title=element_blank(),
          legend.position =c(0.83,0.89),
          legend.text=element_text(size=16),
          title=element_text(size=18, colour="black", face="bold"),
          plot.title.position = "plot",
          axis.text=element_text(size=16, colour="black"),
          axis.title=element_text(size=18, colour="black"),
          axis.text.x=element_blank())+
    labs(x="Providers", title=title, subtitle="")+ 
    geom_vline(xintercept=no_of_providers*number, color="#88b083", linewidth=0.8, linetype="dashed")+
    annotate("text", x=(no_of_providers*number)+7, y=max(data1$Percentage)*0.78, label= decile_quartile, color="#88b083", size=4.5) + 
    scale_y_continuous(expand=c(0,0), limits=c(0,scale))+
    scale_fill_manual(values=c("#BCBAB8","#686f73" ))

}


# Proportion plots by trust and type MANIPULATIONS
plots_of_proportion_by_trust<-function(data, fracture_site, title, scale, number, decile_quartile){
  
  data1<-data|>
    filter(type==fracture_site & !is.na(count))
  
  no_of_providers<-nrow(data1)
  
  data1|>
    ggplot()+
    geom_bar(aes(x=reorder(der_provider_code, -Percentage), Percentage), position = "stack", stat="identity")+
    su_theme()+
    theme(legend.title=element_blank(),
          legend.position = "none",
          title=element_text(size=18, colour="black", face="bold"),
          plot.title.position = "plot",
          axis.text=element_text(size=16, colour="black"),
          axis.title=element_text(size=18, colour="black"),
          axis.text.x=element_blank(),
          plot.margin = unit(c(1,3,1,1), "lines") )+
    labs(x="Providers", title=title, subtitle=NULL)+
    geom_vline(xintercept=no_of_providers*number, color="#88b083", linewidth=0.8, linetype="dashed")+
    annotate("text", x=(no_of_providers*number)+7, y=max(data1$Percentage)*0.94, label= "Lowest", color="#88b083", size=4.5) + 
    annotate("text", x=(no_of_providers*number)+7, y=max(data1$Percentage)*0.86, label= decile_quartile, color="#88b083", size=4.5) + 
    scale_y_continuous(expand=c(0,0), limits=c(0,scale))+
    scale_fill_manual(values=c("#686f73" ))
  
}




# Proportion of fractures manipulated in theatre vs ed

plots_of_theatre_vs_ed<-function(data, fracture_site, title){
  
  data|>
    filter(type==fracture_site & !is.na(count))|>
    select(-type, -count)|>
    spread(key=mua, value=Percentage)|>
    mutate_if(is.numeric, ~replace_na(., 0))|>
    gather(key=mua, value=Percentage, -der_provider_code, -type)|>
    ggplot(aes((x =factor(der_provider_code,levels=der_provider_code[mua == "Manipulation in theatre"][order(-Percentage[mua == "Manipulation in theatre"])])), 
               y = Percentage, 
               group=mua,
               fill = mua)) +
    geom_bar(stat = "identity") +
    su_theme()+
    theme(title=element_text(size=18, colour="black"),
          plot.title.position = "plot",
          axis.text=element_text(size=16, colour="black"),
          axis.title=element_text(size=18, colour="black"),
          axis.text.x=element_blank(),
          legend.title=element_blank(),
          legend.position=c(0.65,-0.3),
          legend.text=element_text(size=14),
          plot.margin = unit(c(0, 0, 1, 0), 
                             "inches"))+
    labs(x="Providers", y="Proportion", title=title, subtitle="")+
    scale_y_continuous(expand=c(0,0), limits=c(0,101))+
    scale_fill_manual(values=c("#f9bf07", "black" , "#686f73"))
  
  
}

#Cost of xrays

plotting_cost_of_xray<-function(data){
  
  data|>
    mutate(xray=ifelse(xray=="1", "X-ray", "No X-ray"))|>
    ggplot(aes(x=as.factor(ed_cost), y=count, group=rev(dept_type), fill=dept_type ))+
    geom_bar(stat="identity", position="stack" )+
    facet_wrap(~xray, ncol=2)+
    su_theme()+
    theme(legend.position = "top",
          legend.text=element_text(size=12),
          legend.title=element_blank(),
          axis.text=element_text(size=14, colour="black"),
          axis.title=element_text(size=16, colour="black"),
          strip.background = element_rect(fill = "NA", colour = "NA"),
          strip.text = element_text(face = "bold", size=16))+
    labs(x="Cost (£)", y="Number of attendances", title=NULL, subtitle=NULL)+ scale_fill_manual(values=c("#686f73","#f9bf07" ))+
    scale_y_continuous(expand=c(0,0.01), limits=c(0,4100)) 
  
  
}





