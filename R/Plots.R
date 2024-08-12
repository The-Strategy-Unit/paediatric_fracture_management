
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

# Fracture type layout formating

fracture_type_layout<-function(data){
  
  data|>ggplot(aes(x=der_activity_month, y=Percentage))+
    geom_smooth(aes(group=der_financial_year),formula=y~1,method="lm",col="#f9bf07",se=FALSE, size=1)+
    geom_line(linewidth=1.2)+
    facet_wrap(~type, ncol=3, scale="free")+
    su_theme()+
    labs(x ="", y = "Percentage")+
    theme(legend.title=element_blank(),
          legend.position=c(0.8,0.2),
          legend.text=element_text(size=16),
          axis.text=element_text(size=14),
          axis.title=element_text(size=16),
          strip.background = element_rect(fill = "NA", colour = "NA"),
          strip.text = element_text(face = "bold", size=16))
  
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
plots_of_proportion_by_trust_fup<-function(data, fracture_site, title, scale){
  
data|>
    filter(type==fracture_site & !is.na(count))|>
    filter(der_contact_type!="N/A")|>
    reframe(total_percent=sum(Percentage), by=c(der_provider_code), der_contact_type, name, Percentage, outpat_attendance, outpat_procedure_done)|>
    mutate(der_contact_type=factor(der_contact_type, levels=c("Virtual", "Face-to-Face")))|>
    ggplot(aes(x=reorder(der_provider_code, -total_percent), Percentage, group=der_contact_type, fill=der_contact_type))+
    geom_bar(position = "stack", stat="identity")+
    su_theme()+
    theme(legend.title=element_blank(),
          legend.position =c(0.75,0.8),
          legend.text=element_text(size=16),
          title=element_text(size=18, colour="black"),
          plot.title.position = "plot",
          axis.text=element_text(size=16, colour="black"),
          axis.title=element_text(size=18, colour="black"),
          axis.text.x=element_blank())+
    labs(x="Providers", title=title, subtitle="")+
    scale_y_continuous(expand=c(0,0), limits=c(0,scale))+
    scale_fill_manual(values=c("#BCBAB8","#686f73" ))

}


# Proportion plots by trust and type MANIPULATIONS
plots_of_proportion_by_trust<-function(data, fracture_site, title, scale){
  
  data|>
    filter(type==fracture_site & !is.na(count))|>
    ggplot(aes(x=reorder(der_provider_code, -Percentage), Percentage))+
    geom_bar(position = "stack", stat="identity")+
    su_theme()+
    theme(legend.title=element_blank(),
          legend.position = "top",
          title=element_text(size=18, colour="black"),
          plot.title.position = "plot",
          axis.text=element_text(size=16, colour="black"),
          axis.title=element_text(size=18, colour="black"),
          axis.text.x=element_blank())+
    labs(x="Providers", title=title, subtitle="")+
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



