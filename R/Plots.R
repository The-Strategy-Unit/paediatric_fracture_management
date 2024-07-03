
# Total ICB incidence rate map
total_incidence_rate_map<-function(shapefile, data){
  
  icb_shapefile<-st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Integrated_Care_Boards_April_2023_EN_BFC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
  
  
  epidemiology_icb<-data|>
    left_join(shapefile, by=c("icb_2023_name"="ICB23NM"))|>
    st_as_sf()
  
  
  color2<-c("#4575b4", "#91bfdb","#e0f3f8", "#ffffbf" , "#fee090"  ,"#fc8d59" ,"#d73027")
  breaks2 <- c(0, 800, 1000, 1200, 1400, 1600, 1800, 2000)
  
  ggplot()+
    geom_sf(data=icb_shapefile, fill=NA, linewidth=0.8) +
    geom_sf(data = (epidemiology_icb|>group_by(icb_2023_name)|>summarise(incidence=sum(incidence))),
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
    left_join(icb_shapefile, by=c("icb_2023_name"="ICB23NM"))|>
    st_as_sf()
  
  color2<-c("#4575b4", "#91bfdb","#e0f3f8", "#ffffbf" , "#fee090"  ,"#fc8d59" ,"#d73027")

ggplot()+
  # base_map(bbox = my_bbox, basemap = 'voyager') +
  geom_sf(data=icb_shapefile, fill=NA, linewidth=0.8) +
  geom_sf(data = (data|>filter(type==fracture_type)|>group_by(icb_2023_name)|>summarise(incidence=sum(incidence))),
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
  
  data|>ggplot()+
    geom_line(aes(x=der_activity_month, y=Percentage), linewidth=1.2)+
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


# Proportion plots by trust and type
plots_of_proportion_by_trust<-function(data, fracture_site, title, scale){
  
data|>
    filter(type==fracture_site)|>
    ggplot(aes(x=reorder(der_provider_code, -Percentage), Percentage))+
    geom_col(fill="#686f73", colour="black")+
    su_theme()+
    theme(title=element_text(size=18, colour="black"),
          plot.title.position = "plot",
          axis.text=element_text(size=16, colour="black"),
          axis.title=element_text(size=18, colour="black"),
          axis.text.x=element_blank())+
    labs(x="Providers", title=title, subtitle="")+
    scale_y_continuous(expand=c(0,0), limits=c(0,scale))

}

# Proportion of fractures manipulated in theatre vs ed

plots_of_theatre_vs_ed<-function(data, fracture_site, title){
  
  data|>
    filter(type==fracture_site)|>
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
          legend.position="top",
          legend.text=element_text(size=16))+
    labs(x="Providers", y="Proportion", title=title, subtitle="")+
    scale_y_continuous(expand=c(0,0), limits=c(0,100))+
    scale_fill_manual(values=c("#f9bf07" , "#686f73"))
  
  
  
}



