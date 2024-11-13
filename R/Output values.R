# Output values - MANIPULATIONS

# Proportions manipulated
proportions_manipulated<-function(frac_type){
  
  a<-manipulations_by_trust|>
    filter(type==frac_type)|>
    group_by(der_provider_code)|>
    summarise(Percentage=round((count/sum(count))*100,1), total=sum(count), mua, count)
  
 return(a) 
}


#Lowest quartile
calculating_lowest_quartile<-function(frac_type, number){
  
  a<-proportions_manipulated(frac_type)
  
  b<-a|>
    filter(mua=="Manipulation in theatre" & !is.na(count))|>
    ungroup()|>
    summarise(value=round(quantile(Percentage, probs = c(number)),1))
  
  return(b$value)
}


# calculating reduction in mua in theatre if all trust were able to reduce levels in line with the best performing quartile

calculating_saving<-function(frac_type, number){
  
  a<-proportions_manipulated(frac_type)

  b<-calculating_lowest_quartile(frac_type, number)

c<-a|>
  filter(Percentage>b & mua=="Manipulation in theatre" )|>
  mutate(number_allowed=total*(b/100))|>
  mutate(number_reduced=count-number_allowed)|>
  ungroup()|>
  reframe(number_reduced=round(sum(number_reduced),0))

return(c$number_reduced)

}


# Percentage change 
calculating_percentage_change<-function(frac_type, number){
  
  number<-calculating_saving(frac_type, number)
  
  d<-manipulations_by_trust|>
    filter(type==frac_type & mua=="Manipulation in theatre")|>
    ungroup()|>
    summarise(current_total=sum(count, na.rm=TRUE))
  
  e<-round(((number/d$current_total)*100),1)
  
  return(e)
  
}


# Output values - FOLLOW UPS/Xray

# Proportions manipulated
proportions_f_up<-function(data, frac_type){
  
  a<-data|>
    filter(type==frac_type)|>
    mutate(total=round((1/Percentage)*count*100,))
  
  return(a) 
}


#Lowest quartile
calculating_lowest_10percent<-function(data, frac_type, number){
  
  a<-proportions_f_up(data, frac_type)
  
  b<-a|>
    ungroup()|>
    reframe(value=round(quantile(Percentage, probs = c(number)),1))#|>
   # filter(Percentage<=value)|>
   # reframe(value=round(mean(Percentage),1))
  
  return(b$value)
}


# calculating reduction in  follow ups if all trusts reduce them to the mean of the lowest 10%

calculating_saving_f_up<-function(data, frac_type, number){
  
  a<-proportions_f_up(data, frac_type)
  
  b<-calculating_lowest_10percent(data, frac_type, number)
  
  c<-a|>
    filter(Percentage>b )|>
    mutate(number_allowed=total*(b/100))|>
    mutate(number_reduced=count-number_allowed)|>
    ungroup()|>
    reframe(number_reduced=round(sum(number_reduced),0))
  
  return(c$number_reduced)
  
}


# Percentage change 
calculating_percentage_change_f_up<-function(data, frac_type, number){
  
  number<-calculating_saving_f_up(data, frac_type, number)
  
  d<-data|>
    filter(type==frac_type)|>
    ungroup()|>
    summarise(current_total=sum(count, na.rm=TRUE))
  
  e<-round(((number/d$current_total)*100),1)
  
  return(e)
  
}






