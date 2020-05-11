#create a summary of outflow trajectories
create_cluster_sum = function(cluster_piv, cases){

  #compute the first confirmed case of each prefecture
  min_date = cases %>%
    group_by(CNTY_CODE) %>%
    filter(city_confirmedCount >= 1) %>% 
    summarise(first_infection = min(date)) %>% 
    drop_na() %>% 
    mutate(days_from = as.numeric(first_infection - as.Date('2020-01-01')))
  
  #summarize cluster_piv to return one prefecture code, cluster label, and date of first confirmed case
  cluster_sum = cluster_piv %>% 
    group_by(to) %>% 
    summarise(cluster = unique(cluster)) %>% 
    mutate(to = as.character(to)) %>% 
    left_join(min_date, by=c('to'='CNTY_CODE'))
  
  return(cluster_sum)
  
}
