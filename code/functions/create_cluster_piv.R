#create long version clustering analysis results
create_cluster_piv = function(cluster){

    cluster_piv = cluster %>%
    select(-cluster) %>% 
    pivot_longer(-X1, names_to = c("date"), values_to = "count")  %>%
    left_join(cluster, by=c('X1'='X1')) %>%
    select(X1, date, count, cluster) %>%
    mutate(cluster = as.character(cluster)) %>%
    mutate(to = as.character(X1)) %>%
    mutate(date = parse_date(as.character(date), format = '%Y%m%d')) %>%
    mutate(prov = substr(X1, 1, 1)) %>% 
    select(-X1) %>% 
    mutate(cluster = chartr("0123456789", "ABCDEFGHIJ", cluster))
  
  #assign cluster colour based on final mean value - a convenience for correctly colouring outflow trajectories from Wuhan
  mean_line = cluster_piv %>% 
    group_by(cluster, date) %>% 
    summarize(mean = mean(count), 
              q_95 = quantile(count, 0.95), 
              q_05 = quantile(count, 0.05))
  
  #relabel clusters based on their mean value on Jan 23
  adjust_cluster = mean_line %>% 
    filter(date == as.Date("2020-01-23")) %>% 
    arrange(-mean) %>% 
    ungroup() %>% 
    mutate(cluster_adjust = c("A", "B", "C", "D"))
  
  old_clusters = adjust_cluster$cluster
  new_clusters = adjust_cluster$cluster_adjust
  
  name_map = setNames(new_clusters, old_clusters)
  
  cluster_piv$cluster = as.character(name_map[as.character(cluster_piv$cluster)])
  
  return(cluster_piv)
  
}