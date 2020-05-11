pacman::p_load(cluster,
               factoextra)

pub_theme = theme(axis.text.y = element_text(size = 5),
                  axis.text.x = element_text(size = 5),
                  axis.title.y = element_text(size = 6),
                  axis.title.x = element_text(size = 6),
                  plot.title = element_text(size=6),
                  panel.grid.major = element_blank(),
                  panel.border = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.line = element_line(colour = "black", size=0.15),
                  axis.ticks = element_line(colour = "black", size=0.15),
                  text = element_text(size = 6))
#load data
connect = read_rds('/Users/hamishgibbs/Dropbox/nCov-2019/data_sources/mobility_data/china_prf_connectivity_0101_0301.rds')
dates = connect$date

#combine daily matrices longer
for (i in 1:length(dates)){
  date = dates[i]
  
  connectivity_day = connect[connect$date == date, 'connect'][[1]][[1]] %>% 
    filter(from == '420100') %>% 
    t()
  
  connectivity_day_df = tibble(connectivity_day[2:length(connectivity_day)]) %>% 
    mutate(location = rownames(connectivity_day)[2:length(connectivity_day)])
  
  colnames(connectivity_day_df) = c('flow', 'location')
  connectivity_day_df = connectivity_day_df %>% 
    mutate(flow = as.numeric(flow)) %>% 
    mutate(date = date)
  
  if (date == dates[1]){
    connectivity_day_df_save = connectivity_day_df
  }else{
    connectivity_day_df_save = rbind(connectivity_day_df_save, connectivity_day_df)
  }
  
}

#filter dates below Jan 23
connectivity_day_df_save = connectivity_day_df_save %>% 
  filter(date <= as.Date('2020-01-23'))

#threshold low connectivity destinations
group_sum = connectivity_day_df_save %>% 
  group_by(location) %>% 
  summarise(flow_sum = sum(flow, na.rm = T),
            flow_mean = mean(flow, na.rm = T)) %>% 
  filter(flow_mean > 0.005)

#pivot trajectory data wider
traj = connectivity_day_df_save %>% 
  filter(location %in% group_sum$location) %>% 
  left_join(group_sum, by = c('location' = 'location')) %>% 
  mutate(flow_scaled = flow / flow_sum) %>% 
  select(location, date, flow_scaled) %>% 
  pivot_wider(names_from = date, values_from = flow_scaled)


create_silhourett_plot_figure = function(traj, c_num){
  
  create_cluster_plots = function(traj, c_num){
    p = traj %>% 
      mutate(cluster = clust$cluster) %>% 
      filter(cluster == c_num) %>% 
      select(- cluster) %>% 
      pivot_longer(cols = -location, names_to = "date", values_to = 'flow') %>% 
      mutate(date = as.Date(as.character(date))) %>% 
      ggplot() +
      theme_bw() + 
      geom_path(aes(x = date, y = flow, group = location), size = 0.1) +
      scale_y_continuous(breaks = c(0.025, 0.05, 0.075, 0.1)) +
      annotate('text', x = as.Date('2020-01-04'), y = 0.09, label = paste0('Cluster ', c_num), size = 1.5) +
      pub_theme +
      xlab('') +
      ylab('')
    return(p)
  }
  
  X = traj %>% 
    select(-location)
  
  clust = kmeans(X, centers = c_num)
  
  traj = traj %>% 
    mutate(cluster = clust$cluster)
  
  dis = dist(X)^2
  sil = silhouette (traj$cluster, dis)
  
  sp = fviz_silhouette(sil) +
    theme_bw() + 
    scale_fill_manual(values = gray.colors(c_num)) +
    scale_colour_manual(values = gray.colors(c_num)) +
    pub_theme +
    theme(legend.key.width = unit(0.4, 'cm'),
          legend.key.height = unit(0.4, 'cm')) +
    labs(fill = 'Cluster') +
    guides(colour = FALSE) +
    theme(axis.text.x = element_blank())
  
  cluster_plots = list()
  for (i in unique(clust$cluster)) cluster_plots[[i]] = create_cluster_plots(traj, i)
  
  return(list(cluster_plots, sp))
  
}

c_num = 2

cluster_sup = create_cluster_supplement_figure(traj, c_num)

arr = do.call("grid.arrange", c(cluster_sup[[1]], ncol=2))
arr

p = arrangeGrob(arr, cluster_sup[[2]], ncol=1)

ggsave(paste0('./cluster_plot_supplement_', c_num, '_clusters.png'), p,
       width = 5,
       height = 4,
       units = 'in')

ggsave(paste0('./cluster_plot_supplement_', c_num, '_clusters.pdf'), p,
       width = 5,
       height = 4,
       units = 'in', useDingbats=FALSE)





