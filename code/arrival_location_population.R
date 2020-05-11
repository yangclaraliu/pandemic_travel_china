pop_boxplot = function(cluster_piv, cluster_sum, china){

  #compare population by trajectory cluster
  pop_dens_plot = cluster_sum %>% 
    left_join(china, by= c('to' = 'CNTY_CODE')) %>% 
    ggplot() +
    theme_bw() +
    geom_boxplot(aes(x = cluster, y = pop_2018 / 1000000, fill=cluster), colour='black', size=0.25, outlier.shape = NA) +
    scale_fill_manual(values = c('A'= '#1f78b4', 'B'= '#a6cee3', 'C'= '#b2df8a', 'D'= '#33a02c')) +
    geom_jitter(aes(x = cluster, y = pop_2018 / 1000000), width = 0.1, size=0.01) +
    theme_bw() +
    theme(legend.key.size = unit(2,"line"),
          legend.position="none",
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 7, colour = 'black'),
          axis.text.y = element_text(size = 7, colour = 'black'),
          axis.line = element_line(colour = "black", size=0.15),
          axis.ticks = element_line(colour = "black", size=0.15),
          text = element_text(size = 8),
          plot.margin = margin(0, 0, 0, 0, "cm")) +
    ylab(paste("Population (millions)", sep='')) +
    xlab('') +
    labs(colour="") +
    ggtitle('c') +
    theme(plot.title = element_text(size=8))
  
  return(pop_dens_plot)
}