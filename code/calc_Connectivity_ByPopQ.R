reduce2_ntile(connect, pop, 4) -> flow_nt
date_range <- flow_nt$date %>% unique %>% sort
vline.level <- date_range[which(date_range == "2020-01-25")]
vline.level2 <- date_range[which(date_range == "2020-01-02")]
ann_text <- data.frame(x = rep(vline.level2,4),
                       y = rep(0.95,4),
                       nt.to = factor(c("Low",
                                        "Med-Low",
                                        "Med-High",
                                        "High") %>% rev,
                                      levels = c("Low",
                                                 "Med-Low",
                                                 "Med-High",
                                                 "High")),
                       nt.from = factor(c("Low",
                                          "Med-Low",
                                          "Med-High",
                                          "High") %>% rev,
                                        levels = c("Low",
                                                   "Med-Low",
                                                   "Med-High",
                                                   "High")))

flow_nt %>% 
  group_by(from, to, wk, nt.from, nt.to) %>% 
  summarise(value = mean(value)+0.001) -> p_table

p_table %>% 
  ggplot(., aes(x = wk, 
                y = value,
                group = interaction(from, to)))+
  geom_point()+
  facet_grid(nt.from~nt.to)+
  theme_bw() +
  labs(y = "From",
       x = "To")+
  scale_y_log10()

ggsave("connecty_byQ_pairwise.png",
       width = 10,
       height = 10)

ggsave("connecty_byQ_pairwise.pdf",
       width = 10,
       height = 10)


flow_nt %>% 
  ungroup %>%
  mutate(wk = factor(wk),
         wuhan = if_else(from == "420100", T, F))%>% 
  dplyr::select(-from,
                -to,
                -wuhan) %>% 
  group_by(nt.from, 
           nt.to,
           date) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  ungroup %>% 
  mutate(nt.from = factor(nt.from,
                          levels = c(1:4) %>% rev, 
                          labels = c("Low",
                                     "Med-Low",
                                     "Med-High",
                                     "High") %>% rev),
         nt.to = factor(nt.to,
                        levels = c(1:4) %>% rev, 
                        labels = c("Low",
                                   "Med-Low",
                                   "Med-High",
                                   "High") %>% rev))-> p_table


labels_1 <- paste0("to ", 
                   "'",
                   c("Low", "Med-Low", "Med-High", "High"),
                   "'")
names(labels_1) <-  c("Low", "Med-Low", "Med-High", "High")

ggplot(p_table) +
  geom_bar(position = position_fill(),
           stat = "identity",
           aes(x = date, 
               y = value,
               fill = factor(nt.from)),
           width = 1,
           na.rm = T) +
  facet_wrap(~nt.to,
             #scales = "free",
             nrow = 1,
             labeller = labeller(nt.to = labels_1)) +
  theme_cowplot()+
  geom_vline(aes(xintercept = vline.level,
             color = "LNY"),
             size = 1,
             linetype = 2) +
  scale_fill_grey(guide = guide_legend(),
                  start = 0.9,
                  end = 0.1) +
  labs(y = "% of Travellers by Population Quartile",
       x = "",
       fill = "Population\nQuartiles",
       title = "Inbound Travel From Different Population Quartiles") +
  geom_text(data = ann_text, 
            aes(x = x,
                y = y,
                label = c("a","b","c","d"),
                size = 12),
            fontface = "bold") +
  theme(strip.background = element_rect(fill = NA),
        plot.title = element_text(hjust = 0.5,
                                  size = 12),
        axis.title = element_text(size = 12,
                                  face = "bold")) +
  scale_y_continuous(breaks = seq(0,1,0.25),
                     labels = scales::percent(seq(0,1,0.25)))+
  theme(strip.background = element_rect(fill = NA)) +
  scale_color_manual(name = "", values = c(LNY = "red"))  -> p_1

labels_2 <- paste0("from ", 
                   "'",
                   c("Low", "Med-Low", "Med-High", "High"),
                   "'")
names(labels_2) <-  c("Low", "Med-Low", "Med-High", "High")

ggplot(p_table) +
  geom_bar(position = position_fill(),
           stat = "identity",
           aes(x = date, 
               y = value,
               fill = factor(nt.to)),
           width = 1,
           na.rm = T) +
  facet_wrap(~nt.from,
             #scales = "free",
             nrow = 1,
             labeller = labeller(nt.form = labels_2)) +
  theme_cowplot()+
  scale_fill_grey(start = 0.9,
                  end = 0.1)+
  labs(y = "% of Travellers by Population Quartile",
       x = "",
       fill = "Population\nQuartiles",
       title = "Outbound Travel Out to Different Population Quartiles") +
  geom_vline(aes(xintercept = vline.level,
                 color = "LNY"),
             size = 1,
             linetype = 2)+
  geom_text(data = ann_text, aes(x = x,
                                 y = y,
                                 label = c("e","f","g","h")),
            fontface = "bold") +
  theme(strip.background = element_rect(fill = NA),
        plot.title = element_text(hjust = 0.5,
                                  size = 12),
        axis.title = element_text(size = 12,
                                  face = "bold"))+  
  scale_y_continuous(breaks = seq(0,1,0.25),
                     labels = scales::percent(seq(0,1,0.25))) +
  scale_color_manual(name = "", values = c(LNY = "red")) -> p_2

ggarrange(p_1,
          p_2,
          nrow = 2,
          common.legend = T,
          legend = "right")

ggsave(filename = "connectivity_byQ.png",
         #filename = paste0(code_path_win,"Baidu/connectivity.png"),
       width = 18,
       height = 10)

ggsave(filename = "connectivity_byQ.pdf",
       #filename = paste0(code_path_win,"Baidu/connectivity.png"),
       width = 18,
       height = 10,
       dpi = 800,
       useDingbats=FALSE)

# ggsave(filename = paste0(gibbs_path_win,"Yang/Connect_ByPopQ.png"),
#        width = 10,
#        height = 16,
#        dpi = 800)
