# statistics used in manuscript
cases %>% 
  filter(cases_tot != 0) %>% 
  filter(CNTY_CODE != "420100") %>% 
  #pull(CNTY_CODE) %>% unique %>% length
  # mutate(prv = substr(CNTY_CODE,1,2)) %>% 
  # filter(prv != 42) %>% 
  group_by(CNTY_CODE) %>% 
  mutate(date_rank = rank(date)) %>% 
  filter(date_rank == 1) %>% 
  mutate(date_diff = date - as.Date("2020-01-23")) %>% 
  pull(date_diff) %>% 
  quantile(., c(0, 0.25, 0.75, 1))

# compare to 2019 and other places in China
move_in_history %>% 
  bind_rows() %>% 
  as_tibble %>% 
  bind_rows(move_out_history %>% 
              bind_rows()) %>% 
  mutate(value = unlist(value),
         date = lubridate::ymd(date),
         year = lubridate::year(date),
         date_diff = if_else(year == 2019,
                             date - as.Date("2019-02-05"),
                             date - as.Date("2020-01-25")),
         CNTY_CODE = as.character(CNTY_CODE),
         CNTY_CODE = case_when(CNTY_CODE == "110000" ~ "110100",
                               CNTY_CODE == "120000" ~ "120100",
                               CNTY_CODE == "310000" ~ "310100",
                               CNTY_CODE == "500000" ~ "500100",
                               TRUE ~ CNTY_CODE)) %>% #filter(date_diff %in% -5:0) %>%  View() 
ungroup %>% 
  dplyr::select(-date) %>% 
  group_by(CNTY_CODE,
           date_diff,
           type) %>% 
  distinct %>% 
  pivot_wider(values_from = value,
              names_from = year) %>% 
  left_join(pop %>% mutate(nt = ntile(pop_tot,4)), 
            by = "CNTY_CODE") -> tab_diff

tab_diff %>% 
  #  filter(date_diff <= 0) %>% 
  filter(date_diff %in% c(-7:-2)) %>%
  group_by(CNTY_CODE, type) %>% 
  mutate(avg19 = mean(`2019`),
         var19 = var(`2019`)) %>% 
  group_by(CNTY_CODE, type, avg19, var19, nt, pop_tot) %>% 
  summarise(`2019` = mean(`2019`, na.rm = T),
            `2020` = mean(`2020`, na.rm = T)) %>% 
  #mutate(rk20 = rank(`2020`)) %>% 
  #filter(rk20 == 6) %>% 
  mutate(val1 = `2020`/avg19 - 1,
         val2 = (`2020` - avg19)/sqrt(var19)) %>% 
  group_by(type) %>% 
  mutate(val1_rk = rank(desc(val1)),
         val2_rk = rank(desc(val2)),
         wuhan = if_else(CNTY_CODE %in% c("420100"), T, F)) %>% 
  filter(type == "history_out") -> p_table

p_table %>%
  filter(wuhan == T)

p_table %>% 
  filter(!is.na(pop_tot)) %>% 
  ggplot(., aes(x = val1_rk,
                y = val1,
                color = log(pop_tot, base = 10)))+
  geom_point()+
  geom_label_repel(data = p_table[p_table$wuhan == T, ],
            aes(x = val1_rk,
                y = val1,
                label = c("Wuhan")),
            force = 50,
            color = "black",
            direction = "both",
            arrow = arrow(length = unit(0.01, "npc"),
                          type = "closed"),
            nudge_x = 50,
            nudge_y = 0.04) +
  theme_bw()+
  scale_color_viridis(option = "magma")+
  labs(x = "Rank", 
       y = "Value", 
       color = "Pop (log)") -> p_1

p_table %>%
filter(!is.na(pop_tot)) %>% 
  ggplot(., aes(x = val2_rk,
                y = val2,
                color = log(pop_tot, base = 10)))+
  geom_point()+
  geom_label_repel(data = p_table[p_table$wuhan == T, ],
                   aes(x = val2_rk,
                       y = val2,
                       label = c("Wuhan")),
                   force = 50,
                   color = "black",
                   direction = "both",
                   arrow = arrow(length = unit(0.01, "npc"),
                                 type = "closed"),
                   nudge_x = 50,
                   nudge_y = 1) +
  theme_bw()+
  scale_color_viridis(option = "magma")+
  labs(x = "Rank", 
       y = "Value", 
       color = "Pop (log)") -> p_2

ggarrange(p_1, p_2,
          common.legend = T,
          labels = c("a", "b"),
          legend = "right")

ggsave(filename = "var_rank.png",
       width = 10,
       height = 4)

ggsave(filename = "var_rank.pdf",
       width = 10,
       height = 4,
       useDingbats=FALSE)

# cor.test(x = p_table$pop_tot,
#          y = p_table$val2,
#          method = "pearson",
#          use = "complete.obs") -> res
# res

# cases %>% 
#   filter(cases_tot != 0) %>% 
#   filter(CNTY_CODE != "420100") %>% 
#   #pull(CNTY_CODE) %>% unique %>% length
#   # mutate(prv = substr(CNTY_CODE,1,2)) %>% 
#   # filter(prv != 42) %>% 
#   group_by(CNTY_CODE) %>% 
#   mutate(date_rank = rank(date)) %>% 
#   filter(date_rank == 1) #%>% 
#   mutate(date_diff = date - as.Date("2020-01-23")) %>% 
#   pull(date_diff) %>% 
#   quantile(., c(0, 0.25, 0.75, 1))

# cases %>% 
#   filter(cases_tot != 0) %>% 
#   group_by(CNTY_CODE) %>% 
#   mutate(cases_rk = rank(date)) %>% 
#   filter(cases_rk == 1) %>% 
#   full_join(p_table, by = "CNTY_CODE") #-> tmp 
# 
# cor.test(as.numeric(tmp$date), tmp$val1, method = "spearman")
# cor.test(as.numeric(tmp$date), tmp$val2, method = "spearman")
