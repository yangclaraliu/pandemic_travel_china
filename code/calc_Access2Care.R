hosp_loc <- st_transform(hosp_loc, crs = st_crs(shp_chn))
intersects <- st_intersects(shp_chn, hosp_loc)
intersects %>% 
  map(length) %>% 
  unlist %>% 
  enframe %>% 
  mutate(CNTY_CODE = shp_chn$CNTY_CODE) %>% 
  arrange(CNTY_CODE) %>% 
  rename(hosp_no = value) %>% 
  dplyr::select(-name) %>% 
  left_join(pop, by = "CNTY_CODE") %>%
  mutate(hosp_pc = hosp_no*100000/pop_tot,
         hosp_pc_nt = ntile(hosp_pc,2)) -> hosp_cts
# 
# hosp_cts %>% 
#   ggplot(., aes(x = pop_tot,
#                 y = hosp_pc))+
#   geom_point()

quantile(hosp_cts$hosp_pc, c(0, 0.5, 1), na.rm = T)

move_in_history %>% 
  bind_rows() %>% 
  distinct() %>% 
  bind_rows(move_out_history %>% 
              bind_rows() %>% 
              distinct()) %>%
  mutate(date = lubridate::ymd(date)) %>% 
  filter(date >= "2020-01-01") %>% 
  as_tibble %>%
  mutate(CNTY_CODE = as.character(CNTY_CODE),
         CNTY_CODE = case_when(CNTY_CODE == "110000" ~ "110100",
                               CNTY_CODE == "310000" ~ "310100",
                               CNTY_CODE == "500000" ~ "500100",
                               CNTY_CODE == "120000" ~ "120100",
                               TRUE ~ CNTY_CODE),
         value = unlist(value)) %>% 
  distinct() -> all_history

all_history$date %>% unique %>% sort -> date_labels

all_history %>% 
  mutate(date_diff = date - as.Date("2020-01-01") + 1,
         date = as.Date(date, origin = "1960-10-01"),
         value = as.numeric(value),
         wk = ceiling(date_diff/7) %>% as.integer()) %>% 
  group_by(CNTY_CODE, wk, type) %>% 
  #summarise(flux = sum(value))%>% 
  pivot_wider(names_from = type,
              values_from = value) %>% 
  ungroup %>% 
  mutate(flux_diff = history_in - history_out,
         date_diff = as.numeric(date_diff),
         date = as.character(date),
         phase = if_else(date <= "2020-01-25", "a. Pre-LNY", "b. Post-LNY"),
         phase = factor(phase, levels = c("a. Pre-LNY", "b. Post-LNY"))) %>% 
  #wk = factor(wk, levels = 1:9), 
  #order = T) %>% 
  left_join(hosp_cts, by = "CNTY_CODE") %>% 
  filter(date <= "2020-03-01",
         wk < 10) %>% 
  mutate(wk = factor(wk, levels = 1:9)) %>% 
  ggplot(., aes(x = hosp_pc, 
                y = flux_diff)) +
                #color = phase))+
  geom_point(alpha = 0.1) +
 # geom_line(alpha = 0) +
  # stat_smooth(geom = "line",
  #             aes(group = phase,
  #                 color = phase),
  #             alpha = 0.5,
  #             size = 1,
  #             method = "lm",
  #             se = T)+
  # scale_color_viridis(discrete = T) +
  # scale_color_viridis(breaks = seq(1,61,20),
  #                     labels = date_labels[seq(1,61,20)]) +
  theme_bw() + 
  facet_wrap(~phase) +
  geom_hline(yintercept = 0) +
  labs(x = "Number of Hospitals per 100,000",
       y = "Net Change by Migration Index") +
  scale_x_log10() +
  theme(strip.background = element_rect(fill = NA),
        panel.grid = element_blank(),
        strip.text = element_text(size = 12)) +
  geom_vline(xintercept = 0.37) -> p_hosp
  # geom_text(data = data.frame(x = rep(0.02,2),
  #                             y = rep(8,2),
  #                             phase = c("Pre-LNY",
  #                                       "Post-LNY")),
  #           aes(x = x, 
  #               y = y, 
  #               label = c("a", "b")))-> p_hosp
  #guides(colour = guide_legend(override.aes = list(alpha=1))) 

p_hosp

cases %>% 
  mutate(inc = c(NA, diff(cases_tot)),
         #inc = if_else(inc <= 0, 0, inc),
         inc = if_else(is.na(inc), 0, inc),
         date_diff = date-as.Date("2020-01-01"),
         wk = (floor(date_diff/7)+1) %>% as.numeric) %>% 
  group_by(CNTY_CODE, wk) %>% 
  mutate(rk = rank(date)) %>% 
  filter(rk == 1 & CNTY_CODE == "110100") %>% 
  pull(date) %>% 
  .[1:7] %>% 
  gsub("2020-","",.)-> labels_date
  
cases %>% 
  mutate(inc = c(NA, diff(cases_tot)),
         #inc = if_else(inc <= 0, 0, inc),
         inc = if_else(is.na(inc), 0, inc),
         date_diff = date-as.Date("2020-01-01"),
         wk = (floor(date_diff/7)+1) %>% as.numeric) %>%
  group_by(CNTY_CODE, wk) %>% 
  summarise(inc = sum(inc)) %>% 
  filter(wk <= 10) %>% 
  full_join(hosp_cts, by = "CNTY_CODE") %>% 
  filter(CNTY_CODE !=  "420100",
         !is.na(hosp_pc_nt)) %>%
  mutate(hosp_pc = if_else(hosp_pc == 0, 0.0013, hosp_pc),
         pressure = inc/hosp_pc,
         hosp_pc_nt = factor(hosp_pc_nt,
                             levels = c(1:2),
                             labels = c("c. Low Access to Healthcare Setting",
                                        "d. High Access to Healthcare Setting"))) %>% 
  group_by(hosp_pc_nt,
           wk) %>%
  # filter(pressure != 0) %>% 
  # summarise(med = median(pressure),
  #           mean = mean(pressure)) %>% 
  # pivot_longer(cols = starts_with("m")) %>% 
  ungroup %>% 
  mutate(hosp_pc_nt = factor(hosp_pc_nt)) -> p_table 

get_p <- function(x){
  wilcox.test(p_table %>% 
           filter(wk == x,
                  hosp_pc_nt == "c. Low Access to Healthcare Setting") %>% 
           pull(pressure),
         p_table %>% 
           filter(wk == x,
                  hosp_pc_nt == "d. High Access to Healthcare Setting") %>% 
           pull(pressure))$p.value %>% return()
}

res <- rep(NA, 7)
for(i in 3:9) res[i-2] <- get_p(i)
wk_range = range(p_table$wk, na.rm = T)
vline.level <- wk_range[which(wk_range == 4.5)]

p_table %>% 
  filter(!is.na(wk),
         wk < 10) %>% 
  left_join(data.frame(wk = 3:9, status = !(res>0.05)),
            by = "wk") %>% 
  ggplot(., aes(x = factor(wk),
              y = pressure,
              color = status)) +
  geom_boxplot()+
  scale_y_log10()+
  facet_wrap(~hosp_pc_nt)+
#   
# geom_smooth(alpha = 0.1, 
#               se = F,
#               linetype = 2) +
  # scale_color_manual(values = c(`1` = "black",
  #                           `2` = "darkgrey"))+
  # scale_fill_manual(values = c(`1` = "black",
  #                               `2` = "darkgrey"),
  #                   labeller(labels = c("A", "b")))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        strip.background = element_rect(NA),
        strip.text = element_text(size = 12))+
  scale_color_manual(values = c(`TRUE` = "black",
                                `FALSE` = "darkgrey"))+
  labs(x = "Week in 2020 and Week Start Date",
       y = "Healthcare Pressure")+
  geom_vline(xintercept = 2.5)+
  geom_label(aes(x = 6, y = 5000, 
                 label = "Post-LNY"), 
             color = "black")+
  geom_label(aes(x = 1.2, y = 5000, 
                 label = "Pre-LNY"),
             color = "black")+
  scale_x_discrete(labels = paste0(3:9, "\n",labels_date))-> p_pressure

p_pressure



#   
# hosp_cts %>% 
#   ggplot(., aes(x = log(pop_tot, base = 10),
#                 y = hosp_pc))+
#   geom_point()+
#   geom_hline(yintercept = 0.38)
# 
#   
#   # group_by(hosp_pc_nt, date) %>% 
#   # summarise(inc = sum(inc),
#   #           pop = sum(pop_tot),
#   #           hosp = sum(hosp_no)) %>% 
#   ggplot(., aes(x = date, 
#                 y = inc,
#                 color = log(hosp_pc))) +
#   geom_point() -> p_ressure

# 
# hosp_cts %>% 
#   group_by(hosp_pc_nt) %>% 
#   summarise(pop = sum(pop_tot, na.rm = T),
#             hosp_no = sum(hosp_no))
# 
# cases %>% 
#   filter(CNTY_CODE != "420100",
#          date == "2020-03-01") %>% 
#   left_join(hosp_cts, by = "CNTY_CODE") %>% 
#   group_by(hosp_pc_nt) %>% 
#   summarise(pop = sum(pop_tot),
#             cases = sum(cases_tot))
#   
# 
# left_join(hosp_cts %>%
#               group_by(hosp_pc_nt) %>%
#               summarise(pop = sum(pop_tot, na.rm = T)) %>%
#               filter(!is.na(hosp_pc_nt)),
#             by = "hosp_pc_nt") %>%
#   
#   
#     %>% 
#   group_by(CNTY_CODE) %>% 
#   %>%
#   #group_by(hosp_pc_nt, date) %>% 
#   # summarise(inc = sum(inc, na.rm = T)) %>% 
# 
#   # ungroup %>% 
#   # mutate(hosp_pc_nt = factor(hosp_pc_nt),
#   #        r_inc = inc/pop) %>% 
#   ggplot(., aes(x = hosp_pc, 
#                 y = inc,
#                 color = hosp_pc))+
#   geom_point()+
#     scale_y_log10()
#   
# ggplot(., aes(x = date, 
#                 y = inc,
#                 fill = hosp_pc_nt))+
#   geom_bar(stat = "identity",
#            position = "stack")
#   
# #group_by(hosp_pc_nt, date, threshold) %>% 
#   #summarise(inc = sum(inc, na.rm = T)) %>% 
#   filter(!is.na(hosp_pc_nt)) %>% 
#   #,
#          # abundant = factor(abundant,
#          #                   levels = c("poor", "abundant"),
#          #                   labels = c("< 30 Hospitals",
#          #                              ">= 30 hospitals"))) %>% 
#   # filter(abundant == "abundant",
#   #        inc > 150)
#   ggplot(., aes(x = date,
#                 y = inc,
#                 color = hosp_pc_nt,
#                 group = CNTY_CODE))+
#   geom_point(alpha = 0.5)+
#   geom_line(alpha = 0.5)+
#   facet_wrap(~hosp_pc_nt,
#              nrow = 1) #+
#   scale_y_log10()
# 
#   
#   geom_point(alpha = 0.5) +
#   geom_line(alpha = 0.5) +
#   theme_bw() +
#   #scale_color_grey()+
#     #scale_color_viridis() +
#   facet_wrap(~hosp_pc_nt) +
#   labs(y = "Daily Incidence",
#        x = "Date")# -> p_cases
# 
# pop %>% 
#   mutate(nt = ntile(pop_tot, 4)) %>% 
#   right_join(hosp_cts, by = "CNTY_CODE") %>%
#   group_by(nt) %>% 
#   filter(nt == 1) %>% 
#   pull(hosp_no) %>% 
#   table
#   
#   summarise(n = sum(hosp_no),
#             pop = sum(pop_tot),
#             hosp = sum(hosp_no)) %>% 
#   mutate(x = sum(n),
#          pop_tot = sum(pop, na.rm = T),
#          r1 = pop/pop_tot,
#          r2 = n/x)
# 
#   
# 
# 
# 
# 
# 
# group_by(abundant) %>% 
#   summarise(n  = sum(hosp_no))
# 

ggarrange(p_hosp,
          p_pressure, 
          ncol = 1,
          align = "v") 

ggsave(filename = "healthcare.png",
       #paste0(gibbs_path_win,"Yang/cases_hosp.png"),
       width = 10,
       height = 10)


#   group_by(CNTY_CODE) %>% 
#   mutate(inc = c(NA, diff(cases_tot, 1)),
#          prv = substr(CNTY_CODE, 1, 2),
#          hubei = if_else(prv == 42, T, F)) %>% 
#   filter(CNTY_CODE != "420100",
#          date <= "2020-03-01") %>% 
#   # filter(date >= "2020-02-15",
#   #        inc >= 150)
#   ggplot(., aes(x = date, 
#                 y = inc,
#                 group = CNTY_CODE,
#                 color = hubei))+
#   geom_point(alpha = 0.5) +
#   geom_line() +
#   facet_grid(hubei~abundant)

# hosp_cts %>% 
#   left_join(pop, by = "CNTY_CODE") %>% 
#   left_join(cases, by = "CNTY_CODE") %>% 
#   group_by(CNTY_CODE) %>% 
#   mutate(inc = c(NA, diff(cases_tot, 1)),
#          prv = substr(CNTY_CODE, 1, 2),
#          hubei = if_else(prv == 42, "hubei", "non-hubei")) %>% 
#   filter(CNTY_CODE != "420100") %>%
#   group_by(abundant, date, hubei) %>%
#   summarise(inc = mean(inc, na.rm = T),
#             pop_tot = mean(pop_tot, na.rm = T),
#             n = n(),
#             r = inc*100000/pop_tot) %>% 
#   ggplot(., aes(x = date, 
#                 y = r,
#                 group = abundant,
#                 color = abundant))+
#   geom_line()+
#   geom_point()+
#   facet_wrap(~hubei,
#              scales = "free")
  
  
  # 
  # filter(date == "2020-01-25")
  # 
  # mutate(r = inc*100000/pop_tot) %>%
  # ggplot(., aes(x = date,
  #               y = r,
  #               group = CNTY_CODE))+
  # geom_point()+
  # facet_grid(hubei~abundant)+
  # scale_y_log10()
  




# ggplot(., aes(x = date, 
#                 y = cases_tot,
#                 group = CNTY_CODE,
#                 color = log(pop_tot, base = 10))) +
#   geom_point() +
#   geom_line() +
#   facet_grid(hubei~abundant)









cases %>% 
  mutate(phase = if_else(date <= "2020-01-25", "Pre-LNY", "Post-LNY"),
         phase = factor(phase, levels = c("Pre-LNY", "Post-LNY"))) %>% 
  filter(CNTY_CODE != "420100") %>% 
  group_by(CNTY_CODE) %>% 
  mutate(inc = c(NA, diff(cases_tot, 1))) %>% 
  filter(date <= "2020-03-01") %>% 
  ggplot(., aes(x = date, 
                y = inc,
                group = CNTY_CODE)) +
  geom_line() +
  geom_point() +
  facet_wrap(~phase)


# all_history %>% 
#   mutate(date_diff = date - as.Date("2020-01-01") + 1,
#          value = as.numeric(value),
#          wk = ceiling(date_diff/7) %>% as.numeric) %>% 
#   group_by(CNTY_CODE, wk, type) %>% 
#   summarise(flux = sum(value))%>% 
#   pivot_wider(names_from = type,
#               values_from = flux) %>% 
#   ungroup %>% 
#   filter(wk < 10) %>% 
#   mutate(flux_diff = history_in - history_out,
#          wk = factor(wk, 
#                      levels = 1:9,
#                      order = T)) %>% 
#   left_join(hosp_cts, by = "CNTY_CODE") %>%
#   filter(!is.na(hosp_no)) %>% 
#   mutate(abundant = if_else(hosp_no >= 30,
#                             T,
#                             F)) %>% 
#   dplyr::select(-flux_diff) %>% 
#   pivot_longer(cols = starts_with("history")) %>% 
#   ggplot(., aes(x = wk,
#                 y = value,
#                 group = interaction(CNTY_CODE,name)))+
#   geom_line()+
#   geom_point()+
#   facet_grid(name ~ abundant)+
#   cowplot::theme_cowplot()


# health_diff <- list()
# for(t in 1:68){
#   all_history %>% 
#     mutate(date_diff = date - as.Date("2020-01-01") + 1,
#            value = as.numeric(value)) %>% 
#     filter(date_diff <= t) %>% 
#     group_by(CNTY_CODE, type) %>% 
#     summarise(flux = sum(value, na.rm = T)) %>% 
#     pivot_wider(names_from = type,
#                 values_from = flux) %>% 
#     mutate(flux_diff = history_in - history_out) %>% 
#     left_join(hosp_cts, by = "CNTY_CODE") -> health_diff[[t]]
# }
# 
# 
# health_diff %>% 
#   setNames(paste0("day",1:68)) %>% 
#   bind_rows(.id = "day") %>% 
#   mutate(day = gsub("day","",day),
#          day = as.numeric(day)) %>% 
#   group_by(day) %>%
#   mutate(day_range = ceiling(day/10)) %>% 
#   ggplot(., aes(x = hosp_no,
#                 y = flux_diff,
#                 color = day))+
#   geom_point(alpha = 0.5)+
#   geom_smooth(aes(group = day,
#                   color = day,
#                   fill = day),
#               alpha = 0.2)+
#   theme_bw()+
#   scale_color_viridis()+
#   scale_fill_viridis()+
#   facet_wrap(~day_range)+
#   geom_hline(yintercept = 0) -> p_1
