reduce2_ntile <- function(connect, pop, n){
  require(magrittr)
  pop_tmp <- pop %>% mutate(nt = ntile(pop_tot, n))
  dates <- connect$date
  res_tmp <- list()
  for(i in 1:length(dates)){
    connect$connect[[i]] %>% 
      pivot_longer(cols = `110100`:`659009`,
                   names_to = "to") %>% 
      mutate(from = as.character(from)) %>% 
      left_join(pop_tmp, by = c("from" = "CNTY_CODE")) %>% 
      left_join(pop_tmp, by = c("to" = "CNTY_CODE")) %>% 
      rename(nt.from = nt.x,
             nt.to = nt.y) %>%
      filter(!is.na(nt.from),
             !is.na(nt.to)) %>% 
      dplyr::select(-pop_tot.x,
                    -pop_tot.y) %>% 
      mutate(date = dates[i]) -> res_tmp[[i]]
  }
  res_tmp %>% 
    bind_rows() %>% 
    mutate(date_diff = (date - as.Date("2020-01-01") + 1) %>% as.numeric,
           wk = ceiling(date_diff/7)) %>% 
    # group_by(from,
    #          to,
    #          wk,
    #          nt.from,
    #          nt.to) %>% 
    # summarise(value = sum(value)) %>% 
    filter(from != to) -> res
  return(res)
}