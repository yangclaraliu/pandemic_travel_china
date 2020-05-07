library(mapview)
library(tidyverse)
library(sf)
library(magrittr)
library(cowplot)
library(ggpubr)
library(viridis)

# create paths
user_name <- Sys.info()["user"]

epic_path_win <- paste0("C:/Users/",
                        user_name,
                        "/Documents/EPIC/")
code_path_win <- paste0("C:/Users/",
                        user_name,
                        "/Documents/GitHub/Local_2019-nCoV/")
# load data
paste0(epic_path_win,
       "[Data] China/Different Versions of Administrative Boundary/NGMC_2014_INUSE/china_shp.rds") %>% 
  read_rds() %>% 
  filter(lvl == "prf" | grepl("Hancheng",PYNAME,.,ignore.case = T)) -> shp_chn

load("~/EPIC/[Data] China/Hospital/hospital.rdata")

paste0("C:/Users/",
       user_name,
       "/Dropbox/nCov-2019/data_sources/mobility_data/china_prf_connectivity_0101_0301.rds") %>% 
  read_rds -> connect

load(paste0(code_path_win,"Baidu/lineplots_20200212_20200302.rdata"))
pop <- readRDS("~/EPIC/[Data] China/Pop_all/CDC_pop_res_prf_2018.rds")

cases <- readRDS("C:/Users/eideyliu/Dropbox/nCov-2019/data_sources/case_data/all_confirmed_prf_20200326.rds")

# load functions
paste0(code_path_win,"Baidu/extract_ts.R") %>% source
paste0(code_path_win,"Baidu/reduce2_ntile.R") %>% source

