# Header -----------------------------------------------------------------------
# Proj: Census Neighbor
# Author: Evan Flack (flack@stanford.edu)
# Desc:

# Read in Packages/Functions ---------------------------------------------------
library(tictoc)
suppressMessages(library(data.table, quietly = T))
library(magrittr)
library(ggplot2)
library(estimatr)
source("../supporting_code/define_fxns.R")

wd <- '~/Documents/projects/census_neighbor/data/'
# wd <- '~/liran/census_neighbor/data/'

# start_log_file("log/id_black_neighbors")

year <- 1880
sub_sample <- "_ny"
pre_sample <- fread(paste0(wd, "cleaned/black_neighbor_sample_", year, 
                           sub_sample, ".csv")) %>% 
  .[match_male_child == 1, ] %>% 
  .[, .(histid_1900)] %>% 
  .[, histid_1900 := tolower(histid_1900)] %>% 
  setnames('histid_1900', 'histid')

dt <- fread(paste0(wd, "census_raw/ipums_", 1900, sub_sample, 
                   ".csv")) %>% 
  setnames(tolower(names(.))) %>% 
  .[, histid := tolower(histid)]

post_sample <- dt %>% 
  .[histid %chin% pre_sample$histid]

post_sample %>%
  .[, .(.N), by = age] %>% 
  .[order(age)]

dt %<>% 
  .[serial %in% post_sample$serial]

dt %<>% 
  .[, match := ifelse(histid %chin% pre_sample$histid, 1, 0)]

dt_hh <- dt 

head(pre_sample)
