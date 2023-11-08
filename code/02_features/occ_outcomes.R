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
occ <- "075"
pre_sample <- fread(paste0(wd, "cleaned/occ_", occ, "_sample_", year, 
                           sub_sample, ".csv")) %>% 
  .[match_male_child == 1, ] %>% 
  .[, .(histid_1900, occ_dist, reel_seq_page)] %>% 
  .[, histid_1900 := tolower(histid_1900)] %>% 
  setnames('histid_1900', 'histid')


uniqueN(pre_sample$histid)


dt <- fread(paste0(wd, "census_raw/ipums_", 1900, sub_sample, 
                   ".csv")) %>% 
  setnames(tolower(names(.))) %>% 
  .[, histid := tolower(histid)]

uniqueN(dt$histid)

post_sample <- dt %>% 
  merge(pre_sample, by = "histid")

post_sample %<>% 
  .[, occ1950 := str_pad(occ1950, 3, pad = "0")] %>% 
  .[, y := ifelse(occ1950 == occ, 1, 0)]



  .[histid %chin% pre_sample$histid]

mean(pre_sample$histid %in% dt$histid)

dt$histid

dt %<>% 
  merge(pre_sample, by = 'histid')

post_sampe


dt %<>% 
  .[serial %in% post_sample$serial]

dt %<>% 
  .[, match := ifelse(histid %chin% pre_sample$histid, 1, 0)]

dt_hh <- dt 

head(pre_sample)
