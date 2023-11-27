# Header -----------------------------------------------------------------------
# Proj: Census Neighbor
# Author: Evan Flack (flack@stanford.edu)
# Desc:

# Read in Packages/Functions ---------------------------------------------------
library(tictoc)
suppressMessages(library(data.table, quietly = T))
library(magrittr)
library(stringr)
source("../supporting_code/define_fxns.R")

# wd <- '~/Documents/projects/census_neighbor/data/'
wd <- '~/liran/census_neighbor/data/'

year1 <- 1880
year2 <- 1900
sub_sample <- ""
occ_codes <- c("075", "093")

start_log_file("log/occ_outcomes")

dt <- fread(paste0(wd, "census_raw/ipums_", year2, sub_sample, 
                   ".csv")) %>% 
  setnames(tolower(names(.))) %>% 
  .[, histid := tolower(histid)]

for (occ in occ_codes) {
  print(occ)
  pre_sample <- fread(paste0(wd, "cleaned/new_occ_", occ, "_sample_", year1, 
                             sub_sample, ".csv")) %>% 
    .[match_male_child == 1, ] %>% 
    .[, .(histid_1900, occ_dist, reel_seq_page)] %>% 
    .[, histid_1900 := tolower(histid_1900)] %>% 
    setnames('histid_1900', 'histid')
  
  
  
  post_sample <- dt %>% 
    merge(pre_sample, by = "histid")
  
  post_sample %<>% 
    .[, occ1950 := str_pad(occ1950, 3, pad = "0")] %>% 
    .[, y := ifelse(occ1950 == occ, 1, 0)]
  
  names(post_sample)
  
  post_sample %<>% 
    .[, .(histid, serial, y, occ_dist, reel_seq_page, age, race, statefip)]
  
  fwrite(post_sample, paste0(wd, "cleaned/occ_", occ, "_outcomes_", year2, 
                             sub_sample, ".csv"))
}

end_log_file()


