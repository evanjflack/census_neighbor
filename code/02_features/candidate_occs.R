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

start_log_file("log/candidate_occs")

# Build Sample -----------------------------------------------------------------

year <- 1880
sub_sample <- ""

# wd <- '~/Documents/projects/census_neighbor/data/'
wd <- '~/liran/census_neighbor/data/'

dt <- fread(paste0(wd, "census_raw/ipums_", year, sub_sample, 
                   ".csv")) %>% 
  setnames(tolower(names(.))) %>% 
  .[, .(sex, occ1950)] %>% 
  .[sex == 1, ] %>% 
  .[, occ1950 := str_pad(occ1950, 3, pad = "0")]

top_occ <- dt %>% 
  .[, .(.N), by = occ1950] %>% 
  .[order(-N)]

fwrite(top_occ, paste0(wd, "cleaned/top_occ.csv"))

end_log_file()