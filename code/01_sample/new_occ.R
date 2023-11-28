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

start_log_file("log/new_occ")

# Build Sample -----------------------------------------------------------------

year <- 1880
sub_sample <- ""

wd <- '~/Documents/projects/census_neighbor/data/'
wd <- '~/liran/census_neighbor/data/'

dt <- fread(paste0(wd, "census_raw/ipums_", year, sub_sample, 
                   ".csv")) %>% 
  setnames(tolower(names(.)))

# Crosswalk
year1 <- 1880
year2 <- 1900
method <- "abe_nysiis_standard"
xwalk <- fread(paste0(wd, "crosswalks/crosswalk_", year1, "_", year2,
                      ".csv")) %>%
  .[get(method) == 1] %>%
  .[, paste0("histid_", c(year1, year2)), with = FALSE]

dt %<>% 
  .[, histid := tolower(histid)]

# Define page number
dt %<>% 
  .[, lag_line := c(1, line[-.N])] %>% 
  .[, lag_larger := ifelse(lag_line > line, 1, 0)] %>% 
  .[1, lag_larger := 1] %>% 
  .[, page_num := ave(lag_larger, reel, microseq, FUN = cumsum)] %>% 
  .[, reel_seq_page := paste(reel, microseq, page_num, sep = "_")] %>% 
  .[, reel_seq_page_line := paste(reel_seq_page, line, sep = "_")] %>% 
  .[, `:=`(lag_line = NULL, lag_larger = NULL)] %>%
  .[, hh_line := c(1, rep(0, .N - 1)), by = serial] %>% 
  .[, hh_line := ave(hh_line, reel_seq_page, FUN = cumsum)]

dt %<>% 
  .[, occ1950 := str_pad(occ1950, 3, pad = "0")]

top_occ <- fread(paste0(wd, "cleaned/top_occ_with_labs.csv")) %>% 
  .[, occ1950 := str_pad(occ1950, 3, pad = "0")]

occ_codes <- top_occ[sample_include == 1, occ1950]


# Doctors, teachers, lawyers, clergymen, carpenters, blacksmith
# occ_codes <- c("075", "093", '055', '009', '510', "501")
for (occ in occ_codes) {
  print(occ)
  hh_sample <- dt %>% 
    .[, occ_ind := ifelse(occ1950 == occ, 1, 0)] %>% 
    .[, .(occ_ind = max(occ_ind)), by = .(serial, reel_seq_page, hh_line)] %>%
    .[, reel := str_split_fixed(reel_seq_page, "_", 3)[, 1]] %>% 
    .[, seq := str_split_fixed(reel_seq_page, "_", 3)[, 2]] %>% 
    .[, page := str_split_fixed(reel_seq_page, "_", 3)[, 3]] %>% 
    .[, .(serial, reel, seq, page, hh_line, occ_ind)] %>%
    .[,  occ_page := sum(occ_ind), by = .(reel, seq, page)]
  
  occ_dist <- hh_sample %>% 
    .[occ_page == 1] %>% 
    .[, occ_line := ifelse(occ_ind == 1, hh_line, 0)] %>%
    .[, occ_line := max(occ_line), by = .(reel, seq, page)] %>%
    .[, max_line := max(hh_line), by = .(reel, seq, page)] %>% 
    .[, max_dist := min(occ_line - 1, max_line - occ_line - 1), 
      by = .(reel, seq, page)] %>% 
    .[, occ_dist := abs(occ_line - hh_line)] %>% 
    .[occ_dist <= max_dist, ] %>% 
    .[occ_dist != 0] %>%
    .[occ_dist <= 10] %>% 
    .[, obs_page := .N, by = .(reel, seq, page)] %>% 
    .[, max_occ_dist := max(occ_dist), by = .(reel, seq, page)] %>% 
    .[obs_page == max_occ_dist * 2, ] %>% 
    .[, .(serial, occ_dist, occ_line)]
  
  sample <- dt %>%
    merge(occ_dist, by = "serial") %>% 
    .[, .(histid,  year, serial, reel_seq_page, hh_line, pernum, 
          occ_dist, occ_line, sex, age, race, nativity, school, lit,
          relate, occscore, erscor50)]
  
  sample %<>% 
    merge(xwalk, by.x = "histid", by.y = paste0("histid_", 1880), all.x = T) %>% 
    .[, match := ifelse(!is.na(histid_1900), 1, 0)]
  
  message(round(mean(sample[sex == 1, match]), 3) * 100, '% sample match')
  
  sample %<>% 
    .[, male_child := ifelse(age <= 18 & sex == 1, 1, 0)] %>% 
    .[, match_male_child := ifelse(male_child == 1 & match == 1, 1, 0)] %>%
    .[, male_child_hh := max(male_child), by = serial] %>% 
    .[, match_male_child_hh := max(match_male_child), by = serial] %>% 
    .[male_child_hh == 1]
  
  message(uniqueN(sample$serial), ' HH with male children')
  
  message(uniqueN(sample[match_male_child_hh == 1, serial]), 
          ' HH with matched male children')
  
  message(sum(sample$match_male_child), ' matched male children')
  
  sample %<>% 
    .[order(serial, pernum)]
  
  
  # Export
  fwrite(sample, paste0(wd, "cleaned/new_occ_", occ, "_sample_", year, 
                        sub_sample, ".csv"))
}

end_log_file()