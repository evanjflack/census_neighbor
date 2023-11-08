# Header -----------------------------------------------------------------------
# Proj: Census Neighbor
# Author: Evan Flack (flack@stanford.edu)
# Desc: Creates sample of people in close proximity to a doctor

# Read in Packages/Functions ---------------------------------------------------
library(tictoc)
suppressMessages(library(data.table, quietly = T))
library(magrittr)
library(stringr)
source("../supporting_code/define_fxns.R")

start_log_file("log/id_occ_samples")

wd <- '~/Documents/projects/census_neighbor/data/'
# wd <- '~/liran/census_neighbor/data/'

# Build Sample -----------------------------------------------------------------
year <- 1880
sub_sample <- "_ny"
# Doctors, teachers, lawyers, clergymen, carpenters, blacksmith
occ_codes <- c("075", "093", '055', '009', '510', "501")

message("Start ", year, ".")
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

# Prep Data --------------------------------------------------------------------
dt %<>% 
  .[gq %in% c(1, 2)]

dt %<>% 
  .[, histid := tolower(histid)] %>% 
  merge(xwalk, by.x = "histid", by.y = paste0("histid_", 1880), all.x = T) %>% 
  .[, match := ifelse(!is.na(histid_1900), 1, 0)]


# Define page number
dt %<>% 
  .[, lag_line := c(1, line[-.N])] %>% 
  .[, lag_larger := ifelse(lag_line > line, 1, 0)] %>% 
  .[1, lag_larger := 1] %>% 
  .[, page_num := ave(lag_larger, reel, microseq, FUN = cumsum)] %>% 
  .[, reel_seq_page := paste(reel, microseq, page_num, sep = "_")] %>% 
  .[, `:=`(lag_line = NULL, lag_larger = NULL)] %>%
  .[, hh_line := c(1, rep(0, .N - 1)), by = serial] %>% 
  .[, hh_line := ave(hh_line, page_num, FUN = cumsum)] %>% 
  .[, occ1950 := str_pad(occ1950, 3, pad = "0")]

# Identify HH within 10 lines of occupations
for (occ in occ_codes) {
  message(occ)
  
  hh_sample <- dt %>% 
    .[, paste0("ind_", occ) := ifelse(occ1950 == occ, 1, 0)] %>%
    .[, .(occ_house_ind = max(get(paste0("ind_", occ)))),
      by = .(serial, reel_seq_page, hh_line)] %>% 
    .[, reel := str_split_fixed(reel_seq_page, "_", 3)[, 1]] %>% 
    .[, seq := str_split_fixed(reel_seq_page, "_", 3)[, 2]] %>% 
    .[, page := str_split_fixed(reel_seq_page, "_", 3)[, 3]] %>% 
    .[order(reel, seq, page, hh_line)] %>%
    .[, ord := seq(1, .N), by = .(reel, seq)] %>% 
    .[, reel_seq := paste(reel, seq, sep = "_")]
  
  
  occ_hh <- hh_sample %>% 
    .[occ_house_ind == 1, ] %>% 
    .[, .(reel_seq, ord)] %>% 
    setnames('ord', 'occ_ord')
  
  non_occ_hh <- hh_sample %>% 
    .[occ_house_ind == 0, ] %>% 
    .[, .(reel_seq, ord)] 
  
  vars <- unique(hh_sample$reel_seq)
  occ_dist <- data.table()
  for (i in vars) {
    occ_dist1 <- non_occ_hh[reel_seq == i] %>%
      merge(occ_hh[reel_seq == i], on = 'reel_seq', allow.cartesian = T) %>% 
      .[, dist := abs(ord - occ_ord)]  %>% 
      .[dist <= 10]
    
    if (nrow(occ_dist1) > 0) {
      occ_dist1 %<>% 
        .[, .(occ_dist = min(dist)), by = .(reel_seq, ord)]
      occ_dist %<>% rbind(occ_dist1)
    }
  }
    
  occ_dist  %<>%
    merge(hh_sample[, .(serial, reel_seq, ord)], 
          by = c("reel_seq", "ord"))  %>% 
    .[, .(serial, occ_dist)]
  
  sample <- dt %>%
    merge(occ_dist, by = "serial") %>% 
    .[, .(histid,  year, serial, reel_seq_page, hh_line, pernum, 
          occ_dist, sex, age, race, nativity, school, lit,
          relate, occscore, erscor50)]
  
  sample %<>% 
    .[, histid := tolower(histid)] %>% 
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
  
  
  fwrite(sample, paste0(wd, "cleaned/occ_", occ, "_sample_", year, 
                        sub_sample, ".csv"))
  
  # rm(occ_sample, occ_dist, occ_loc, dt_occ)
  
}


message("End ", year, ".")

# End --------------------------------------------------------------------------

end_log_file()
