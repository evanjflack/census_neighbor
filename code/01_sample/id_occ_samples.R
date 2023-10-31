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

# wd <- '~/Documents/projects/census_neighbor/data/'
wd <- '~/liran/census_neighbor/data/'

# Build Sample -----------------------------------------------------------------
year <- 1880
sub_sample <- ""
occ_codes <- c("058", "075", "093")

message("Start ", year, ".")
dt <- fread(paste0(wd, "census_raw/ipums_", year, sub_sample, 
                   ".csv")) %>% 
  setnames(tolower(names(.)))

# Cro
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

# mean(dt[sex == 1, match])

# Define page number
dt %<>% 
  .[, lag_line := c(1, line[-.N])] %>% 
  .[, lag_larger := ifelse(lag_line > line, 1, 0)] %>% 
  .[1, lag_larger := 1] %>% 
  .[, page_num := ave(lag_larger, reel, microseq, FUN = cumsum)] %>% 
  .[, reel_seq_page := paste(reel, microseq, page_num, sep = "_")] %>% 
  .[, `:=`(lag_line = NULL, lag_larger = NULL)]

dt %<>% 
  .[, hh_line := c(1, rep(0, .N - 1)), by = serial] %>% 
  .[, hh_line := ave(hh_line, page_num, FUN = cumsum)]

dt %<>% 
  .[, occ1950 := str_pad(occ1950, 3, pad = "0")]

# Identify HH within 10 lines of occupations
for (occ in occ_codes) {
  message(occ)
  dt_occ <- dt %>% 
    .[, paste0("ind_", occ) := ifelse(occ1950 == occ, 1, 0)] %>%
    .[, occ_house_ind := max(get(paste0("ind_", occ))), by = serial] %>% 
    .[, num_occ_page := sum(get(paste0("ind_", occ))), by = reel_seq_page] %>% 
    .[num_occ_page > 0, ] %>% 
    .[, .(serial, reel_seq_page, hh_line, occ_house_ind)] %>% 
    .[, .SD[1], by = serial]
  
  occ_loc <- dt_occ[occ_house_ind == 1, .(reel_seq_page, hh_line)] %>% 
    setnames("hh_line", "occ_line") %>% 
    unique()
  
  occ_dist <- dt_occ %>%
    .[, .(serial, reel_seq_page, hh_line)] %>% 
    merge(occ_loc, by = "reel_seq_page", allow.cartesian = T) %>% 
    .[, occ_dist := abs(hh_line - occ_line)] %>%
    .[order(serial, occ_dist)] %>% 
    .[, .SD[1], by = serial] %>% 
    .[, .(serial, occ_dist)] %>% 
    .[occ_dist <= 10 & occ_dist != 0, ]
  
  occ_sample <- dt %>%
    merge(occ_dist, by = "serial") %>%
    .[, .(histid, histid_1900, year, serial, reel_seq_page, hh_line, pernum, 
          relate, occ_dist, sex, age, occ1950, occscore, erscor50, match)] 
  
  occ_sample %<>% 
    .[, male_child := ifelse(age <= 18 & sex == 1, 1, 0)] %>% 
    .[, match_male_child := ifelse(male_child == 1 & match == 1, 1, 0)] %>%
    .[, male_child_hh := max(male_child), by = serial] %>% 
    .[, match_male_child_hh := max(match_male_child), by = serial] %>% 
    .[male_child_hh == 1]
  
  message(uniqueN(occ_sample$serial), ' HH with male children')
  
  message(uniqueN(occ_sample[match_male_child_hh == 1, serial]), 
          ' HH with matched male children')
  
  message(sum(occ_sample$match_male_child), ' matched male children')
  
  fwrite(occ_sample, paste0(wd, "cleaned/occ_", occ, "_sample_", year, 
                            sub_sample, ".csv"))
  
  rm(occ_sample, occ_dist, occ_loc, dt_occ)
  
}


message("End ", year, ".")

# End --------------------------------------------------------------------------

end_log_file()
