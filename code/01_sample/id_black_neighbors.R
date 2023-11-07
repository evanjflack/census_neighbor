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

start_log_file("log/id_black_neighbors")

# Build Sample -----------------------------------------------------------------

year <- 1880
sub_sample <- "_ny"

wd <- '~/Documents/projects/census_neighbor/data/'
# wd <- '~/liran/census_neighbor/data/'

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

# Only standard households (no group quarters)
dt %<>% 
  .[gq %in% c(1, 2)]

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

hh_sample <- dt %>% 
  .[, black := ifelse(race == 2, 1, 0)] %>%
  .[, white := ifelse(race == 1, 1, 0)] %>%
  .[, .(black = mean(black), white = mean(white)), 
    by = .(serial, reel_seq_page, hh_line)] %>% 
  .[, black := ifelse(black == 1, 1, 0)] %>% 
  .[, white := ifelse(white == 1, 1, 0)] %>% 
  .[, reel := str_split_fixed(reel_seq_page, "_", 3)[, 1]] %>% 
  .[, seq := str_split_fixed(reel_seq_page, "_", 3)[, 2]] %>% 
  .[, page := str_split_fixed(reel_seq_page, "_", 3)[, 3]] %>% 
  .[, ord := seq(1, .N), by = .(reel, seq)] %>% 
  .[, reel_seq := paste(reel, seq, sep = "_")]

black_hh <- hh_sample %>% 
  .[black == 1, ] %>% 
  .[, .(reel_seq, ord)] %>% 
  setnames('ord', 'black_ord')

white_hh <- hh_sample %>% 
  .[white == 1, ] %>% 
  .[, .(reel_seq, ord)]

vars <- unique(hh_sample$reel_seq)
black_dist <- data.table()
for (i in vars) {
  black_dist1 <- white_hh[reel_seq == i] %>% 
    merge(black_hh[reel_seq == i], on = 'reel_seq', allow.cartesian = T) %>% 
    .[, dist := abs(ord - black_ord)]  %>% 
    .[dist <= 10]
  
  if (nrow(black_dist1) > 0) {
    black_dist1 %<>% 
      .[, .(black_dist = min(dist)), by = .(reel_seq, ord)]
    black_dist %<>% rbind(black_dist1)
  }
}

black_dist  %<>%
  merge(hh_sample[, .(serial, reel_seq, ord)], 
        by = c("reel_seq", "ord"))  %>% 
  .[, .(serial, black_dist)]

sample <- dt %>%
  merge(black_dist, by = "serial") %>% 
 .[, .(histid,  year, serial, reel_seq_page, hh_line, pernum, 
        black_dist, sex, age, race, nativity, school, lit,
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

# Export -----------------------------------------------------------------------

fwrite(sample, paste0(wd, "cleaned/black_neighbor_sample_", year, 
                      sub_sample, ".csv"))

end_log_file()