# Header -----------------------------------------------------------------------
# Proj: Census Neighbor
# Author: Evan Flack (flack@stanford.edu)
# Desc:

# Read in Packages/Functions ---------------------------------------------------
library(tictoc)
suppressMessages(library(data.table, quietly = T))
library(magrittr)
source("../supporting_code/define_fxns.R")

start_log_file("log/id_black_neighbors")

# Build Sample -----------------------------------------------------------------

year <- 1880
sub_sample <- ""

dt <- fread(paste0("../../data/census_raw/ipums_", year, sub_sample, 
                   ".csv")) %>% 
  setnames(tolower(names(.)))

names(dt)

# Crosswalk
year1 <- 1880
year2 <- 1900
method <- "abe_nysiis_standard"
xwalk <- fread(paste0("../../data/crosswalks/crosswalk_", year1, "_", year2, 
                      ".csv")) %>%
  .[get(method) == 1] %>%
  .[, paste0("histid_", c(year1, year2)), with = FALSE]


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
  .[, `:=`(lag_line = NULL, lag_larger = NULL)]

dt %<>% 
  .[, hh_line := c(1, rep(0, .N - 1)), by = serial] %>% 
  .[, hh_line := ave(hh_line, reel_seq_page, FUN = cumsum)]

# Location of black households
 black_loc <- dt %>% 
   .[race == 2, ] %>% 
   .[, .(reel_seq_page, hh_line)] %>% 
   unique() %>%
   setnames("hh_line", "black_line")

 # Distance from closest black hh
 black_dist <- dt  %>%
   .[, white := ifelse(race == 1, 1, 0)] %>% 
   .[, hh_white := mean(white), by = serial] %>% 
   .[hh_white == 1, ] %>%
   .[, .(serial, reel_seq_page, hh_line)] %>% 
   unique() %>%
   merge(black_loc, by = "reel_seq_page", allow.cartesian = T) %>%
   .[, .(serial, hh_line, black_line)] %>%
   .[, black_dist := abs(hh_line - black_line)] %>% 
   .[order(serial, black_dist)] %>%
   .[black_dist <= 10, ] %>% 
   .[, .(serial, black_line, black_dist)]
 

 # White individuals within 10 lines of a black hh
sample <- dt %>%
  merge(black_dist, by = "serial") %>%
 .[, .(histid,  year, serial, reel_seq_page, hh_line, pernum, 
       black_line, black_dist, sex, age, race, nativity, school, lit,
       relate, occscore, erscor50)] 
 
 
sample %<>% 
   .[, histid := tolower(histid)] %>% 
   merge(xwalk, by.x = "histid", by.y = paste0("histid_", 1880), all.x = T) %>% 
   .[, match := ifelse(!is.na(histid_1900), 1, 0)]
 
sample %<>% 
   .[, male_child := ifelse(age <= 18 & sex == 1, 1, 0)] %>% 
   .[, match_male_child := ifelse(male_child == 1 & match == 1, 1, 0)] %>%
   .[, male_child_hh := max(male_child), by = serial] %>% 
   .[, match_male_child_hh := max(match_male_child), by = serial] %>% 
   .[male_child_hh == 1, ]
  

# Export -----------------------------------------------------------------------

fwrite(sample, paste0("../../data/cleaned/black_neighbor_sample_", year, 
                      sub_sample, ".csv"))

end_log_file()