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

year1 <- 1880
year2 <- 1900
sub_sample <- ""
occ <- "093"

occ_codes <- c("093", "075")
occ_labs <- c("Teacher", "Doctor")

dt_fit <- data.table()
for (i in 1:length(occ_codes)) {
  print(occ_codes[i])
  
  DT_fit <- fread(paste0(wd, "cleaned/occ_", occ_codes[i], "_outcomes_", year2, 
                         sub_sample, ".csv"))
  
  DT_fit %<>% 
    .[, dm_y := y - mean(y), by = reel_seq_page] %>% 
    .[, dm_occ_dist := occ_dist - mean(occ_dist), by = reel_seq_page]

  
  fit <- lm_robust(dm_y ~ dm_occ_dist, data = DT_fit, se_type = 'stata')
  
  dt_fit1 <- tidy(fit) %>% 
    as.data.table() %>% 
    .[term == 'dm_occ_dist'] %>% 
    .[, mean := mean(DT_fit$y)] %>% 
    .[, perc := round(estimate / mean, 3) * 100] %>% 
    .[, `:=`(estimate = round(estimate * 100, 3), 
             std.error = round(std.error * 100, 3), 
             mean = round(mean * 100, 3))] %>% 
    .[, stars1 := ifelse(p.value <= .01, "***", ifelse(p.value <= .05, "**",
                                                       ifelse(p.value <= .1,
                                                              "*", "")))] %>%
    .[, estimate := paste0(estimate, stars1)] %>%
    .[, est_se := paste0("\\begin{tabular}{@{}c@{}}", estimate,
                         "\\\\ (", std.error,  ")\\end{tabular}")] %>% 
    .[, occ := occ_labs[i]] %>%
    .[, obs := nrow(DT_fit)] %>%
    .[, .(occ, obs, mean, est_se, perc)]
  
  dt_fit %<>% rbind(dt_fit1)
  
}

pre_sample <- fread(paste0(wd, "cleaned/new_occ_", occ_codes[i], "_sample_", year1, 
                           sub_sample, ".csv")) %>% 
  .[match_male_child == 1, ] %>% 
  .[, .(histid_1900, occ_dist, reel_seq_page)] %>% 
  .[, histid_1900 := tolower(histid_1900)] %>% 
  setnames('histid_1900', 'histid')



print(dt_fit1)
