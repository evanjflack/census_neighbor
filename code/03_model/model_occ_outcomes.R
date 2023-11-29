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
library(xtable)
source("../supporting_code/define_fxns.R")

wd <- '~/liran/census_neighbor/data/'

tart_log_file("log/model_occ_outcomes")

year1 <- 1880
year2 <- 1900
sub_sample <- ""

top_occ <- fread(paste0(wd, "cleaned/top_occ_with_labs.csv")) %>% 
  .[, occ1950 := str_pad(occ1950, 3, pad = "0")]

occ_codes <- top_occ[sample_include == 1, occ1950]
occ_labs <- top_occ[sample_include == 1, label]

dt_fit <- data.table()
for (i in 1:length(occ_codes)) {
  print(occ_labs[i])
  
  DT_fit <- fread(paste0(wd, "cleaned/occ_", occ_codes[i], "_outcomes_", year2, 
                         sub_sample, ".csv")) %>% 
    .[, occ1950 := str_pad(occ1950, 3, pad = "0")] %>% 
    .[age1 >= 5, ]
  
  
  sample <- fread(paste0(wd, "cleaned/new_occ_", occ_codes[i], "_sample_", year1, 
                         sub_sample, ".csv")) %>%
    .[match_male_child == 1, ] %>% 
    .[, .(histid_1900, urban)] %>%
    .[, histid_1900 := tolower(histid_1900)] %>% 
    setnames("histid_1900", "histid") %>% 
    .[, urban := ifelse(urban == 2, 1, 0)]
  
  DT_fit %<>% 
    merge(sample, by = "histid")
  
  for (j in c(0, 1)) {
    DT_fit1 <- copy(DT_fit) %>% 
      .[urban == j] %>% 
      .[, y := ifelse(occ1950 == occ_codes[i], 1, 0)] %>%
      .[, dm_y := y - mean(y), by = reel_seq_page] %>% 
      .[, dm_occ_dist := occ_dist - mean(occ_dist), by = reel_seq_page]
    
    
    fit <- lm_robust(dm_y ~ dm_occ_dist, data = DT_fit1, se_type = 'stata')
    
    dt_fit1 <- tidy(fit) %>% 
      as.data.table() %>% 
      .[term == 'dm_occ_dist'] %>% 
      .[, .(estimate, std.error, p.value)] %>%
      .[, mean := mean(DT_fit1$y)] %>% 
      .[, sample_occ := occ_labs[i]] %>% 
      .[, perc := round(estimate / mean, 4) * 100]
    
    
    dt_fit1 %<>%
      .[, `:=`(estimate = round(estimate * 100, 3), 
               std.error = round(std.error * 100, 3), 
               mean = round(mean * 100, 3))] %>% 
      .[, stars1 := ifelse(p.value <= .01, "***", ifelse(p.value <= .05, "**",
                                                         ifelse(p.value <= .1,
                                                                "*", "")))] %>%
      .[, estimate := paste0(estimate, stars1)] %>%
      .[, est_se := paste0("\\begin{tabular}{@{}c@{}}", estimate,
                           "\\\\ (", std.error,  ")\\end{tabular}")] %>% 
      .[, obs := nrow(DT_fit1)] %>%
      .[, urban := j] %>%
      .[, .(sample_occ, urban, obs, mean, est_se, perc)]
    dt_fit %<>% rbind(dt_fit1)
  }
}

dt_fit %<>% 
  .[, sample_occ := factor(sample_occ, levels = occ_labs)] %>% 
  .[order(sample_occ), ]

print(xtable(dt_fit[urban == 1, .(sample_occ, obs, mean, est_se, perc)]), 
      sanitize.text.function = force, 
      include.rownames = FALSE)

print(xtable(dt_fit[urban == 0, .(sample_occ, obs, mean, est_se, perc)]), 
      sanitize.text.function = force, 
      include.rownames = FALSE)

end_log_file()