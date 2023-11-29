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
library(stringr)
source("../supporting_code/define_fxns.R")

wd <- '~/Documents/projects/census_neighbor/data/'
wd <- '~/liran/census_neighbor/data/'

start_log_file("log/occ_balance_tests")


sub_sample <- ""
year <- 1880

top_occ <- fread(paste0(wd, "cleaned/top_occ_with_labs.csv")) %>% 
  .[, occ1950 := str_pad(occ1950, 3, pad = "0")]

occ_codes <- top_occ[sample_include == 1, occ1950]
occ_labs <- top_occ[sample_include == 1, label]

vars <- c("age", "white", "is_lit")

dt_fit <- data.table()
for (i in 1:length(occ_codes)) {
  print(occ_labs[i])
  
  sample <- fread(paste0(wd, "cleaned/new_occ_", occ_codes[i], "_sample_", year, 
                         sub_sample, ".csv"))
  
  sample %<>% 
    .[match_male_child_hh == 1, ]
  
  hh_sample <- sample %>% 
    .[relate == 1] %>% 
    .[, female := ifelse(sex == 2, 1, 0)] %>% 
    .[, reel := str_split_fixed(reel_seq_page, "_", 3)[, 1]] %>% 
    .[, is_lit := ifelse(lit == 4, 1, 0)] %>% 
    .[, female := ifelse(sex == 2, 1, 0)] %>% 
    .[, foreign := ifelse(nativity == 5, 1, 0)] %>% 
    .[, urban := ifelse(urban == 2, 1, 0)] %>% 
    .[, white := ifelse(race == 1, 1, 0)]
  
  for (var in vars) {
    for (j in c(0, 1)) {
      DT_fit <- copy(hh_sample) %>%
        .[occ_dist <= 10, ] %>% 
        .[urban == j] %>%
        .[, x := get(var)] %>% 
        .[, dm_x := x - mean(x), by = reel_seq_page] %>% 
        .[, dm_occ_dist := occ_dist - mean(occ_dist), by = reel_seq_page]
      
      if (var == "occscore") {
        DT_fit %<>% 
          .[x > 0, ]
      }
 
      fit <- lm_robust(dm_x ~ dm_occ_dist, data = DT_fit, se_type = 'stata')
      
      dt_fit2 <- tidy(fit) %>% 
        as.data.table() %>% 
        .[term == 'dm_occ_dist'] %>% 
        .[, var := var] %>%
        .[, fe := 1] %>% 
        .[, mean := mean(DT_fit$x)] %>%
        .[, .(var, mean, estimate, std.error, p.value, fe)]
      
      dt_fit3 <- rbind(dt_fit2) %>% 
        .[, occ := occ_labs[i]] %>% 
        .[, obs := nrow(DT_fit)] %>%
        .[, urban := j] %>%
        .[, .(occ, urban, var, obs, mean, estimate, std.error, p.value, fe)]
      
      
      dt_fit %<>% rbind(dt_fit3)
    }
  }
}

dt_fit %<>% 
  .[, perc := round(estimate /mean, 4) * 100]

dt_print <- copy(dt_fit) %>% 
  .[fe == 1, ] %>% 
  .[var %in% c("female", "is_lit", "foreign", "white"), `:=`(estimate = estimate * 100, 
                                                    std.error = std.error * 100, 
                                                    mean = mean * 100)] %>% 
  .[, var := factor(var, levels = c("white", "is_lit", "age"), 
                    labels = c("White", "Literacy", "Age"))] %>% 
  .[, `:=`(estimate = round(estimate, 2), std.error = round(std.error, 2), 
           mean = round(mean, 2))] %>% 
  .[, stars1 := ifelse(p.value <= .01, "***", ifelse(p.value <= .05, "**",
                                                     ifelse(p.value <= .1,
                                                            "*", "")))] %>%
  .[, estimate := paste0(estimate, stars1)] %>%
  .[, est_se := paste0("\\begin{tabular}{@{}c@{}}", estimate,
                       "\\\\ (", std.error,  ")\\end{tabular}")] %>% 
  .[order(urban, occ, var)] %>% 
  .[, .(occ, urban,  var, obs, mean, est_se, perc)] %>% 
  .[, max_obs := max(obs), by = occ] %>% 
  .[order(urban, -max_obs, var)] %>% 
  .[, max_obs := NULL] %>% 
  .[, ord := seq(0, .N - 1)] %>% 
  .[, occ := paste0(occ, " (N = ", obs, ")")] %>% 
  .[!(ord %% 3 == 0), occ := ""]

print(xtable(dt_print[urban == 1, .(occ, var, mean, est_se, perc)][1:24]), 
      sanitize.text.function = force, 
      include.rownames = FALSE)

print(xtable(dt_print[urban == 1, .(occ, var, mean, est_se, perc)][25:48]), 
      sanitize.text.function = force, 
      include.rownames = FALSE)

print(xtable(dt_print[urban == 0, .(occ, var, mean, est_se, perc)][1:24]), 
      sanitize.text.function = force, 
      include.rownames = FALSE)

print(xtable(dt_print[urban == 0, .(occ, var, mean, est_se, perc)][25:48]), 
      sanitize.text.function = force, 
      include.rownames = FALSE)

end_log_file()
