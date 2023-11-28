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
# wd <- '~/liran/census_neighbor/data/'

# start_log_file("log/id_black_neighbors")


sub_sample <- ""

year <- 1880
# Doctors, teachers, lawyers, clergymen, carpenters, blacksmith
occ_codes <- c("075", "093", '055', '009', '510', "501")
occ_labs <- c("Doctor", "Teacher", "Lawyer", "Clergy", "Carpenter", "Blacksmith")
vars <- c("age", "female", "is_lit", "foreign", "occscore")

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
    .[, foreign := ifelse(nativity == 5, 1, 0)]
  
  


  for (var in vars) {
    DT_fit <- copy(hh_sample) %>%
      .[occ_dist <= 10, ] %>% 
      .[, x := get(var)] %>% 
      .[, dm_x := x - mean(x), by = reel_seq_page] %>% 
      .[, dm_occ_dist := occ_dist - mean(occ_dist), by = reel_seq_page]
    
    if (var == "occscore") {
      DT_fit %<>% 
        .[x > 0, ]
    }
    
    fit <- lm_robust(x ~ occ_dist, data = DT_fit, se_type = 'stata')
    
    dt_fit1 <- tidy(fit) %>% 
      as.data.table() %>% 
      .[term == 'occ_dist'] %>% 
      .[, var := var] %>%
      .[, fe := 0] %>% 
      .[, mean := mean(DT_fit$x)] %>%
      .[, .(var, mean, estimate, std.error, p.value, fe)]
    
    fit <- lm_robust(dm_x ~ dm_occ_dist, data = DT_fit, se_type = 'stata')
    
    dt_fit2 <- tidy(fit) %>% 
      as.data.table() %>% 
      .[term == 'dm_occ_dist'] %>% 
      .[, var := var] %>%
      .[, fe := 1] %>% 
      .[, mean := mean(DT_fit$x)] %>%
      .[, .(var, mean, estimate, std.error, p.value, fe)]
    
    dt_fit3 <- rbind(dt_fit1, dt_fit2) %>% 
      .[, occ := occ_labs[i]] %>% 
      .[, obs := nrow(DT_fit)] %>%
      .[, .(occ, var, obs, mean, estimate, std.error, p.value, fe)]
      

    dt_fit %<>% rbind(dt_fit3)
    
  }
  
}

dt_fit %<>% 
  .[, perc := round(estimate /mean, 4) * 100]

dt_print <- copy(dt_fit) %>% 
  .[fe == 1, ] %>% 
  .[var %in% c("female", "is_lit", "foreign"), `:=`(estimate = estimate * 100, 
                                                    std.error = std.error * 100, 
                                                    mean = mean * 100)] %>% 
  .[, var := factor(var, levels = c("female", "is_lit", "foreign", 
                                    "age", "occscore"), 
                    labels = c("Female", "Literacy", "Foregin", "Age", 
                               "Occupational Score"))] %>% 
  .[, `:=`(estimate = round(estimate, 2), std.error = round(std.error, 2), 
           mean = round(mean, 2))] %>% 
  .[, stars1 := ifelse(p.value <= .01, "***", ifelse(p.value <= .05, "**",
                                                     ifelse(p.value <= .1,
                                                            "*", "")))] %>%
  .[, estimate := paste0(estimate, stars1)] %>%
  .[, est_se := paste0("\\begin{tabular}{@{}c@{}}", estimate,
                       "\\\\ (", std.error,  ")\\end{tabular}")] %>% 
  .[order(occ, var)] %>% 
  .[, .(occ, var, obs, mean, est_se, perc)] %>% 
  .[, occ := NULL] %>% 
  .[, var := paste0("\\hspace{5 mm}", var)]

dt_lab <- data.table(var = occ_labs, obs = "", mean = "", 
                     est_se = "", perc = "") %>% 
  .[order(var)] %>% 
  .[, var := paste0("\\textit{", var, "}")]

dt_print <- rbind(dt_lab[1], dt_print[1:5], 
                  dt_lab[2], dt_print[6:10], 
                  dt_lab[3], dt_print[11:15], 
                  dt_lab[4], dt_print[16:20], 
                  dt_lab[5], dt_print[21:25], 
                  dt_lab[6], dt_print[26:30])

print(xtable(dt_print), sanitize.text.function = force, 
      include.rownames = FALSE)

var <- 'female'
dtp <- hh_sample %>% 
  .[, y := get(var)] %>% 
  .[, .(mean = mean(y), 
        sd = sd(y), 
        obs = .N), by = occ_dist] %>% 
  .[, se := sd / sqrt(obs)] %>% 
  .[, `:=`(lb = mean - 1.96 * se, ub = mean + 1.96 * se)]

ggplot(dtp) + 
  aes(x = occ_dist, y = mean, ymin = lb, ymax = ub) + 
  geom_point() + 
  geom_errorbar()

summary(fit)




male_sample <- sample %>% 
  .[male_child == 1, ] %>%
  .[, is_lit := ifelse(lit == 4, 1, 0)] %>% 
  .[, is_native := ifelse(nativity ==1, 1, 0)]

var <- "occscore"
dtp <- male_sample %>% 
  .[occscore > 0] %>% 
  .[match == 1, ] %>% 
  .[match_male_child_hh == 1, ] %>% 
  .[, y := get(var)] %>% 
  .[, .(mean = mean(y), 
        sd = sd(y), 
        obs = .N), by = black_dist] %>% 
  .[, se := sd / sqrt(obs)] %>% 
  .[, `:=`(lb = mean - 1.96 * se, ub = mean + 1.96 * se)]




mean(male_sample)


fit <- lm(occscore ~ black_dist, 
          data = male_sample[occscore > 0 & match == 1 & black_dist <= 5])
summary(fit)


hh_sample <- sample %>% 
  .[relate == 1, ] %>% 
  .[, na_occ := ifelse(occscore == 0, 1, 0)] %>% 
  .[, is_lit := ifelse(lit == 4, 1, 0)]

var <- "occscore"
dtp <- hh_sample %>% 
  .[lit > 0] %>% 
  .[match_male_child_hh == 1, ] %>% 
  .[, y := get(var)] %>% 
  .[, .(mean = mean(y), 
        sd = sd(y), 
        obs = .N), by = black_dist] %>% 
  .[, se := sd / sqrt(obs)] %>% 
  .[, `:=`(lb = mean - 1.96 * se, ub = mean + 1.96 * se)]

ggplot(dtp[black_dist <= 10]) + 
  aes(x = black_dist, y = mean, ymin = lb, ymax = ub) + 
  geom_point() + 
  geom_errorbar()

names(sample)

fit <- lm(occscore ~ black_dist, data = hh_sample[occscore > 0 & black_dist <= 5])
summary(fit)

head(hh_sample)

occ_score  <- sample %>% 
  .[erscor50 == "999.9", erscor50 := NA] %>%
  .[occscore == 0, occscore := NA] %>%
  .[, .(occscore = max(occscore, na.rm = T),
        erscor50 = max(erscor50, na.rm = T)),
    by = serial] %>%
  .[occscore == -Inf, occscore := NA] %>%
  .[erscor50 == -Inf, erscor50 := NA] %>% 
  .[!(is.na(occscore) | is.na(erscor50)), ]

hh_sample %<>% 
  merge(occ_score, by = "serial")

var <- "erscor50"
dtp <- hh_sample %>% 
  .[, y := get(var)] %>% 
  .[, .(mean = mean(y), 
        sd = sd(y), 
        obs = .N), by = black_dist] %>% 
  .[, se := sd / sqrt(obs)] %>% 
  .[, `:=`(lb = mean - 1.96 * se, ub = mean + 1.96 * se)]

fit <- lm_robust(occscore ~ black_dist,
                 data = hh_sample)

summary(fit)




