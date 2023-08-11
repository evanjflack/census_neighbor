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

# start_log_file("log/id_black_neighbors")

year <- 1880
sub_sample <- ""
sample <- fread(paste0("../../data/cleaned/black_neighbor_sample_", year, 
                       sub_sample, ".csv"))

# sample <- fread(paste0("../../data/cleaned/occ_", "093", "_sample_", year, 
#                        sub_sample, ".csv"))

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

ggplot(dtp[black_dist <= 10]) + 
  aes(x = black_dist, y = mean, ymin = lb, ymax = ub) + 
  geom_point() + 
  geom_errorbar()


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




