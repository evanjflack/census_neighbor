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

year <- 1880
sub_sample <- "_ny"
sample <- fread(paste0(wd, "cleaned/occ_093_sample_", year, 
                       sub_sample, ".csv"))

sample %<>% 
  .[match_male_child_hh == 1, ]

library(stringr)
hh_sample <- sample %>% 
  .[relate == 1] %>% 
  .[, female := ifelse(sex == 2, 1, 0)] %>% 
  .[, reel := str_split_fixed(reel_seq_page, "_", 3)[, 1]]


dtp <- hh_sample %>%
  .[, .N, by = .(occ_dist)]

ggplot(dtp) + 
  aes(x = occ_dist, y = N) + 
  geom_bar(stat = 'identity')





vars <- c("age", "female")
dt_fit <- data.table()
for (var in vars) {
  print(var)
  DT_fit <- copy(hh_sample) %>%
    .[occ_dist <= 10, ] %>% 
    .[, x := get(var)] %>% 
    .[, dm_x := x - mean(x), by = reel_seq_page] %>% 
    .[, dm_occ_dist := occ_dist - mean(occ_dist), by = reel_seq_page]
  
  
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
  
  
  
  dt_fit %<>% rbind(dt_fit1, dt_fit2)
  
}

dt_fit %<>% 
  .[, perc := estimate /mean]


print(dt_fit)

var <- 'age'
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




