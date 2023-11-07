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
sub_sample <- ""
sample <- fread(paste0(wd, "cleaned/black_neighbor_sample_", year, 
                       sub_sample, ".csv"))

sample %<>% 
  .[match_male_child_hh == 1, ]

library(stringr)
hh_sample <- sample %>% 
  .[relate == 1] %>% 
  .[, is_lit := ifelse(lit == 4, 1, 0)] %>% 
  .[, female := ifelse(sex == 2, 1, 0)] %>% 
  .[, foreign := ifelse(nativity == 5, 1, 0)] %>% 
  .[, reel := str_split_fixed(reel_seq_page, "_", 3)[, 1]]


dtp <- hh_sample %>%
  .[, .N, by = black_dist]

ggplot(dtp) + 
  aes(x = black_dist, y = N) + 
  geom_bar(stat = 'identity')

uniqueN(hh_sample$black_line)


var <- "is_lit"

vars <- c("age", "is_lit")
FE = TRUE

dt_fit <- data.table()

for (var in vars) {
  print(var)
  hh_sample %<>% 
    .[, x := get(var)]


    fit <- lm_robust(x ~ black_dist, data = hh_sample)
    
    dt_fit1 <- tidy(fit) %>% 
      as.data.table() %>% 
      .[term == 'black_dist'] %>% 
      .[, var := var] %>%
      .[, fe := 0] %>% 
      .[, .(var, estimate, std.error, p.value, fe)]
    
    fit <- lm_robust(x ~ black_dist, data = hh_sample, 
                     fixed_effects = ~ black_line)
    
    dt_fit2 <- tidy(fit) %>% 
      as.data.table() %>% 
      .[term == 'black_dist'] %>% 
      .[, var := var] %>%
      .[, fe := 0] %>% 
      .[, .(var, estimate, std.error, p.value, fe)]
  

  
  dt_fit %<>% rbind(dt_fit1, dt_fit2)

}

print(dt_fit)



var <- 'is_lit'
dtp <- hh_sample %>% 
  .[, y := get(var)] %>% 
  .[, .(mean = mean(y), 
        sd = sd(y), 
        obs = .N), by = black_dist] %>% 
  .[, se := sd / sqrt(obs)] %>% 
  .[, `:=`(lb = mean - 1.96 * se, ub = mean + 1.96 * se)]

ggplot(dtp) + 
  aes(x = black_dist, y = mean, ymin = lb, ymax = ub) + 
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




