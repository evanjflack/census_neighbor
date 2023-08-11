# Header -----------------------------------------------------------------------
# Proj: Census Neighbor
# Author: Evan Flack (flack@stanford.edu)
# Desc: Links doctor sample from year 1 to matched sample from year 2

# Read in Packages/Functions ---------------------------------------------------
library(tictoc)
suppressMessages(library(data.table, quietly = T))
library(magrittr)
source("define_fxns.R")

start_log_file("log/match_doctor_sample")

year1 <- 1880
year2 <- 1900
sub_sample <- ""

# Read in Data -----------------------------------------------------------------

doc_sample <- fread(paste0("../data/cleaned/doc_sample_", year1, sub_sample, 
                          ".csv")) %>% 
  .[, .(histid, doc_dist, age, sex, occ1950)] %>% 
  setnames(paste0(names(.), "_", year1))

year2_sample <- fread(paste0("../data/cleaned/matched_sample_", 
                             year1, "_", year2, sub_sample, ".csv")) %>% 
  .[, c("histid", paste0("histid_", year1), "age", "sex", "occ1950", "chborn", 
        "chsurv"), with = FALSE]

# Merge samples ----------------------------------------------------------------

matched_sample <- doc_sample %>% 
  merge(year2_sample, by = paste0("histid_", year1))

fwrite(matched_sample, paste0("../data/cleaned/merged_doc_sample_", year1, "_", 
                              year2, sub_sample, ".csv"))

end_log_file()

