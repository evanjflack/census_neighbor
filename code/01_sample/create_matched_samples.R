# Header -----------------------------------------------------------------------
# Proj: Census Neighbor
# Author: Evan Flack (flack@stanford.edu)
# Desc: Creates matched samples between census years

# Read in Packages/Functions ---------------------------------------------------
library(tictoc)
suppressMessages(library(data.table, quietly = T))
library(magrittr)
suppressMessages(library(tidyr))
library(foreach)
source("../supporting_code/define_fxns.R")

# start_log_file("log/create_matched_samples")

# Create Matched Samples -------------------------------------------------------

years <- c(1880, 1900, 1910, 1920, 1930, 1940)

sub_sample <- "_ny"
message(sub_sample, " sample.")

years_grid <- crossing(years, years) %>% 
  as.data.table() %>% 
  setnames(c("year1", "year2")) %>%
  .[year2 > year1] %>% 
  .[year1 == 1880 & year2 == 1900, ]

method <- "abe_nysiis_standard"

year1 <- 1880
year2 <- 1900
ret <- foreach(year1 = years_grid$year1, 
               year2 = years_grid$year2) %do% 
  {
    message("Start ", year1, "-", year2, ".")
    
    message("Reading in xwalk...")
    xwalk <- fread(paste0("../../data/crosswalks/crosswalk_", year1, "_", year2, 
                          ".csv")) %>%
      .[get(method) == 1] %>%
      .[, paste0("histid_", c(year1, year2)), with = FALSE]
    
    message("Reading in year 2 data...")
    sub_samplem <- "_ny"
    sample <- fread(paste0("../../data/cleaned/occ_", "075", "_sample_", year1, 
                           sub_sample, ".csv")) %>% 
      .[, .(histid)]
      merge(xwalk, by.x = "histid", by.y = paste0("histid_", year1))
    
    head(sample)
    
    
    
    mean(is.na(sample$histid_1900))
    

    dt_year2 <- fread(paste0("../../data/census_raw/ipums_", year2, sub_sample,
                             ".csv")) %>%
      setnames(tolower(names(.))) %>%
      .[sex == 1, ]
    
    dt_year2 %<>% 
      merge(xwalk, by.x = "histid", by.y = paste0("histid_", year2))
    
    dt_year1 <- fread(paste0("../../data/census_raw/ipums_", year1, sub_sample,
                             ".csv")) %>%
      setnames(tolower(names(.))) %>%
      .[sex == 1, ] %>% 
      .[, histid := tolower(histid)]
  
    
    dt_year1 %<>% 
      merge(xwalk, by.x = "histid", by.y = paste0("histid_", year1))
    
    names(dt_year1)
    
    mean(occ_sample$histid %in% dt_year1$histid)
    
    head(dt_year1)
    
    head(dt_year2)
    
    
    mean(dt_year1$histid %in% dt_year2$hisid_1880)
    
    
    # 
    # n1 <- nrow(dt_year2)
    # 
    # dt_year2 %<>% 
    #   merge(xwalk, by.x = "histid", by.y = paste0("histid_", year2))
    # 
    # n2 <- nrow(dt_year2)
    # message("Match rate = ", round(n2 / n1, 3) * 100, "%")

    fwrite(dt_year2, paste0("../data/cleaned/matched_sample_", 
                            year1, "_", year2, sub_sample, ".csv"))
    
    message("End ", year1, "-", year2, ".")
    
  }

# End --------------------------------------------------------------------------

end_log_file()
