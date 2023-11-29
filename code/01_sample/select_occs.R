top_occ <- fread(paste0(wd, "cleaned/top_occ.csv"))

occ_labs <- c("None", "Farmer", "Farmer", "Laborer", "Student", 
              "Service Worker", "None", "Manager", "Carpenter", "Sales",
              "Miner", "Blacksmith", "Truck Driver", "Household Worker", "Painter/Construction", 
              "Shoemaker", "Retired", "Clerical Worker", "Stonemason", "Craftsman", 
              "Machinist", "Doctor", "Tailor", "Meat Cutter", "Teacher", 
              "Sailor", "Lawyer", "Clergymen", "Bookkeeper", "Typesetter", 
              "Stationary Engineer", "Huckster")

include <- c(0, 0, 0, 0, 0, 
             0, 0, 0, 1, 0, 
             0, 1, 0, 0, 1, 
             1, 0, 0, 1, 1, 
             1, 1, 1, 1, 1, 
             0, 1, 1, 1, 1, 
             1, 0)

my_occ <- top_occ[1:32] %>% 
  .[, label := occ_labs] %>% 
  .[, sample_include := include]

sum(my_occ$sample_include)

fwrite(my_occ, paste0(wd, "cleaned/top_occ_with_labs.csv"))
