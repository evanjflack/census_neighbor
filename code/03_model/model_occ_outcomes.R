post_sample %<>% 
  .[, dm_y := y - mean(y), by = reel_seq_page] %>% 
  .[, dm_occ_dist := occ_dist - mean(occ_dist), by = reel_seq_page]

fit <- lm(dm_y ~ dm_occ_dist, data = post_sample)

mean(post_sample$y)

summary(fit)




.[histid %chin% pre_sample$histid]

mean(pre_sample$histid %in% dt$histid)

dt$histid

dt %<>% 
  merge(pre_sample, by = 'histid')

post_sampe


dt %<>% 
  .[serial %in% post_sample$serial]

dt %<>% 
  .[, match := ifelse(histid %chin% pre_sample$histid, 1, 0)]

dt_hh <- dt 

head(pre_sample)