library(pacman)
p_load(dplyr,tidyverse,plyr,purrr,ggplot2)

# Persistence ####
## Positive ####
# Create list of probability, alpha, returns, mtna, mnav
list_detected_neg <-  lapply(2:42, function(x)
  left_join(
    left_join(
      left_join(
        inner_join(
          inner_join(
            BayProbIS %>% dplyr::select(1,x) %>% filter(.[[2]] <= 0.05),
            BayAlphaIS %>% dplyr::select(1,x) ,by = 'Funds'),
          inner_join(
            BayProbOOS %>% dplyr::select(Funds,x, x+1),
            BayAlphaOOS  %>% dplyr::select(Funds,x, x+1), by = 'Funds')
          , by = 'Funds') %>% filter(!is.na(.[[4]])), 
        RealReturns %>% filter(date >= paste(1978+x,'-12-01',sep = '') & 
                                 date <= paste(1978+x,'-12-31',sep = '')) %>% 
          dplyr::select(Funds, returnrf, mtna, mnav, date), by =  'Funds'),
      RealReturns %>% filter(date >= paste(1979+x,'-12-01',sep = '') & 
                               date <= paste(1979+x,'-12-31',sep = '')) %>% 
        dplyr::select(Funds, returnrf, mtna, mnav, date), by =  'Funds'),
    RealReturns %>% filter(date >= paste(1980+x,'-12-01',sep = '') & 
                             date <= paste(1980+x,'-12-31',sep = '')) %>% 
      dplyr::select(Funds, returnrf, mtna, mnav, date), by =  'Funds') %>% 
    dplyr::select(1,2,4,5,3,6,7,8:19))
list_detected_neg # 41 elements

## Total funds per period ####
# We have managerial info from [19] onwards 
total_funds_is_neg <- detection_neg$Bayes
sum(total_funds_is_neg[19:43])
sum(total_funds_is_neg)
total_funds_skilled_neg <- sapply(1:41, function(x) 
  nrow(list_detected_neg[[x]] %>% filter(.[[3]] >= 0.95)))
sum(total_funds_skilled_neg[19:41])
sum(total_funds_skilled_neg)

# We can change the threshold
total_funds_skilled_x2_neg <- sapply(1:41, function(x) 
  nrow(list_detected_neg[[x]] %>% filter(.[[3]] >= 0.95 & .[[4]] >= 0.90)))
sum(total_funds_skilled_x2_neg[19:41])
sum(total_funds_skilled_x2_neg)

# Bounce-back skill
total_funds_skilled_bb_neg <- sapply(1:41, function(x) 
  nrow(list_detected_neg[[x]] %>% filter(.[[2]] >= 0.95 & .[[4]] >= 0.95)))
sum(total_funds_skilled_bb_neg[19:41])
sum(total_funds_skilled_bb_neg)

# Bounce-back list ####
list_detected_positive_bb <- lapply(1:41, function(x) 
  (list_detected_positive[[x]] %>% filter(.[[2]] >= 0.95 & .[[4]] >= 0.95)))
