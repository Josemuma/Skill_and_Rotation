library(pacman)
p_load(dplyr,tidyverse,plyr,tictoc,purrr,ggplot2)

# Compare methods ####
# IS Period ####
## Detection Period Positive ####
detection_positive <- bind_cols(
  Bayes = sapply(1:43, function(x)
    nrow(IS_Bayes_Detected[[x]])),
  OLS = sapply(1:43, function(x) 
    nrow(IS_OLS_Detected[[x]]))
)
detection_positive
colMeans(detection_positive)

# Proportion skilled (IS period) Positive ####
proportion_IS_skilled_positive <- bind_cols(
  Bayes = sapply(1:42, function(x) 
    nrow(inner_join(IS_Bayes_Detected[[x]],
                    IS_Bayes_Detected[[x+1]], 
                    by = 'Funds')) / nrow(IS_Bayes_Detected[[x]])),
  OLS = sapply(1:42, function(x) 
    nrow(inner_join(IS_OLS_Detected[[x]],
                    IS_OLS_Detected[[x+1]],
                    by = 'Funds'))/nrow(IS_OLS_Detected[[x]]))
)
# We add the year 2021 for both cases
proportion_IS_skilled_positive
colMeans(proportion_IS_skilled_positive, na.rm = T)

# OOS Period ####
# Proportion skilled (OOS period) Positive ####
# We find intersection from what we saw 5 years ago and what we 
# see the immediate next period
proportion_OOS_skilled_positive <- bind_cols(
  Bayes = sapply(1:42, function(x) 
    nrow(inner_join(OOS_Bayes_Detected[[x]],
                    IS_Bayes_Detected[[x]], 
                    by = 'Funds')) / nrow(IS_Bayes_Detected[[x]])),
  OLS = sapply(1:42, function(x) 
    nrow(inner_join(OOS_OLS_Detected[[x]],
                    IS_OLS_Detected[[x]],
                    by = 'Funds'))/nrow(IS_OLS_Detected[[x]]))
)
proportion_OOS_skilled_positive
colMeans(proportion_OOS_skilled_positive,na.rm = T)
# Lack of recent positive alphas make the difference less significant

# Proportion skilled (1-year & OOS) Positive ####
# We find intersection from what we saw 1 year ago and what we 
# see the immediate next period
proportion_OOS_skilled_positive_1to1 <- bind_cols(
  Bayes = sapply(1:41, function(x) 
    nrow(inner_join(OOS_Bayes_Detected[[x+1]],
                    OOS_Bayes_Detected[[x]], 
                    by = 'Funds')) / nrow(OOS_Bayes_Detected[[x]])),
  OLS = sapply(1:41, function(x) 
    nrow(inner_join(OOS_OLS_Detected[[x+1]],
                    OOS_OLS_Detected[[x]],
                    by = 'Funds'))/nrow(OOS_OLS_Detected[[x]]))
)
proportion_OOS_skilled_positive_1to1
colMeans(proportion_OOS_skilled_positive_1to1, na.rm = T)
colMeans(proportion_OOS_skilled_positive_1to1 %>% replace_na(list(Bayes = 0, OLS = 0)))

