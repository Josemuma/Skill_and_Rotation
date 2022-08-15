library(pacman)
p_load(fitdistrplus,dplyr,tinytex,plyr)

# This script joins all the posterior results (probabilities and medians)
# of the Bayesian regression. If the seed is different, the MCMC will 
# show different results, similar, but different. 

# For the estimation of the probabilistic model, there are two paths:
# one is to estimate with the posterior outputs a probabilistic distribution
# which from literature should be 'normal'; two, with the estimated value 
# given by the regression.

# 5-year Probability ####
# n = # NUMBER OF FUNDS RERGESSED
# Bayes_5_Year_List <-read_rds('Bayes_Posteriors_5_Year_List')
# Bayes_Prob_5_Year <- matrix(nrow = n,ncol = 42)
# 
# n = # NUMBER OF FUNDS RERGESSED
# for (i in 1:n) { 
#   for(j in 1:42){
#     if(is.na(Bayes_5_Year_List[[i]][1,j]) == T){
#       Bayes_Prob_5_Year[i,j] <- 'NA'
#     } else{
#     Bayes_Prob_5_Year[i,j] <- pnorm(
#       0 ,fitdist(as.numeric(Bayes_5_Year_List[[i]][,j])*100,distr = "norm")$estimate[1],
#       fitdist(as.numeric(Bayes_5_Year_List[[i]][,j])*100,distr = "norm")$estimate[2],lower.tail = FALSE)
#     }
#   }
# }
# Bayes_Prob_5_Year <- as_tibble(Bayes_Prob_5_Year)
# colnames(Bayes_Prob_5_Year) <- paste0(rep(LETTERS[25],41),1975:2017,1979:2021)
# Bayes_Prob_5_Year <- Bayes_Prob_5_Year %>% mutate_if(is.factor, as.character) %>% mutate_if(is.character, as.numeric)
# Bayes_Prob_5_Year <- bind_cols(FundsToUse$Funds[1:n],Bayes_Prob_5_Year)
# colnames(Bayes_Prob_5_Year)[1] <- 'Funds'
# saveRDS(Bayes_Prob_5_Year, file = "Bayes_Prob_5_Year")
# rm(Bayes_5_Year_List)


## 5-Year Median ####
# We need to convert to tibble 
# Bayes_5_Year_Median_Alpha <- read_rds('Bayes_5_Year_Median_Alpha')
# Bayes_5_Year_Median_Alpha <- as_tibble(bind_cols(FundsToUse$Funds[1:n],Bayes_5_Year_Median_Alpha))
# colnames(Bayes_5_Year_Median_Alpha) <- c('Funds',paste0(rep(LETTERS[25],41),1975:2017,1979:2021))
# saveRDS(Bayes_5_Year_Median_Alpha, file = "Bayes_5_Year_Median_Alpha")


# 1-year Probability ####
# Bayes_1_Year_List <-read_rds('Bayes_Posteriors_1_Year_List')
# Bayes_Prob_1_Year <- matrix(nrow = n,ncol = 42)
# 
# n = NUMBER OF FUNDS RERGESSED
# for (i in 1:n) { 
#   for(j in 1:42){
#     if(is.na(Bayes_1_Year_List[[i]][1,j]) == T){
#       Bayes_Prob_1_Year[i,j] <- 'NA'
#     } else{
#       Bayes_Prob_1_Year[i,j] <- pnorm(
#         0 ,fitdist(as.numeric(Bayes_1_Year_List[[i]][,j])*100,distr = "norm")$estimate[1],
#         fitdist(as.numeric(Bayes_1_Year_List[[i]][,j])*100,distr = "norm")$estimate[2],lower.tail = FALSE)
#     }
#   }
# }
# Bayes_Prob_1_Year <- as_tibble(Bayes_Prob_1_Year)
# Bayes_Prob_1_Year <- bind_cols(FundsToUse$Funds[1:n],Bayes_Prob_1_Year)
# colnames(Bayes_Prob_1_Year) <- c('Funds', paste0(rep(LETTERS[25],41),1980:2021))
# Bayes_Prob_1_Year <- Bayes_Prob_1_Year %>% mutate_if(is.factor, as.character) %>% mutate_if(is.character, as.numeric)
# saveRDS(Bayes_Prob_1_Year, file = "Bayes_Prob_1_Year")
# rm(Bayes_1_Year_List)

## 1-Year Median ####
# We need to convert to tibble 
# Bayes_1_Year_Median_Alpha <- read_rds('Bayes_1_Year_Median_Alpha')
# Bayes_1_Year_Median_Alpha <- as_tibble(bind_cols(FundsToUse$Funds[1:n],Bayes_1_Year_Median_Alpha))
# colnames(Bayes_1_Year_Median_Alpha) <- c('Funds',paste0(rep(LETTERS[25],41),1980:2021))
# saveRDS(Bayes_1_Year_Median_Alpha, file = "Bayes_1_Year_Median_Alpha")

## Coefficients Together ####
### IS Change load data ####
# Bayes_5_Year_Coef_01 <- bind_cols(FundsToUse$Funds,
#                                bind_rows(as_tibble(read_rds(file = 'BR_IS_Coef01_5_Year')),
#                                          as_tibble(read_rds(file = 'BR_IS_Coef01_5_Year_2')),
#                                          as_tibble(read_rds(file = 'BR_IS_Coef01_5_Year_3'))[1:661,]))
# Bayes_5_Year_Coef_01 %>% mutate_if(is.factor, as.character) %>% mutate_if(is.character, as.numeric)
# colnames(Bayes_5_Year_Coef_01) <- c('Funds',paste0(rep(LETTERS[25],41),1975:2017,1979:2021))
# Bayes_5_Year_Coef_01
# # saveRDS(Bayes_5_Year_Coef_01, file = 'Bayes_5_Year_Coef_01')
# 
### OOS Change load data ####
# Bayes_1_Year_Coef_01 <- bind_cols(FundsToUse$Funds,
#                                bind_rows(as_tibble(read_rds(file = 'BR_OOS_Coef01_5_Year')),
#                                          as_tibble(read_rds(file = 'BR_OOS_Coef01_5_Year_2')),
#                                          as_tibble(read_rds(file = 'BR_OOS_Coef01_5_Year_3'))[1:661,]))
# Bayes_1_Year_Coef_01 %>% mutate_if(is.factor, as.character) %>% mutate_if(is.character, as.numeric)
# colnames(Bayes_1_Year_Coef_01) <- c('Funds',paste0(rep(LETTERS[25],41),1975:2017,1979:2021))
# Bayes_1_Year_Coef_01
# # saveRDS(Bayes_1_Year_Coef_01, file = 'Bayes_1_Year_Coef_01')


# Number of funds active in each period 
# IS
# Bay_Activ_IS_Funds <- sapply(1:42, function(x) nrow(BayAlphIS %>% dplyr::select(x+1) %>% filter(!is.na(.[[1]]))))
# OLS_Activ_IS_Funds <- sapply(1:42, function(x) nrow(OLSAlphIS %>% dplyr::select(x+1) %>% filter(!is.na(.[[1]]))))
# # OOS
# Bay_Activ_OOS_Funds <- sapply(1:41, function(x) nrow(BayAlphOOS %>% dplyr::select(x+1) %>% filter(!is.na(.[[1]]))))
# OLS_Activ_OOS_Funds <- sapply(1:41, function(x) nrow(BayAlphOOS %>% dplyr::select(x+1) %>% filter(!is.na(.[[1]]))))
