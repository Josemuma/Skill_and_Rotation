library(pacman)
p_load(dplyr, tidyverse,tibble,plyr,kableExtra,tictoc,purrr,ggplot2,
       fitdistrplus, plyr, knitr, scales)
# Load Data ####
## Funds and Returns ####
RealReturns<- readRDS("RealReturns")
FundsToUse <- readRDS("Number_of_Fund") # Real Funds Numbers
colnames(FundsToUse)[1] <- c("Funds")

# Bayes ####
## Load Data ####
# We load the DBs and conver to numeric where necessary for the analysis
# Estimated Alphas IS (5 year period)
BayAlphaIS <- readRDS('Bayes_5_Year_Median_Alpha')
# Probabilities IS (5 year period)
BayProbIS <- readRDS('Bayes_5_Year_Prob')
# Estimated Alphas OOS (1 year period)
BayAlphaOOS <- readRDS('Bayes_1_Year_Median_Alpha')
# Probabilities OOS (1 year period)
BayProbOOS <- readRDS('Bayes_1_Year_Prob')

## Bayes IS ####
# Funds detected in IS not free of survivor bias
BayIS_All_neg <- lapply(2:44, function(x)
  (tibble(BayProbIS %>% dplyr::select(Funds,x) %>% 
            filter(.[[2]] <= 0.05))))

# One has a list per period with the detected funds in the 
# 5-year period, next period and t + 1 period IS
IS_Bayes_Detected_neg <- lapply(2:44, function(x)
    left_join(
      BayProbIS %>% dplyr::select(Funds, x) %>% 
        filter(!is.na(.[[2]]) & .[[2]] <= 0.05),
      BayAlphaIS %>% dplyr::select(Funds, x)
      , by = 'Funds'))

# The returns for the list above throughout each month
IS_Bayes_Detected_Rtns_neg <- lapply(2:44, function(x) 
  left_join(
    BayProbIS %>% dplyr::select(Funds, x) %>% 
      filter(!is.na(.[[2]]) & .[[2]] <= 0.05) %>% 
      dplyr::select(Funds),
    RealReturns %>% dplyr::select(Funds, returnrf, date, mtna, mnav) %>% 
      filter(date >= '1979-12-01'), 
    by = 'Funds')
)
      
##  Bayes OOS ####
# One has a list per period with the detected funds in the 
# 5-year period, next period and t + 1 period OOS
OOS_Bayes_Detected_neg <- lapply(2:43, function(x)
  left_join(
    BayProbOOS %>% dplyr::select(Funds, x) %>% 
      filter(!is.na(.[[2]]) & .[[2]] <= 0.05),
    BayAlphaOOS %>% dplyr::select(Funds, x)
    , by = 'Funds'))

# The returns for the list above throughout each month
OOS_Bayes_Detected_Rtns_neg <- lapply(2:43, function(x) 
  left_join(
    BayProbIS %>% dplyr::select(Funds, x) %>% 
      filter(!is.na(.[[2]]) & .[[2]] <= 0.05) %>% 
      dplyr::select(Funds),
    RealReturns  %>% dplyr::select(Funds, returnrf, date, mtna, mnav) %>% 
      filter(date >= '1979-12-01'), 
    by = 'Funds')
)

# OLS Data ####
## Load Data ####
OLSAlphaIS <- readRDS('OLS_Alphas_5_Year') 
OLSpValueIS <- readRDS('OLS_pValue_5_Year')
OLSAlphaOOS <- readRDS('OLS_Alphas_1_Year')
OLSpValueOOS <- readRDS('OLS_pValue_1_Year')

##  OLS IS ####
# One has a list per period with the detected funds in the 
# 5-year period, next period and t + 1 period IS
OLSIS_All_neg <-lapply(2:44, function(x)
  left_join(OLSAlphaIS %>% dplyr::select(1,x) %>% filter(.[[2]] < 0),
            OLSpValueIS %>% dplyr::select(1,x) %>% filter(.[[2]] <= 0.05),
            by = 'Funds') %>% filter(!is.na(.[[3]])))

# Funds detected in IS free of survivor bias
IS_OLS_Detected_neg <- lapply(2:44, function(x) 
  left_join(
    OLSAlphaIS %>% dplyr::select(Funds, x) %>% 
      filter(!is.na(.[[2]]) & .[[2]] < 0.0),
    OLSpValueIS %>% dplyr::select(Funds, x) %>% 
    filter(!is.na(.[[2]]))
    , by = 'Funds') %>% filter(.[[3]] <= 0.05))

# The returns for the list above throughout each month
IS_OLS_Detected_Rtns_neg <- lapply(2:44, function(x) 
  left_join(
    IS_OLS_Detected_Rtns_neg[[x-1]] %>% dplyr::select(Funds),
    RealReturns  %>% dplyr::select(Funds, returnrf, date, mtna, mnav)%>%
      filter(date >= '1979-12-01'), 
    by = 'Funds')
)

##  OLS OOS ####
# One has a list per period with the detected funds in the 
# 5-year period, next period and t + 1 period OOS
OOS_OLS_Detected_neg <- lapply(2:43, function(x) 
  inner_join(
    OLSAlphaOOS %>% dplyr::select(Funds, x) %>% 
      filter(!is.na(.[[2]]) & .[[2]] < 0),
    OLSpValueOOS %>% dplyr::select(Funds, x) %>% 
      filter(!is.na(.[[2]]) & .[[2]] <= 0.05)
    , by = 'Funds'))

# The returns for the list above throughout each month
OOS_OLS_Detected_Rtns_neg <- lapply(2:43, function(x) 
  left_join(
    OOS_OLS_Detected_neg[[x-1]] %>% dplyr::select(Funds),
    RealReturns  %>% dplyr::select(Funds, returnrf, date, mtna, mnav) %>%
      filter(date >= '1979-12-01'), 
    by = 'Funds')
)



