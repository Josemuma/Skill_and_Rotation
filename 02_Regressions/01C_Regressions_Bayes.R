library(pacman)
p_load(fitdistrplus,rstan,dplyr, rstanarm, dbplyr, lubridate)

# 5 year (IS) Bayesian Regression ####
# Number of periods should be 42 from 1975-1979 to 2016-2020
# Pre load the funds to use for the test
RealReturns<- readRDS("RealReturns")

# This is to extract the matrix of data for the regression
FundsToUse <- readRDS("Number_of_Fund") # Real Funds Numbers
colnames(FundsToUse)[1] <- c("Funds")

# # 5 years (IS) Bayesian Regression ####
# n <- # NUMBER OF FUNDS RERGESSED
# RealPosteriors5_YearList<- list() # Stores the posterior for each company
# RealPosteriors5_Year <- matrix(nrow = 4000, ncol = 42) # Column per year
# RealMedianAlphas5_Year <- as.data.frame(matrix(nrow = n,ncol = 42))
# BR_IS_Coef01 <- as.data.frame(matrix(nrow = n,ncol = 42))
# BR_IS_Coef02 <- as.data.frame(matrix(nrow = n,ncol = 42))
# BR_IS_Coef03 <- as.data.frame(matrix(nrow = n,ncol = 42))
# BR_IS_Coef04 <- as.data.frame(matrix(nrow = n,ncol = 42))
# #
# n <- NUMBER OF FUNDS RERGESSED
# for (i in 1:n){
#   RealPosteriors5_Year <- matrix(nrow = 4000, ncol = 42) # For company i
#   for (j in 1:42){
#     tempInfo <- na.omit(
#       RealReturns %>%
#         dplyr::filter(Funds == FundsToUse$Funds[i] &
#                         date >= paste(1974+j,"-01-01",sep = "") &
#                         date <= paste(1978+j,"-12-31",sep = "")))
#     if (nrow(tempInfo[,1]) > 12) {
#       set.seed(23)
#       temporaldf <- stan_glm(tempInfo$returnrf ~ ., data= data.frame(tempInfo[,2:5]), refresh = 0)
#       RealMedianAlphas5_Year[i,j] <- temporaldf$coefficients[1]
#       RealPosteriors5_Year[,j] <- insight::get_parameters(temporaldf)[,1]
#       BR_IS_Coef01[i,j] <- temporaldf$coefficients[2]
#       BR_IS_Coef02[i,j] <- temporaldf$coefficients[3]
#       BR_IS_Coef03[i,j] <- temporaldf$coefficients[4]
#       BR_IS_Coef04[i,j] <- temporaldf$coefficients[5]
#     }
#   } # Save posterior probabilities for each fund
#   RealPosteriors5_YearList[[i]]<- RealPosteriors5_Year
# }
# 
# saveRDS(RealMedianAlphas5_Year, file="Bayes_5_Year_Median_Alpha_2")
# saveRDS(RealPosteriors5_YearList,"Bayes_Posteriors_5_Year_List_2")
# saveRDS(BR_IS_Coef01,"BR_IS_Coef01_5_Year_2")
# saveRDS(BR_IS_Coef02,"BR_IS_Coef02_5_Year_2")
# saveRDS(BR_IS_Coef03,"BR_IS_Coef03_5_Year_2")
# saveRDS(BR_IS_Coef04,"BR_IS_Coef04_5_Year_2")
#####


#################################################################################

# # 1 year (OOS) Bayesian Regression ####
# # Number of periods should be 41 from 1980-2020
# # Pre load the funds to use for the test 
# # This is to extract the matrix of data for the regression
# RealPosteriorsPerYearList<- list() # Stores the posterior for each company
# RealPosteriorsPerYear <- matrix(nrow = 4000, ncol = 42) # Column per year
# RealMedianAlphasPerYear <- as.data.frame(matrix(nrow = n,ncol = 42))
# BR_OOS_Coef01 <- as.data.frame(matrix(nrow = n,ncol = 42))
# BR_OOS_Coef02 <- as.data.frame(matrix(nrow = n,ncol = 42))
# BR_OOS_Coef03 <- as.data.frame(matrix(nrow = n,ncol = 42))
# BR_OOS_Coef04 <- as.data.frame(matrix(nrow = n,ncol = 42))
# 
# n <- NUMBER OF FUNDS RERGESSED
# for (i in 1:n){  # Number of funds
#   RealPosteriorsPerYear <- matrix(nrow = 4000, ncol = 42) # For company i
#   for (j in 1:42){ 
#     tempInfo <- na.omit(
#       RealReturns %>% dplyr::filter( Funds == FundsToUse$Funds[i] &
#                                        date >= paste(1979+j,"-01-01",sep = "") &
#                                        date <= paste(1979+j,"-12-31",sep = "")))
#     if (nrow(tempInfo[,1]) > 6) {
#       set.seed(23)
#       temporaldf <-stan_glm(tempInfo$returnrf ~ ., data= data.frame(tempInfo[,2:5]), refresh = 0)
#       RealMedianAlphasPerYear[i,j] <- temporaldf$coefficients[1]
#       RealPosteriorsPerYear[,j] <- insight::get_parameters(temporaldf)[,1]
#       BR_OOS_Coef01[i,j] <- temporaldf$coefficients[2]
#       BR_OOS_Coef02[i,j] <- temporaldf$coefficients[3]
#       BR_OOS_Coef03[i,j] <- temporaldf$coefficients[4]
#       BR_OOS_Coef04[i,j] <- temporaldf$coefficients[5]
#     }
#   } # Save posterior probabilities for each fund
#   RealPosteriorsPerYearList[[i]]<- RealPosteriorsPerYear
# }
# 
# saveRDS(RealMedianAlphasPerYear, file="Bayes_1_Year_Median_Alpha")
# saveRDS(RealPosteriorsPerYearList,"Bayes_Posteriors_1_Year_List_2")
# 
# saveRDS(BR_OOS_Coef01,"BR_OOS_Coef01")
# saveRDS(BR_OOS_Coef02,"BR_OOS_Coef02")
# saveRDS(BR_OOS_Coef03,"BR_OOS_Coef03")
# saveRDS(BR_OOS_Coef04,"BR_OOS_Coef04")





