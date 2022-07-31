# 5 Year regression #####
# This script is to run regressions on a 5 year period basis. These results are to help
# me estimate the IS and use it to compare with the detected funds in OOS period
library(pacman)
p_load(dplyr,tidyr)

RealReturns<- readRDS("RealReturns")
FundsToUse <- readRDS("Number_of_Fund") # Real Funds Numbers
colnames(FundsToUse)[1] <- c("Funds")

# Per 5 year Period Regressions OLS #####
n <- length(FundsToUse$Funds)
OLSpValueIS <-  as.data.frame(matrix(nrow = n,ncol = 42))
OSLAlphasIS <-  as.data.frame(matrix(nrow = n,ncol = 42))

for(i in 1:n){ #1:n
  for(j in 1:42){ 
    print((i)+(j/100))
    tempData <-na.omit(
      RealReturns %>% dplyr::filter(
        Funds  == FundsToUse$Funds[i] &
          date >= paste(1974+j,"-01-01",sep = "") &
          date <= paste(1978+j,"-12-31",sep = "")))

    if(nrow(tempData[,1]) > 12){
      tempDataSum <- summary(lm(tempData$returnrf ~., data = data.frame(tempData[,2:5])))
      OSLAlphasIS[i,j] <- tempDataSum$coefficients[1,1]
      OLSpValueIS[i,j] <- tempDataSum$coefficients[1,4]
    }
  }
}
saveRDS(OSLAlphasIS,file = "OLS_Alphas_5_Year")
saveRDS(OLSpValueIS,file = "OLS_pValue_5_Year")

## Rename and modify ####
# Alpha
OLS_Alphas_5_Year <- as_tibble(bind_cols(FundsToUse$Funds,OSLAlphasIS))
colnames(OLS_Alphas_5_Year) <- c('Funds',paste0(rep(LETTERS[25],41),1975:2017,1979:2021))
saveRDS(OLS_Alphas_5_Year,file = "OLS_Alphas_5_Year")

# P-value
OLS_pValue_5_Year <- as_tibble(bind_cols(FundsToUse$Funds,OLSpValueIS))
colnames(OLS_pValue_5_Year) <- c('Funds',paste0(rep(LETTERS[25],41),1975:2017,1979:2021))
saveRDS(OLS_pValue_5_Year,file = "OLS_pValue_5_Year")










