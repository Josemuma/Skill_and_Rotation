# Yearly regression #####
# This script is to run regressions on a yearly basis. These results are to help
# me estimate the OOS and use it to compare with the detected funds in IS period
library(pacman)
p_load(dplyr,tidyr)

RealReturns<- readRDS("RealReturns")
FundsToUse <- readRDS("Number_of_Fund") # Real Funds Numbers
colnames(FundsToUse)[1] <- c("Funds")

# Per 5 year Period Regressions OLS #####
n <- 1400

OSLAlphasOOS <-  as.data.frame(matrix(nrow = n,ncol = 42))
OLSpValueOOS <-  as.data.frame(matrix(nrow = n,ncol = 42))

for(i in 1:n){
  for(j in 1:42){ 
    print((i)+(j/100))
    tempData <-na.omit(
      RealReturns %>% filter(
        Funds  == FundsToUse$Funds[i] &
        date >= paste(1979+j,"-01-01",sep = "") &
          date <= paste(1979+j,"-12-31",sep = "")))

    if(nrow(tempData[,1]) > 6 ){
      tempDataSum <- summary(lm(tempData$returnrf ~., data = data.frame(tempData[,2:5])))
      OSLAlphasOOS[i,j] <- tempDataSum$coefficients[1,1]
      OLSpValueOOS[i,j] <- tempDataSum$coefficients[1,4]
    }
  }
}

# saveRDS(OSLAlphasOOS,file = "OLS_Alphas_1_Year")
# saveRDS(OLSpValueOOS,file = "OLS_pValue_1_Year")

## Rename and modify ####
# Alpha
OLS_Alphas_1_Year <- as_tibble(bind_cols(FundsToUse$Funds[1:n],OSLAlphasOOS))
colnames(OLS_Alphas_1_Year) <- c('Funds',paste0(rep(LETTERS[25],41),1980:2021))
# saveRDS(OLS_Alphas_1_Year,file = "OLS_Alphas_1_Year")

# P-value
OLS_pValue_1_Year <- as_tibble(bind_cols(FundsToUse$Funds[1:n],OLSpValueOOS))
colnames(OLS_pValue_1_Year) <- c('Funds',paste0(rep(LETTERS[25],41),1980:2021))
# saveRDS(OLS_pValue_1_Year,file = "OLS_pValue_1_Year")
