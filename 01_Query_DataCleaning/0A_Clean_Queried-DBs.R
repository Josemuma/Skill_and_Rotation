# First script to clean databases that were created or downloaded from 
# CRSP in WRDS from Matlab query

library(pacman)
p_load(RPostgres, dplyr)

# Use this in case you collect data from CRSP (WRDS) ####
wrds <- dbConnect(Postgres(),host='FILL HOST',
                  port='FILL PORT', dbname='FILL DBNAME', 
                  sslmode='FILL sslmode',user='FILL USER')
#####

# Load retrieved data ####
FourFactors <- read_csv("LOCATION") # Load Fama French 4 factors for regressions 
FourFactors<- (FourFactors %>% dplyr::select(2:4,6:7,5))
colnames(FourFactors)[5] <- "date"
# saveRDS(FourFactors,"FamaFactors") # Save df for future use as R object

# View FF df
View(readRDS("FamaFactors.rds"))

# Load Returns and funds queried through Matlab from folder
returns <- read_csv("LOCATION") # Load fund returns and numbers for regressions 
colnames(returns)[2] <- "date"

# Join FF with returns and fund number
FundsAndReturns <- left_join(returns, FourFactors, by = "date") %>% 
  mutate(returnrf = .[[3]]-.[[8]]) %>% dplyr::select(9,4:7,1:2,8)
colnames(FundsAndReturns)[6] <- 'Funds'
# Save them for future use
# saveRDS(FundsAndReturns,"FundsAndReturns.rds")

#Funds to use
FundsToUse <- tibble(data.frame(subset(table(returns$crsp_fundno), 
                                       table(returns$crsp_fundno) > 130)))
uniqueFunds <-tibble(data.frame(as.numeric(as.character(FundsToUse$Var1))))
colnames(uniqueFunds) <- "Funds"

# Dates
DatesFile <- data.frame(sort(unique(FundsAndReturns$date)))
colnames(DatesFile) <- "date"
# saveRDS(DatesFile,"DatesFile")

# The age of the funds ####
# FundsYears <- sapply(1:1400, function(x) 
#   RealReturns %>% filter(Funds == FundsToUse[x,1]) %>% 
#     dplyr::select(Funds, date) %>% 
#     summarise(as.Date(last(.[[2]]))-as.Date(first(.[[2]])))/365.75)
# FundsYears <- (cbind(FundsToUse$Funds,do.call(rbind.data.frame, FundsYears)))
# colnames(FundsYears) <- c('Funds','Years')
# head(FundsYears)
# saveRDS(FundsYears, file = "Funds_Years")

