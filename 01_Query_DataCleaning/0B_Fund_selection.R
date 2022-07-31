# Funds selection ####
library(pacman)
p_load(dbplyr,RPostgres,dplyr, lubridate)
  ## Load Data ####
  # Fama French
FourFactors <- readRDS("LOCATION") # Load FF factors for regressions 
FourFactors<- (FourFactors %>% filter(date >= '1975-01-01') %>% 
                 dplyr::select(2:4,8,5,9)) 
colnames(FourFactors)[6] <- "date"
FourFactors

  # Funds
  # Use summary DB to filter by style, among other conditions
wrds_summary <- tibble(readRDS("~/FILE NAME"))
colnames(wrds_summary)
wrds_summary %>% dplyr::select(1:2,4)

wrds_summary2 <- tibble(readRDS("~/FILE NAME"))
wrds_summary2 # 691,438
colnames(wrds_summary2)

wrds_retTNAnav <- tibble(readRDS("~/FILE NAME"))
wrds_retTNAnav # 8,683,861
colnames(wrds_retTNAnav)

wrds_portnomap <- tibble(readRDS("~/FILE NAME"))
wrds_portnomap
colnames(wrds_portnomap)

wrds_Hist <- tibble(readRDS("~/FILE NAME"))
wrds_Hist # Funds = 67,177 ; Port = 27,136 ; cl_group = 26,156

    ### 1. Returns ####
    # FundTNA >= 50,
    # date start in 1975 until 2021,
    # must have at least 60 returns 

# Filter_Returns_TNA <- 
filter_retTNAnav <- wrds_retTNAnav %>% filter(caldt >= '1975-01-01' & caldt <= '2021-12-31') %>% 
  filter(mtna >= 50 & !is.na(mtna)) %>% 
  filter(!is.na(mnav)) %>% filter(!is.na(mret))
filter_retTNAnav

Filter_Returns_TNA <- filter_retTNAnav %>% group_by(crsp_fundno) %>% 
  summarise(n = n()) %>% filter(n >= 60)
Filter_Returns_TNA

    ### 2. Intersect summary2 with the funds that have  ####
    # the minimum number of returns. 
summary2_with_Returns <- left_join(Filter_Returns_TNA, wrds_summary2, 
                                 by = 'crsp_fundno') # 350,745
summary2_with_Returns

    ### 3. Clean summary2_with_Returns ####
    # Needs to have a crsp_portno,
    # Equity, open to invest, no accrual, 
    # CRSP_objective Equity(E)-Domestic(D)-Style(Y)-Growth(G)|Growth & Income(B),
    # lipper_objective Growth Funds (G) | Growth & Income Funds (GI),
    # no sales restrictions, is NOT ETN | ETF,
    # is not Indexed-Based(B) | Pure-Index(D) | Index-Enhanced(E),
    # it must have a TICKER, add or not caldt

filter_summary2 <- distinct(
  summary2_with_Returns %>%
    filter(!is.na(crsp_portno)) %>% 
    filter(lipper_asset_cd == "EQ") %>% 
    filter(open_to_inv == "Y" ) %>% 
    filter(accrual_fund == "N" ) %>% 
    filter(crsp_obj_cd=="EDYG" |crsp_obj_cd== "EDYB") %>%
    filter(lipper_obj_cd == "GI"|lipper_obj_cd=="G")  %>% 
    filter(sales_restrict == 'N') %>%
    filter(is.na(et_flag)) %>%
    filter(is.na(index_fund_flag)) %>% 
    filter(!is.na(ticker)) %>% 
    # filter(dead_flag == "Y") %>% 
    filter(delist_cd != "M" | is.na(delist_cd)) %>%
    select(crsp_fundno,n,crsp_portno,ticker, first_offer_dt, 
           dead_flag))
(filter_summary2) 

      ### 4. Portfolio numbers unique ####
Fund_No <- tibble(crsp_fundno = unique(filter_summary2$crsp_fundno))
Fund_No # 3,461

# The above filter is the preferred to not repeat portfolios of stocks
# Portf_Ticker <- tibble(Portf_Ticker = unique(filter_summary2$ticker))
# length(Portf_Ticker$Portf_Ticker) # 3,594

    ### 5. Re-link with Returns, TNA, etc ####

Return_and_Fund_No <- right_join(filter_retTNAnav, 
          Fund_No, by = 'crsp_fundno' )
length(unique(Return_and_Fund_No$crsp_fundno))
colnames(Return_and_Fund_No)[1] <- 'date'
Return_and_Fund_No

    ### 5.1 Joing with FF ####
RealReturns <- left_join(Return_and_Fund_No, FourFactors, by = 'date') %>% 
  mutate(returnrf = mret - rf) %>% rename(Funds = crsp_fundno) %>% 
  dplyr::select(returnrf, mktrf, smb, hml, umd, Funds, date, rf, mtna, mnav)

    ### 6. Save files to use
# saveRDS(Fund_No, file = 'Number_of_Fund')
# saveRDS(FourFactors, file = 'FourFactors')
# saveRDS(RealReturns, file = 'RealReturns')


# Delete DB to make r file lighter 
rm(wrds_summary,wrds_summary2,wrds_retTNAnav,wrds_portnomap,
   wrds_fees,wrds_Hist, borrar )
