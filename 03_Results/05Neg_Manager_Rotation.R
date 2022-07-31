# Change the working directory based on the computer from Desktop to Documents
# Load the DB with manager rotation information 
# wrds_summary <- readRDS("~/FILE")
# wrds_retTNAnav <- readRDS("~/FILE")
# wrds_hdr <- readRDS("~/FILE")
# colnames(wrds_hdr)[2] <- c('Funds') #rename column 
wrds_hdrHist <- readRDS("~/FILE")
colnames(wrds_hdrHist)[2] <- c('Funds') # rename column
wrds_hdr <- readRDS("~/FILE")
colnames(wrds_hdr)[2] <- c('Funds') # rename column
# For reference: 
# mgr_dt: Date that the current portfolio manager assumed responsibility
# for the portfolio Data available beginning December 1993
# fhadv_name: Data available beginning December 1999
# chgdt: date that the header information was active

# The first period to start in the IS analysis is from 1993 to 1998 
# From our lists we start looking at funds where x = 19
# BayIS_SB[[x]][[1]][1] #[[Period x]][[Funds]][[Fund i]]

# Careful as the stored values can be empty if it didn't find any changes in 
# that particular period. But it can be possible that there are changes in 
# another period.

# Managers in IS ####
# 5 year periods funds
# We found the information of managers for each of the funds found in the 
# 5 year period (Detected funds)
manager_list_neg <- lapply(19:41, function(x)
  lapply(1:nrow(list_detected_neg[[x]]), function(i) 
    wrds_hdrHist %>% filter(Funds == list_detected_neg[[x]][[1]][i]) %>% 
      filter(mgr_dt >= paste(1974+x,'-01-01',sep = "") & 
               mgr_dt <= paste(1978+x,'-12-31',sep = "")) %>% 
      dplyr::select(Funds, mgr_name,mgr_dt, chgdt, chgenddt,adv_name)))
manager_list_neg # 23 lists
# Counting Change of managers ####
# Create a table with number of managers during the 5 year period in column 2
# in column 3 the number of managers in OOS period, in column 4 put if that 
# fund is part of the skilled funds or not. This is in the document

# Number of managers of Fund i in the OOS period
# The year to be reviewed starts in January 1998 to December 1998
# If number of rows = 0 that means that there was no change in the manager(s)
# during that OOS (1 year) period, i.e. it's the same as the manager in IS.

# Do not need to re-run this ####
# Just load the table as below if it has been run before
Bay_Mgr_Table_neg <- readRDS("Bay_Mgr_Table_neg") #####
# We create a list of lists, where each list is a different period. It only has
# information of managers, therefore the list starts in 1993-1997 and there will
# be only 23 periods. To start searching for funds from our previous lists, we 
# need to start in the position 19. 
# For each period t, we count the number of changes of managers in each Fund. 
# We start with the number of changes in the 5year period, then in the OOS and 
# last in the OOS + 1. If there is a change register in the last day of the year
# we consider it as part of the next year, therefore it filter stops in month 11.

# m = length(manager_list_neg) # Number of periods starting in 1993-1997
# Bay_Mgr_Table <- lapply(19:(19 + m -1), function(t)
#   tibble(bind_cols(
#     Funds = sapply(1:nrow(list_detected_neg[[t]]), function(i)
#       list_detected_neg[[t]][[1]][[i]]),
# 
#     IS_Period = sapply(1:nrow(list_detected_neg[[t]]), function(i)
#       nrow(wrds_hdrHist %>%
#              dplyr::select(Funds,mgr_name,mgr_dt,chgdt,chgenddt,adv_name) %>%
#              filter(Funds == list_detected_neg[[t]][[1]][[i]] &
#                       chgdt >= paste(1974+t,'-01-01',sep = "") &
#                       chgdt <= paste(1978+t,'-11-30',sep = "")))),
# 
#     OOS = sapply(1:nrow(list_detected_neg[[t]]), function(i)
#       nrow( wrds_hdrHist %>%
#               dplyr::select(Funds,mgr_name,mgr_dt,chgdt,chgenddt,adv_name) %>%
#               filter(Funds == list_detected_neg[[t]][[1]][[i]] &
#                        chgdt >= paste(1978+t,'-12-01',sep = "") &
#                        chgdt <= paste(1979+t,'-11-30',sep = "")))),
# 
#     OOS_Plus1 = sapply(1:nrow(list_detected_neg[[t]]), function(i)
#       nrow( wrds_hdrHist %>%
#               dplyr::select(Funds,mgr_name,mgr_dt,chgdt,chgenddt,adv_name) %>%
#               filter(Funds == list_detected_neg[[t]][[1]][[i]] &
#                        chgdt >= paste(1978+t+1,'-12-01',sep = "") &
#                        chgdt <= paste(1979+t+1,'-11-30',sep = ""))))
#   )))
# saveRDS(Bay_Mgr_Table_neg,file = "Bay_Mgr_Table_neg") ####

# Average number of changes of managers per period for positive alphas
# Tibble with average number of managers per period studied
# It shows 1998, as it is where we start the OOS analysis. The first 5 year
# period is from 1993-1998.
# Table with total of rotation per period in IS, OOS, OOS_P1
## Manager Tot Changes ####
manager_total_changes_n <- sapply(1:(length(Bay_Mgr_Table_neg)), function(x)
  colSums(Bay_Mgr_Table_neg[[x]][,2:4]))
# Transpose
manager_total_changes_n <- as_tibble(cbind(nms = names(manager_total_changes_n), 
                                          t(manager_total_changes_n)))
# Add periods (years)
manager_total_changes_n <- tibble(cbind(Periods = 1998:2020,
                                         manager_total_changes_n))
manager_total_changes_n # Total of rotation per period

# Table of average rotation per period IS, OOS, OOS_P1 ####
manager_mean_n <- tibble(bind_cols(
  Periods = 1998:2020,
  t(sapply(1:(length(Bay_Mgr_Table_neg)), function(x) 
    colMeans(Bay_Mgr_Table_neg[[x]][,2:4])))))
#####
manager_mean_n %>% print(n = Inf) # Print all periods
colMeans(manager_mean_n[,2:4]) # Average rotations per period

# Skilled Funds Manager rotation ####
# This part is to create a table that only deals with those funds that appeared
# in the 5 year period and OOS (skilled). 
# There are only 22 periods because the period 2016-2020 can only see
# OOS = 2021 but 00S + 1 = 2022 cannot be checked, therefore we reduce the 
# periods to only 22.
# We use Bay_Mgr_Table_neg that was previously created. 
# 18 + t because that would be 1998
manager_pers_skill_n <- lapply(1:(length(Bay_Mgr_Table_neg)), function(t)
  right_join(Bay_Mgr_Table_neg[[t]],
             list_detected_neg[[18 + t]] %>% filter(.[[3]] <= 0.05), 
             by  = 'Funds'))
manager_pers_skill_n[[6]] # Example should show 3
# Mean
colMeans(tibble(t(sapply(1:length(Bay_Mgr_Table_neg), function(x)
  colMeans((manager_pers_skill_n[[x]])[,c(2:13,15:17,19:21)],na.rm = T)))),na.rm = T)
# Occasional skilled funds rotation ####
manager_occa_skill_n <- lapply(1:(length(Bay_Mgr_Table_neg)), function(t)
  right_join(Bay_Mgr_Table_neg[[t]],
             list_detected_neg[[18 + t]] %>% filter(.[[3]] > 0.05), 
             by  = 'Funds'))
manager_occa_skill_n[[6]] # Example should show 67
# Mean
colMeans(tibble(t(sapply(1:length(Bay_Mgr_Table_neg), function(x)
  colMeans((manager_occa_skill_n[[x]])[,c(2:13,15:17,19:21)],na.rm = T)))),na.rm = T)
# Continuous persistent skill ####
manager_contin_pers_skill_n <- lapply(1:(length(Bay_Mgr_Table_neg)), function(t)
  right_join(Bay_Mgr_Table_neg[[t]],
             list_detected_neg[[18 + t]] %>% filter(.[[3]] <= 0.05 & .[[4]] <= 0.05), 
             by  = 'Funds'))
manager_contin_pers_skill_n[[6]]
# Mean
colMeans(tibble(t(sapply(1:length(Bay_Mgr_Table_neg), function(x)
  colMeans((manager_contin_pers_skill_n[[x]])[,c(2:13,15:17,19:21)],na.rm = T)))),na.rm = T)
# Occasional persistent skill ####
manager_occas_pers_skill_n <- lapply(1:(length(Bay_Mgr_Table_neg)), function(t) 
  right_join(Bay_Mgr_Table_neg[[t]],
             list_detected_neg[[18 + t]] %>% filter(.[[3]] <= 0.05 & .[[4]] > 0.05), 
             by  = 'Funds'))
manager_occas_pers_skill_n[[6]]
# Mean
colMeans(tibble(t(sapply(1:length(Bay_Mgr_Table_neg), function(x)
  colMeans((manager_occas_pers_skill_n[[x]])[,c(2:13,15:17,19:21)],na.rm = T)))),na.rm = T)
# Bounce skill ####
manager_bounce_skill_n <- lapply(1:(length(Bay_Mgr_Table_neg)), function(t) 
  right_join(Bay_Mgr_Table_neg[[t]],
             list_detected_neg[[t + 18]] %>% filter(.[[3]] > 0.05 & .[[4]] <= 0.05), 
             by  = 'Funds'))
manager_bounce_skill_n[[6]]
# Mean
colMeans(tibble(t(sapply(1:length(Bay_Mgr_Table_neg), function(x)
  colMeans((manager_bounce_skill_n[[x]])[,c(2:13,15:17,19:21)],na.rm = T)))),na.rm = T)
# Drop skill
manager_drop_skill_n <- lapply(1:(length(Bay_Mgr_Table_neg)), function(t) 
  right_join(Bay_Mgr_Table_neg[[t]],
             list_detected_neg[[t + 18]] %>% filter(.[[3]] > 0.05 & .[[4]] > 0.05), 
             by  = 'Funds'))
manager_drop_skill_n[[6]]
# Mean
colMeans(tibble(t(sapply(1:length(Bay_Mgr_Table_neg), function(x)
  colMeans((manager_drop_skill_n[[x]])[,c(2:13,15:17,19:21)],na.rm = T)))),na.rm = T)


## Plot of number of rotation per period ALL ####
# Plots with average rotation for IS, OOS, OOS+1 #### 
# ggsave(filename = "Avg_Rotation_perPeriod.png", 
#      ggplot(reshape2::melt(Bay_Mgr_Table_Mean, id.var = "Periods"), 
#       aes(Periods,value,colour = variable))+
#  geom_line(aes(linetype=variable, group = variable)) +
#  # geom_hline(yintercept = mean(Bay_Mgr_Table_Mean$IS_Period), color="black") +
#  # annotate("text", x=3, y=3.3, label=" Average \n Rotation = 2.75", color = "black") +
#  scale_linetype_manual("", values=c("solid", "dashed", "dotted"),
#                        labels = c("Detection \n period", "OOS period","OOS t + 1\n period")) +
#  theme_classic()+
#  theme(axis.text.x = element_text(angle = 90, vjust = 0.9, hjust=1.5))+
#  labs(x = "Year", y = "Average Number of Managerial Changes") +
# scale_color_manual("",labels = c("Detection \n period", "OOS period","OOS t + 1\n period"),
#                     values = c("steelblue3","red2","darkgreen")) +
#  theme(legend.position = "bottom",legend.text = element_text(size = 8), 
#        axis.text = element_text(size = 8),
#        axis.title.x = element_text(size = 11),
#        axis.title.y = element_text(size = 11),legend.title=element_blank())
# ,width = 10, height = 4, dpi = 300, units = "in", device='png')

# Plots with total number of rotation for IS, OOS, OOS+1 #### 
# ggsave(filename = "Number_Of_Rotation_perPeriod.png", 
# ggplot(reshape2::melt(Bay_Mgr_Table_Per_Period, id.var = "Periods"), 
#        aes(Periods,value,colour = variable))+
#   geom_line(aes(linetype=variable, group = variable)) +
#   # geom_hline(yintercept = mean(Bay_Mgr_Table_Per_Period$IS_Period), color="black") +
#   # annotate("text", x=3, y=3.3, label=" Number of \n Rotations = 2.75", color = "black") +
#   scale_linetype_manual("", values=c("solid", "dashed", "dotted"),
#                         labels = c("Detection \n period", "OOS period","OOS t + 1\n period")) +
#   theme_classic()+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.9, hjust=1.5))+
#   labs(x = "Year", y = "Total Number of \n Managerial Changes") +
#   scale_color_manual("",labels = c("Detection \n period", "OOS period","OOS t + 1\n period"),
#                      values = c("steelblue3","red2","darkgreen")) +
#   theme(legend.position = "bottom",legend.text = element_text(size = 8), 
#         axis.text = element_text(size = 8),
#         axis.title.x = element_text(size = 11),
#         axis.title.y = element_text(size = 11),legend.title=element_blank())
# ,width = 10, height = 4, dpi = 300, units = "in", device='png')

# Plots 
# No. of detected funds in 5 year period and avg. rotation of managers  #####
# in 5 year period
rm(dataForTable)
dataForTable <- data.frame(bind_cols(Bay_Mgr_Table_Mean[,1:2],
                                     DetecTab_95$Bayes[20:41]))
colnames(dataForTable) <- c('Periods','Managers', 'ISPeriod')
dataForTable
ggsave(filename = "Detected_and_AvgRotMgr.png",
       ggplot(dataForTable, aes(x = Periods))+
         geom_line(aes(y = ISPeriod, group = 1, 
                       linetype = "No. of funds detected per \n 5 year period"), color="red2") +
         geom_line(aes(y = Managers*20, group = 2, 
                       linetype = "Rotation of managers per \n 5 year period"), color="red2") +
         scale_y_continuous(
           sec.axis = sec_axis(~ .*20, labels = number_format(scale=1/200),
                               name='Rotation of managers per \n 5 year period \n(positive alpha)'))+
         theme_classic() +
         labs(x = "Year", y = "No. of funds detected per \n 5 year period \n(positive alpha)", color = "Legend") +
         theme(axis.text.x = element_text(angle = 90, vjust = 0.9, hjust=1.5))+
         theme(legend.position = "bottom",legend.text = element_text(size = 8),
               axis.text = element_text(size = 8),
               axis.title.x = element_text(size = 11),
               axis.title.y = element_text(size = 11),legend.title=element_blank())
       ,width = 10, height = 4, dpi = 300, units = "in", device='png')

# Delete db to reduce size rm(wrds_hdrHist,wrds_retTNAnav,wrds_summary)





