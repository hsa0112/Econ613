
# # -------------------------------------------------------
# # ------ 5 - Panel Data: Individual Fixed Effects -------
# # -------------------------------------------------------
# 
# # 5.1 Between estimation
# 
# between_data <- final_panel %>% as.data.table()
# between_data <- between_data[,paste('avg', names(between_data)[4:22], sep="_") :=lapply(.SD,mean),
#                              by=NUID,.SDcols=4:22] %>% as.data.frame()
# bet_model <- lm(data=between_data,avg_arrest~avg_tenure+avg_tot_crim_per_res+avg_p50_inc+
#                   avg_share_white+avg_share_hisp+avg_share_black) 
# 
# 
# 
# # 5.2 Within estimation
# 
# within_data <- left_join(final_panel %>% 
#                            dplyr::select(NUID,crime_month,arrest,tenure,tot_crim_per_res,
#                                          p50_inc,tot_white,tot_hisp,tot_black),between_data)
# within_data <- within_data %>% mutate(
#   w_arrest=arrest-avg_arrest,
#   w_tenure=tenure-avg_tenure,
#   w_tot_crim_per_res=tot_crim_per_res-avg_tot_crim_per_res,
#   w_p50_inc=p50_inc-avg_p50_inc,
#   w_share_white=share_white-avg_share_white,
#   w_share_hisp=share_hisp-avg_share_hisp,
#   w_share_black=share_black-avg_share_black
# )
# 
# # within_model <- lm(data=within_data,w_arrest~w_tenure+w_tot_crim_per_res+w_p50_inc+
# # w_tot_white+w_tot_hisp+w_tot_black) 
# 
# # 5.3 First Difference
# subset_panel<- final_panel %>% dplyr::select(NUID,crime_month,arrest,tenure,tot_crim_per_res,
#                                              p50_inc,share_white,share_hisp,share_black) %>% 
#   mutate(NUID=as.numeric(NUID))
# ordered_panel <- subset_panel[order(subset_panel$NUID,subset_panel$crime_month),]
# diff_panel <- NULL
# 
# 
# min_month <- ordered_panel %>% group_by(NUID) %>% summarise(crime_month=min(crime_month))
# min_month <- min_month %>% mutate(drop=1)
# 
# diff_panel <- ordered_panel[,3:9]-dplyr::lag(ordered_panel[,3:9])
# diff_panel <- cbind(ordered_panel[,1:2],diff_panel)
# diff_panel <- left_join(diff_panel,min_month)
# diff_panel <- diff_panel %>% filter(is.na(drop))
# diff_panel <- diff_panel %>% dplyr::select(-drop)
# 
# firstdiff_model <- lm(data=diff_panel, arrest~tenure+tot_crim_per_res+p50_inc+
#                         share_white+share_hisp+share_black)
# 
# stargazer(bet_model,within_model,firstdiff_model)
# 

library(AER)
library(ggplot2)
library(texreg)
library(plm)

data("Guns")
names(Guns)
Guns$group = substr(Guns$state,1,1)
table(Guns$group)



data("Produc", package="plm")

formula = log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp

model1 = plm(formula, data=Produc, model="within")
model2 = plm(formula, data=Produc, model="between")
model3 = plm(formula, data=Produc, model="fd")

summary(model1)

