#####Cfpb model

cat("\014") #clears screen
graphics.off() # clears old plots
rm(list=ls()) 
start_time <- Sys.time()


# LOAD REQUISITE DIRECTORIES ----------------------------------------------

library(Hmisc)
library(stringr)
library(lubridate)
library(xlsx)
library(dplyr)
library(gbm)
library(XLConnect)
library(caTools)
library(ROCR)
library(data.table)
library(Matrix)


# CONTROL/INPUT VARIABLES -------------------------------------------------

# date variables

# training variables

# data pull variables

# model parameters

# program control variables

# RODBC parameters


lib_ld_time <-  Sys.time()
print("libraries loaded in",lib_ld_time-start_time )


# PULL RELEVANT DATA ------------------------------------------------------

#uploading ICW complaint loan 
#complaint_data <- read.csv("IV_WOE_data/complaint_data_aug16-jul17_without_inquiry_final.csv")
#complaint_data_with_inq <- read.csv("IV_WOE_data/1 comp_loans_aug_16_jan_17.csv")


#Uploading ICW data
icw_data <- read.csv("IV_WOE_data/icase_loan.csv")

# Uploading Demographics data
demographic_data <- read.csv("IV_WOE_data/3 qrm_cfpb_loan_dem_det.csv")

# uploading cfpb loan data
cfpb_loan_data <- read.csv("cfpb_data_aug16_jan17.csv")


# uploading cfpb loan data
prev_comp_data <- read.csv("prev_complaint_flag.csv")

#creating a new column dependent variable, setting it to 1

cfpb_loan_data$cfpb_loan <- 1
prev_comp_data$prev_comp_flag <- 1
icw_data$prev_icw_comp <- 1

mod_aprvl_data <- read.csv("IV_WOE_data/mod_approval_apr_2015_jul_2016.csv")
mod_denyl_data <- read.csv("IV_WOE_data/mod_denial_apr_2015_jul_2016.csv")
fpi_data <- read.csv("IV_WOE_data/fpi_final.csv")
call_data <- read.csv("calls_apr_15_jul_16.csv")
day_diff_data <- read.csv("loan_date_diff_cfpb.csv")
act_complt_date_diff_data <- read.csv("act_complt_date_diff_train.csv")
incative_loan_date_diff_data <- read.csv("incative_loan_date_diff.csv")

max(act_complt_date_diff_data$ACTV_DATE_DIFF)

actv_vali_data <- read.csv('act_complt_date_diff_validation.csv')
inactv_vali_data <- read.csv('cfpb_val_date_diff_inactv.csv')


# DATA PROCESSING ---------------------------------------------------------

# Cleaning the data

for (i in  ls(pattern= "_data") ){
  temp_data <- get(i)
  
  # convert column names to lower case
  names(temp_data) <- str_replace_all(string = tolower(names(temp_data)), pattern = "[[:punct:]]+", replacement = "_")
  
  
  # convert POSIXT columns to Date
  for (j in  names(temp_data) [sapply(temp_data, is.POSIXt)]){
    temp_data[,j] <- as.Date(temp_data[,j])
  }
  
  # convert factors to character
  for (j in names(temp_data) [sapply(temp_data, is.factor)]){
    temp_data[,j] <- as.character(temp_data[,j])
  }
  
  # convert character to upper
  for (j in names(temp_data) [sapply(temp_data, is.character)]){
    temp_data[,j] <- toupper(temp_data[,j])
    temp_data[,j] <- str_replace_all(string = temp_data[,j], pattern = " ", replacement = "")
  }
  #update the _data files with the new data with above 4 changes.
  assign(x = i,value = temp_data)
  rm(list=ls(pattern= "temp")) # remove the temp frame
}


#length(unique(demographic_data$loan_num))

final_data <- arrange(final_data, desc(max_effct_dt))
icw_data <- icw_data[!duplicated(icw_data$loan_num),] 
call_data <- call_data[!duplicated(call_data$loan_num), ] 

demo_icw_data <-merge(x=demographic_data, y=icw_data, by = "loan_num",all.x = TRUE)
###Merging the above data
final_data <- merge(x=demo_icw_data, y=complaint_data,by = "loan_num", all.x = TRUE)

final_data <- merge(x=demo_icw_data, y=mod_aprvl_data,by = "loan_num", all.x = TRUE)
final_data <- merge(x=final_data, y=mod_denyl_data,by = "loan_num", all.x = TRUE)
final_data <- merge(x=final_data, y=fpi_data,by = "loan_num", all.x = TRUE)
final_data <- merge(x=final_data, y=call_data,by = "loan_num", all.x = TRUE)
final_data <- merge(x=final_data, y=cfpb_loan_data,by = "loan_num", all.x = TRUE)
final_data <- merge(x=final_data, y=prev_comp_data,by = "loan_num", all.x = TRUE)
#final_data$date_diff <- as.Date("31-JUL-16", "%d-%B-%y") - as.Date(final_data$max_effct_dt, "%d-%B-%y") 
final_data <- merge(x=final_data, y=day_diff_data,by = "loan_num", all.x = TRUE)
final_data <- merge(x=final_data, y=act_complt_date_diff_data,by = "loan_num", all.x = TRUE)
final_data <- merge(x=final_data, y=incative_loan_date_diff_data,by = "loan_num", all.x = TRUE)




#Replacing null/NA values
final_data[is.na(final_data)] <- 0
final_data[is.null(final_data)] <- 0


####Calcualting Weight Of Evidence and Information Value ###


for(i in names(final_data)[(!names(final_data) %in% c("dependent_variable" ,"loan_num", "intrate", "orgprinbal", "curprinbal", "pipmtamt", "itchrgdue", "curr_ltv", "mv_curr_low", "fico", "sum_eff_dt_diff"))]){
  temp_df <- final_data%>%
    group_by_(i) %>% 
    summarize(total_loans = n(), 
              good_loans = sum(dependent_variable), 
              bad_loans = total_loans - good_loans)  %>%
    mutate(per_bad_loan = bad_loans/total_loans,
           distrbn_loans = total_loans/sum(total_loans),
           distrbn_good_loans = good_loans/sum(good_loans),
           distrbn_bad_loans = bad_loans/sum(bad_loans),
           WOE = log(distrbn_good_loans/(distrbn_bad_loans+1e-16) +  1e-16 ),
           diff_of_DG_DB = distrbn_good_loans-distrbn_bad_loans,
           IV = WOE*diff_of_DG_DB)
  
  
  writeWorksheetToFile(file = "WOE_output_final.xlsx", data = temp_df, sheet = i)
}




#### Remove columns with IV less than 20%

# final_data$loan_num_a <- NULL
# final_data$rnk <- NULL
# final_data$max_effct_dt <- NULL
# final_data$min_effct_dt <- NULL
# final_data$loan_num_dept_most <- NULL
# final_data$loantyp <- NULL
# final_data$inttype <- NULL
# final_data$loanstat <- NULL
# final_data$lienpos <- NULL
# final_data$stdm <- NULL
# final_data$stse <- NULL
# final_data$stnp <- NULL
# final_data$owner_occp_status <- NULL
# final_data$cn_case_bu_first <- NULL
# final_data$cn_case_typ_first <- NULL
# final_data$dcn_isu_cat_last <- NULL
# final_data$dcn_case_bu_last <- NULL
# final_data$cn_case_typ_count_sum <- NULL
# final_data$dcn_case_bu_count_sum <- NULL
# final_data$cn_case_isus_rslvd_flg_sum <- NULL
# final_data$dcn_case_rgltry_body_flg_sum <- NULL
# final_data$dcn_case_cntc_auth_tp_flg_sum <- NULL
# final_data$dcn_cas_isus_rslvd_flg_sum <- NULL
# final_data$abk_flag_latest_sum <- NULL
# final_data$alit_flag_latest_sum <- NULL
# final_data$advo_flag_latest_sum <- NULL
# final_data$bra_flag_latest_sum <- NULL
#final_data$dcn_case_typ_last <- NULL


##### Categorize extra categories into Others ###

final_data$stfc[which(final_data$stfc == "")] <- "OTHERS"
final_data$stfb[which(final_data$stfb == "")] <- "OTHERS"
final_data$stre[which(final_data$stre == "")] <- "OTHERS"
final_data$stbk[which(final_data$stbk == "")] <- "OTHERS"
final_data$dcn_isu_cat_most[which(final_data$dcn_isu_cat_most == "")] <- "OTHERS"

final_data$case_who_cntced_us_last_flag[which(final_data$case_who_cntced_us_last_flag == "")] <- "OTHERS"
final_data$case_res_cd_latest[which(final_data$case_res_cd_latest == "")] <- "OTHERS"

final_data$cn_case_typ_most[which(final_data$cn_case_typ_most %in% c("BORROWERCOMPLAINT-GENERAL", 
                                                                     "INTERNALESCALATION", 
                                                                     "PAYMENTNOTAPPLIEDCORRECTLY", 
                                                                     "REGULATORYINQUIRY", 
                                                                     "CEASE&DESIST", 
                                                                     "CLAIMSLOANPAIDINFULL", 
                                                                     "TAXESNOTPAID,NOTESCROWED"))] <- "OTHERS"


final_data$case_res_cd_latest[which(final_data$case_res_cd_latest %in% c("UNABLETOCONTACT",
                                                                         "DUPLICATIVE",
                                                                         "SHORTSALECLOSEDORFINALIZED",
                                                                         "OVERBROAD",
                                                                         "SERVICERELEASED",
                                                                         "CONTINUEWITHDIL",
                                                                         "UNTIMELY",
                                                                         "DILCLOSEDORFINALIZED",
                                                                         "CUSTOMERNOLONGERINTERESTEDINSHORTSALEORDIL"))] <- "OTHERS"

final_data$state[which(final_data$state %in% c("BAYAMON",
                                               "HUMACAO",
                                               "VI",
                                               "AGUADILLA",
                                               "JUNCOS",
                                               "NAGUABO",
                                               "SAN JUAN-RIO PIEDRAS",
                                               "GU",
                                               "ME",
                                               "RIO GRANDE",
                                               "GUAYAMA",
                                               "CAROLINA",
                                               "RIOGRANDE"))] <- "OTHERS"



final_data$stg_otcm[which(final_data$stg_otcm == 0)] <- "OTHERS"


final_data <- final_data[which(final_data$status_servicing != 'RSLD'),]

##### Save the Rdata and the Rfile
save(final_data , file = "final_data.Rdata")
load(file = "final_data.Rdata")