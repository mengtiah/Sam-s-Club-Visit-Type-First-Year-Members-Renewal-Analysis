library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)
####### Loading Data ########
setwd("~/Downloads/USC_Attrition Paths")
a_pos = fread('pos.txt', sep = '|')
View(a_pos[1:5000,])
a_dmm_gmm = fread('dmm_gmm.txt', sep = '|')
a_members = fread('members.txt', sep = '|')
a_tender_type = fread('tender_type.txt', sep = '|')

setwd("~/Downloads/USC_FY_Renewals")
r_members = fread('members.txt', sep = '|')
r_pos = fread('pos.txt', sep = "|")

########### Cleaning data for a_members table ############
## The dates
a_members$join_date = ymd(a_members$JOIN_DATE)
a_members$last_renew_date = ymd(a_members$LAST_RENEW_DATE)
a_members$renew_date = ymd(a_members$RENEW_DATE) # has NA
a_members$next_renew_date = ymd(a_members$NEXT_RENEW_DATE)
a_members$plus_upgrade_date = ymd(a_members$PLUS_UPGRADE_DATE) # has NA
a_members = a_members %>%
  select(-JOIN_DATE, -(LAST_RENEW_DATE:NEXT_RENEW_DATE))
a_members = a_members%>%
  select(-PLUS_UPGRADE_DATE)
## Filter the members that we need
a_members = a_members %>%
  filter(join_date > '2017-01-01')
## To see how many renewed and how many did not renew
data = a_members%>%
  group_by(renew_date)%>%
  summarise(count = n())

save(a_members, file = 'a_members.rda')


########### Cleaning data for r_members table ############
library(stringr)
r_members$hhh_age_desc = str_sub(r_members$hhh_age_desc, start = 5, end = 6)
library(naniar)
r_members %>%
  replace_with_na(replace = list(income_desc = "?"))

r_members$income_desc = ifelse(r_members$income_desc == '?',NA,r_members$income_desc)
r_members$MMILES_TO_CLUB = as.numeric(r_members$MILES_TO_CLUB)


######################### 11/12/2018 Explore the Data #########################
# We should group by both category_num and membership_id
setwd("~/Downloads")
a_pos_group = load('a_pos_groupby_member.rda')


setwd("~/Downloads/USC_FY_Renewals")
r_pos = fread('pos.txt', sep = "|")
group = r_pos%>%
  group_by(MEMBERSHIP_ID, CATEGORY_NBR) %>%
  summarise(sales = sum(RETAIL_PRICE),
            cost = sum(UNIT_COST*UNIT_QTY))


r_dmm_gmm = fread('dmm_gmm.txt', sep = "|")
r_dmm_gmm[r_dmm_gmm$CATEGORY_NBR == 76,]

all_member_category = left_join(group, r_dmm_gmm, by = 'CATEGORY_NBR')
r_dmm_gmm[GMM_NBR == 45 & DMM_NBR == 9,]

# focus on the renewal dataset- about the first year vips. We have to seperate ourselves

# seperate the r_members
setwd("~/Downloads")
r_member = load('r_members.rda')
r_tender = load('r_tender_2017_2018.rda')

all = left_join(all_member_category, r_members, by = 'MEMBERSHIP_ID')
save(all, file = 'all.rda')
write.csv(all, file = 'all.csv')

unrenew = all %>%
  filter(RENEW_IND == 'UNRENEWED')
renew = all %>%
  filter(RENEW_IND != 'UNRENEWED')

save(unrenew, file = 'unrenew.rda')
save(renew, file = 'renew.rda')
##### The tender has to be done diffrently ######

hh = r_pos%>%
  group_by(MEMBERSHIP_ID,VISIT_NBR) %>%
  summarise(unitprice = sum(RETAIL_PRICE),
            unitcost = sum(UNIT_COST),
            qty = sum(UNIT_QTY))
tender = left_join(hh, r_tender_2017_2018, by = 'VISIT_NBR')
save(tender, file = 'tender.rda')

View(hh[1:10,])


View(r_pos[1:20,])


alls = load('all.rda')

all_sel= all%>%
  select(-GMM, -DMM, -CATEGORY, -COHORT_MONTH, 
         -MEMBERSHIP_TYPE_DESC, -TENURE_GRP, -LAST_RENEW_DATE,
         -RENEW_DATE, -NEXT_RENEW_DATE, -PLUS_STATUS_BEFORE_REN, 
         -PLUS_STATUS_AFTER_REN, -PLUS_UPGRADE_DATE)

all_sel$marital_status_desc = ifelse(all_sel$marital_status_desc == 'Married',1,0)
levels(all_sel$ethnic_desc)  
all_sel$ethnic_desc = ifelse(all_sel$ethnic_desc == 'African Americn', 0,
                             ifelse(all_sel$ethnic_desc == 'Asian Other', 1,
                                    ifelse(all_sel$ethnic_desc == 'Chinese', 2,
                                           ifelse(all_sel$ethnic_desc == 'Eastrn European', 3,
                                                  ifelse(all_sel$ethnic_desc == 'Greek', 4,
                                                         ifelse(all_sel$ethnic_desc == 'Hispanic', 5,
                                                                ifelse(all_sel$ethnic_desc == 'Jewish', 6,
                                                                       ifelse(all_sel$ethnic_desc == 'Korean', 7,
                                                                              ifelse(all_sel$ethnic_desc == 'Middle Eastern', 8,
                                                                                     ifelse(all_sel$ethnic_desc == 'Native Americn', 9,
                                                                                            ifelse(all_sel$ethnic_desc == 'Polynesian', 10,
                                                                                                   ifelse(all_sel$ethnic_desc == 'Scandinavian', 11,
                                                                                                          ifelse(all_sel$ethnic_desc == 'Unknown', 12,
                                                                                                                 ifelse(all_sel$ethnic_desc == 'Vietnamese', 13, 14))))))))))))))


all_sel$RENEW_IND = ifelse(all_sel$RENEW_IND == 'UNRENEWED', 0,
                           ifelse(all_sel$RENEW_IND == 'RENEWED BASE', 1,2))
all_sel$PLUS_MEMBERSHIP_IND = ifelse(all_sel$PLUS_MEMBERSHIP_IND == 'N',0,1)

all_sel$autorenew_ind = ifelse(all_sel$autorenew_ind == 'N',0,1)
all_sel$payroll_deduct_ind = ifelse(all_sel$payroll_deduct_ind == 'N',0,1)

write.csv(all_sel, file = 'all_all.csv')
