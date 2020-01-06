library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)

##### find GMM avg sales difference among groups 

DMM = read.csv('group_join_DMM.csv')

DMM$group = factor(DMM$group, levels = c('6','4','1','3','2','5','7'))


dmm = DMM %>%
  group_by(group, DMM) %>%
  summarise(sales_DMM = sum(sales_category),
            num = n_distinct(MEMBERSHIP_ID))  %>%
  mutate(avg_sales = sales_DMM/num)

group_avg = dmm %>%
  select( group,DMM, avg_sales ) %>%
  filter(!is.na(DMM))
write.csv(group_avg,file = 'group_avg.csv')

group_avg = read.csv('group_avg.csv')

group_avg = group_avg %>%
  select(group, DMM, avg_sales)

dmm_unstack = reshape(group_avg, direction="wide", v.names="avg_sales", timevar="DMM", idvar="group")

write.csv(dmm_unstack, file = 'group_dmm_avg_sales.csv' )


dmm_avg = DMM %>%
  group_by(DMM) %>%
  summarise(DMM_sales = sum(sales_category),
            num = n_distinct(MEMBERSHIP_ID)) %>%
  mutate(pop_avg_DMM = DMM_sales/num)     ###find the average DMM sales per person in poplation
  
#### find the tender_type difference among groups
group = read.csv('group_initial.csv')
group_demographic_avg = group %>%
  group_by(group) %>%
  summarise(miles_avg = mean(MILES_TO_CLUB, na.rm = T),
            age_avg = mean(hhh_age_desc, na.rm = T),
            marital_rate_avg = mean(marital_status_desc, na.rm = T),
            income_lower_avg = mean(income_lower, na.rm = T),
            income_upper_avg = mean(income_upper, na.rm = T),
            hh_size_avg = mean(hh_size_desc,na.rm = T),
            nbr_children_avg = mean(nbr_children_desc, na.rm = T),
            renew_rate_avg = mean(RENEW_IND, na.rm = T),
            plus_rate_avg = mean(PLUS_STATUS_BEFORE_REN, na.rm = T),
            auto_renew_avg = mean(autorenew_ind, na.rm = T),
            payroll_avg = mean(payroll_deduct_ind, na.rm = T))

write.csv(group_demographic_avg, file = 'group_demographic_avg.csv')

summary = summary(group)

group %>%
  group_by(group) %>%
  summarise(n = n())

#### find the tender_type average sales difference among groups
tender = read.csv('stack_tender.csv')

group_tender = left_join(select(group, MEMBERSHIP_ID, group), tender, by= 'MEMBERSHIP_ID')

group_tender_avg = group_tender %>%
  group_by(group, TENDER_TYPE_DESC) %>%
  summarise(num = n_distinct(MEMBERSHIP_ID),
            sum_tender_sales = sum(sum_sales)) %>%
  mutate(group_avg_tender = sum_tender_sales/num) %>%
  select(group, TENDER_TYPE_DESC, group_avg_tender)


write.csv(group_tender_avg, file = 'group_tender_avg.csv')

group_tender_avg = read.csv('group_tender_avg.csv')
group_tender_avg = group_tender_avg %>%
  select(-X)

group_tender_unstack = reshape(group_tender_avg, direction="wide", v.names="group_avg_tender", timevar="TENDER_TYPE_DESC", idvar="group")

write.csv(group_tender_unstack, file = 'group_tender_unstack.csv')


tender_avg = tender %>%
  group_by(TENDER_TYPE_DESC) %>%
  summarise(sum_sale = sum(sum_sales),
            num = n_distinct(MEMBERSHIP_ID)) %>%
  mutate(pop_avg = sum_sale/num)



