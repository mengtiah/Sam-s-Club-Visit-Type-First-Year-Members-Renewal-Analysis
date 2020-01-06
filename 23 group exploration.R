library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)

tender = read.csv('stack_tender.csv')
group = read.csv('group.csv')
tender[is.na(tender)] <- 0

df = left_join(group, tender, by = 'MEMBERSHIP_ID')

df$group = factor(df$group, levels = c('Lazy','Golden','Active','Silver','Mediocre','Square','Inactive'))



df %>%
  group_by(group, TENDER_TYPE_DESC) %>%
  filter(!is.na(TENDER_TYPE_DESC)) %>%
  summarise(group_sum = sum(sum_sales)) %>%
  ggplot(aes(x = as.factor(group), y= reorder(TENDER_TYPE_DESC,-group_sum), fill = group_sum))+
  geom_tile()+
  scale_fill_gradient(low ='White', high = '#006699')+
  theme_light()+
  theme_classic()+
  theme(axis.text.x = element_text(size=46 ,face = 'bold'),
        axis.text.y = element_text(size=40, face = 'bold'),
        legend.text=element_text(size=30,face = 'bold'),
        legend.title =element_text(size=30,face = 'bold'))+
  xlab('')+
  ylab('')



##### find category difference among groups, not as good as DMM category


r_pos = fread('r_pos.txt', sep = '|')
r_dmm_gmm = fread('r_dmm_gmm.txt',sep = '|')

pos_small = r_pos %>%
  group_by(MEMBERSHIP_ID, CATEGORY_NBR,CATEGORY_DESC) %>%
  summarise( sales_category = sum(RETAIL_PRICE))

group_small = group %>%
  select(MEMBERSHIP_ID, group)


category = select(left_join(group_small, pos_small, by = 'MEMBERSHIP_ID'), MEMBERSHIP_ID, group,CATEGORY_DESC, sales_category)
category$group = factor(category$group, levels = c('6','4','1','3','2','5','7'))
category$CATEGORY_DESC = factor(category$CATEGORY_DESC)
write.csv(category, file = 'group_join_category.csv')

category %>%
  group_by(group, CATEGORY_DESC) %>%
  summarise(sales_category = sum(sales_category)) %>%
  filter(!is.na(CATEGORY_DESC)) %>%
  ggplot(aes( x = as.factor(group), y= reorder(CATEGORY_DESC,-sales_category), fill = sales_category))+
  geom_tile()+
  scale_fill_gradient(low ='White', high = 'darkred')+
  theme_light()+
  theme_classic()+
  xlab('')+
  ylab('')


##### find GMM difference among groups clearer than category in whole year
DMM = left_join(left_join(group_small, pos_small, by = 'MEMBERSHIP_ID'), r_dmm_gmm, by = 'CATEGORY_NBR')
DMM$group = factor(DMM$group, levels = c('6','4','1','3','2','5','7'))
write.csv(DMM, file ='group_join_DMM.csv' )

DMM = read.csv('group_join_DMM.csv')
DMM$group = factor(DMM$group, levels = c('6','4','1','3','2','5','7'))

levels(DMM$group) = c('Lazy','Golden','Active','Silver','Mediocre','Square','Inactive')

dmm = DMM %>%
  group_by(group, DMM) %>%
  summarise(sales_DMM = sum(sales_category)) 



DMM %>%
  group_by(group, DMM) %>%
  summarise(sales_DMM = sum(sales_category)) %>%
  filter(!is.na(DMM)) %>%
  ggplot(aes( x = as.factor(group), y= reorder(DMM,-sales_DMM), fill = sales_DMM))+
  geom_tile()+
  theme_light()+
  theme_classic()+
  scale_fill_gradient(low ='White', high = '#006699')+
  theme(axis.text.x = element_text(size=45, angle = 70, hjust = 1 ,face = 'bold'),
        axis.text.y = element_text(size=30, face = 'bold'),
        legend.text=element_text(size=30,face = 'bold'),
        legend.title =element_text(size=30,face = 'bold'))+
  xlab('')+
  ylab('')


##### find the first three month DMM category sales among goups

pos_date = r_pos %>%
  select(MEMBERSHIP_ID, CATEGORY_NBR, VISIT_DATE,RETAIL_PRICE )
  
pos_date$VISIT_DATE = ymd(pos_date$VISIT_DATE)

pos_three_month = left_join(left_join(group_small, pos_date, by= 'MEMBERSHIP_ID'), r_dmm_gmm, by = 'CATEGORY_NBR')
pos_three_month$group = factor(pos_three_month$group, levels = c('6','4','1','3','2','5','7'))

pos_three = pos_three_month %>%
  mutate(month = paste(year(VISIT_DATE), month(VISIT_DATE))) %>%
  select(MEMBERSHIP_ID, group, month, DMM, RETAIL_PRICE)
write.csv(pos_three, file = 'group_month_sales.csv')

pos_three = pos_three %>%
  filter(month == '2017 1' | month == '2017 2' | month == '2017 3')%>%
  group_by(group, DMM) %>%
  summarise(sales_DMM = sum(RETAIL_PRICE))%>%
  filter(!is.na(DMM)) 

write.csv(pos_three, file = 'group_month_sales_3months.csv')

## can directly run code from following
pos_three = read.csv('group_month_sales_3months.csv')

pos_three$DMM = as.factor(pos_three$DMM)
pos_three$DMM = factor(pos_three$DMM, levels= c("DMM CANDY-SNACKS-BEV-TOB-WHSL","DMM CONSUMABLES","DMM TOBACCO","DMM FREEZER - DELI - COOLER","DMM DRY GROCERY","DMM MEAT - SEAFOOD","DMM FUEL","DMM HBA - OTC AND BABY","DMM PRODUCE AND BAKERY","DMM HOME - HARDLINES - TIRE -","DMM APPAREL AND JEWELRY","DMM CONSUMER ELECTRONICS","DMM ENTERTAINMENT AND OFFICE S", "DMM SEASONAL","DMM ADULT BEVERAGES","DMM PREPARED MEALS","DMM","DMM HEALTH CARE", "DMM MOBILE AND SERVICES"))
pos_three$group = factor(pos_three$group, levels = c('6','4','1','3','2','5','7'))
levels(pos_three$group) = c('Lazy','Golden','Active','Silver','Mediocre','Square','Inactive')

ggplot(pos_three, aes(x = group, y= DMM, fill = sales_DMM))+
  geom_tile()+
  scale_fill_gradient(low ='White', high = '#5D9732')+
  theme_light()+
  theme_classic()+
  theme(axis.text.x = element_text(size=45, angle = 70, hjust = 1 ,face = 'bold'),
        axis.text.y = element_text(size=30, face = 'bold'),
        legend.text=element_text(size=30,face = 'bold'),
        legend.title =element_text(size=30,face = 'bold'))+
  xlab('')+
  labs(title = 'Category Sales', size = 30)+
  ylab('')
  




##### look at the month sales among groups
month_sales = read.csv('group_month_sales.csv')
save(month_sales, file = 'group_month_sales.rda')



load(file = 'group_month_sales.rda')

month_sales$group = factor(month_sales$group, levels = c('6','4','1','3','2','5','7'))
levels(month_sales$group) = c('Lazy','Golden','Active','Silver','Mediocre','Square','Inactive')

month_sales$month = factor(month_sales$month, levels = c("2017 1" ,  "2017 2" , "2017 3" , "2017 4",  "2017 5",  "2017 6",  "2017 7" , "2017 8" ,
                                                         "2017 9" , "2017 10", "2017 11" ,"2017 12", "2018 1" ,"NA NA"))
month_sales$RETAIL_PRICE[is.na(month_sales$RETAIL_PRICE)] <- 0
month_sales %>%
  filter(month!= 'NA NA') %>%
  group_by(group, month) %>%
  summarise(sales_month = sum(RETAIL_PRICE)) %>%
  ggplot( aes(x = group, y= month, fill = sales_month))+
  geom_tile()+
  scale_fill_gradient(low ='White', high = '#006699')+
  theme_light()+
  theme_classic()+
  theme(axis.text.x = element_text(size=50,face = 'bold'),
        axis.text.y = element_text(size=45, face = 'bold'),
        legend.text=element_text(size=30,face = 'bold'),
        legend.title =element_text(size=30,face = 'bold'))+
  xlab('')+
  ylab('')




