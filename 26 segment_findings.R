library(dplyr)
library(ggplot2)
data = read.csv("group.csv")



######## Renewal Rate by Group

data %>%
  group_by(group) %>%
  summarise(num = n(),
            renew = sum(RENEW_IND),
            per_renew = renew/num)




####### change group label
data$group = factor(data$group,levels = c(6,4,1,3,2,5,7))
levels(data$group)

levels(data$group) = c("Lazy","Golden","Active","Silver","Mediocre","Square","Inactive")
levels(data$group)






######## Marriage Rate is higher than average in Top 3 Segments

marital_status = data %>%
  filter(!is.na(marital_status_desc)) %>%
  group_by(marital_status_desc,group) %>%
  summarise(num = n()) %>%
  group_by(group) %>%
  mutate(per = round(num/sum(num)*100,2))


marital_status %>%
  filter(marital_status_desc == 1) %>%
  filter(group %in% c("Lazy","Golden","Active")) %>%
  ggplot(aes(x = factor(group, labels = c("Lazy","Golden","Active")), y = per))  +
  geom_col(width = 0.5, fill = "#0069AA") +
  geom_text(aes(label = per), 
            color = "#0069AA",
            vjust = -0.3, size = 10)+
  geom_hline(aes(yintercept = mean(data$marital_status_desc,na.rm = T)*100), size = 2) +
  theme_minimal() +
  theme(legend.position = "none",
        line = element_blank(),
        panel.grid.major.y = element_line(color = "grey"),
        axis.title = element_blank(),
        axis.text = element_text(size=30, face = "bold"))





#### Household is not significant
###### difference of renew_rate between different Household is less than 9%, average +/- 4%

data %>%
  filter(!is.na(hh_size_desc)) %>%
  group_by(hh_size_desc) %>%
  summarise(num = n()) %>%
  mutate(per = num/sum(num))

data %>%
  filter(!is.na(hh_size_desc)) %>%
  group_by(hh_size_desc) %>%
  summarise(renew_rate = mean(RENEW_IND,na.rm = T)) %>%
  arrange(desc(renew_rate))%>%
  ggplot(aes(x = hh_size_desc, y = renew_rate)) +
  geom_line() +
  scale_y_continuous(breaks = NULL) 
+
  theme_minimal() +
  theme(legend.position = "none",
        line = element_blank(),
        panel.grid.major.y = element_line(color = "grey"),
        axis.title = element_blank(),
        axis.text = element_text(size=30, face = "bold"))


hh_summary = data %>%
  filter(!is.na(hh_size_desc)) %>%
  group_by(hh_size_desc) %>%
  summarise(num = n()) %>%
  mutate(per = num/sum(num)) %>%
  mutate(group = "All") %>%
  ungroup()


data %>%
  filter(!is.na(hh_size_desc)) %>%
  group_by(hh_size_desc,group) %>%
  summarise(num = n(),
            renew_rate = mean(RENEW_IND,na.rm = T)) %>%
  arrange(desc(renew_rate)) %>%
  ggplot(aes(x = factor(hh_size_desc), y = renew_rate, group = group, color = group)) +
  geom_line()


hh %>%
  filter(group %in% c("Lazy","Golden","Active")) %>%
  ggplot(aes(x = factor(hh_size_desc), y = per, group = factor(group),color = factor(group))) +
  geom_line(size = 1 , alpha = 0.7) +
  geom_line(data = hh_summary ,aes(x = factor(hh_size_desc), y = per, group = 1), size = 1.5, alpha = 0.7) +
  scale_color_manual(values = c("#004B8D","#0081C6","#5D9732","black")) +
  theme_minimal() +
  theme(line = element_blank(),
    panel.grid.major.y = element_line(color = "grey"),
    axis.title = element_blank())





##### nbr of child is not significant

child_summary = data %>%
  filter(!is.na(nbr_children_desc)) %>%
  group_by(nbr_children_desc) %>%
  summarise(num = n()) %>%
  mutate(per = num/sum(num)) %>%
  mutate(group = "All") %>%
  ungroup()


data %>%
  filter(!is.na(nbr_children_desc) &
         marital_status_desc == 1) %>%
  group_by(nbr_children_desc,group) %>%
  summarise(num = n()) %>%
  group_by(group) %>%
  mutate(per = num/sum(num)) %>%
  arrange(desc(per)) 



child %>%
  filter(group %in% c(1,4,6)) %>%
  ggplot(aes(x = factor(nbr_children_desc), y = per, group = factor(group),color = factor(group))) +
  geom_line(size = 1 , alpha = 0.7) +
  geom_line(data = child_summary ,aes(x = factor(nbr_children_desc), y = per, group = 1), size = 1.5, alpha = 0.7) +
  scale_color_manual(values = c("#004B8D","#0081C6","#5D9732","black")) +
  theme_minimal() +
  theme(line = element_blank(),
    panel.grid.major.y = element_line(color = "grey"),
    axis.title = element_blank())


data %>%
  filter(!is.na(nbr_children_desc),
         !is.na(hh_size_desc),
         marital_status_desc == 1) %>%
  group_by(nbr_children_desc,hh_size_desc,group) %>%
  summarise(num = n()) %>%
  group_by(group) %>%
  mutate(per = num/sum(num)) %>%
  ungroup() %>%
  ggplot(aes(x = hh_size_desc, y = nbr_children_desc, fill = per)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#004B8D") +
  facet_wrap(.~group)



child_hh = data %>%
  filter(!is.na(nbr_children_desc),
         !is.na(hh_size_desc),
         marital_status_desc == 1) %>%
  group_by(nbr_children_desc,hh_size_desc,group) %>%
  summarise(num = n()) %>%
  group_by(group) %>%
  mutate(per = num/sum(num)) %>%
  arrange(desc(per))

data %>%
  group_by(group) %>%
  summarize(count = n())

data %>%
  filter(!is.na(nbr_children_desc),
         !is.na(hh_size_desc),
         marital_status_desc == 1) %>%
  group_by(nbr_children_desc,hh_size_desc,group) %>%
  summarise(num = n()) %>%
  group_by(group) %>%
  mutate(per = num/sum(num)) %>%
  arrange(desc(per)) %>%
  ggplot(aes(x = reorder(group,-per), y = per, fill = factor(hh_size_desc))) +
  geom_col()
  


## membership type code


data %>%
  filter(!is.na(MEMBERSHIP_TYPE_CODE)) %>%
  group_by(MEMBERSHIP_TYPE_CODE,group) %>%
  summarise(num = n()) %>%
  group_by(group) %>%
  mutate(per = num/sum(num)) %>%
  ggplot(aes(x = factor(group, c("Lazy","Golden","Active","Silver","Mediocre","Square","Inactive")), y = per , fill = factor(MEMBERSHIP_TYPE_CODE)))  +
  geom_col() +
  geom_hline(aes(yintercept = mean(data$MEMBERSHIP_TYPE_CODE) - 2))


## MILES to Club

miles_to_club = data %>%
  filter(!is.na(MILES_TO_CLUB)) %>%
  mutate(Miles_category = ifelse(MILES_TO_CLUB > 100, ">100", floor(MILES_TO_CLUB/25)*25)) %>%
  group_by(Miles_category ,group) %>%
  summarise(num = n()) %>%
  group_by(group) %>%
  mutate(per = num/sum(num)) 

data %>%
  filter(!is.na(MILES_TO_CLUB)) %>%
  mutate(Miles_category = ifelse(MILES_TO_CLUB > 100, ">100", floor(MILES_TO_CLUB/25)*25)) %>%
  group_by(Miles_category) %>%
  summarise(num = n(),
            renew_rate = mean(RENEW_IND,na.rm = T)) %>%
  mutate(per = num/sum(num))

data %>%
  filter(!is.na(MILES_TO_CLUB)) %>%
  mutate(Miles_category = ifelse(MILES_TO_CLUB > 100, ">100", floor(MILES_TO_CLUB/25)*25)) %>%
  group_by(Miles_category ,group) %>%
  summarise(num = n()) %>%
  group_by(group) %>%
  mutate(per = num/sum(num)) %>%
  ggplot(aes(x = factor(group, c("Lazy","Golden","Active","Silver","Mediocre","Square","Inactive")), y = per , fill = factor(Miles_category,c(">100","75","50","25","0"))))  +
  geom_col()

data %>%
  filter(!is.na(MILES_TO_CLUB)) %>%
  mutate(Miles_category = ifelse(MILES_TO_CLUB > 100, ">100", floor(MILES_TO_CLUB/25)*25)) %>%
  group_by(Miles_category ,group) %>%
  summarise(num = n()) %>%
  group_by(Miles_category) %>%
  mutate(per = num/sum(num)) %>%
  ggplot(aes(x = factor(Miles_category,c(">100","75","50","25","0")), y = per, fill = factor(group, c("Lazy","Golden","Active","Silver","Mediocre","Square","Inactive"))))  +
  geom_col()






##### plot about Miles
miles_summary = data %>%
  filter(!is.na(MILES_TO_CLUB)) %>%
  mutate(Miles_category = ifelse(MILES_TO_CLUB > 100, ">100", floor(MILES_TO_CLUB/10)*10)) %>%
  group_by(Miles_category) %>%
  summarise(num = n()) %>%
  mutate(per = num/sum(num)*100) %>%
  mutate(group = "All") %>%
  ungroup()

miles = data %>%
  filter(!is.na(MILES_TO_CLUB)) %>%
  mutate(Miles_category = ifelse(MILES_TO_CLUB > 100, ">100", floor(MILES_TO_CLUB/10)*10)) %>%
  group_by(Miles_category,group) %>%
  summarise(num = n()) %>%
  group_by(group) %>%
  mutate(per = num/sum(num)*100) %>%
  ungroup()

miles = rbind.data.frame(miles, miles_summary)
miles$group = as.factor(miles$group)
levels(miles$group)
levels(miles$group) = c("Active","Mediocre","Silver","Golden","Square","Lazy","Inactive")
miles$group = factor(miles$group, levels = c("Lazy","Golden","Active","Silver","Mediocre","Square","Inactive"))

miles %>%
  filter(group %in% c("Lazy","Golden","Active")) %>%
  ggplot(aes(x = factor(Miles_category, c(seq(0,100,by = 10),">100")), y = per ,fill = group)) +
  geom_col(position = "dodge") +
  geom_point(data = miles_summary, aes(x = factor(Miles_category, c(seq(0,100,by = 10),">100")), y = per), size = 4, alpha = 0.7) +
  scale_fill_manual(limits = c("Lazy","Golden","Active","All"),values = c("#004B8D","#0081C6","#5D9732","black")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        line = element_blank(),
        panel.grid.major.y = element_line(color = "grey"),
        axis.text = element_text(size=30, face = 'bold'),
        legend.text=element_text(size=30,face = 'bold'),
        legend.title =element_blank(),
        legend.key.width = unit(3,"line"),
        legend.spacing.x = unit(2,"line")) +
  xlab("") +
  ylab("")







######## Income & other varialbes

data$income_average = ifelse(is.na(data$income_lower), data$income_upper,
                        ifelse(is.na(data$income_upper), data$income_lower, (data$income_lower+data$income_upper)/2))



data %>%
  filter(!is.na(income_average)) %>%
  mutate(Income_category = floor(income_average/25)*25) %>%
  group_by(Income_category) %>%
  summarize(num = n())

segmentation = data %>%
  filter(!is.na(marital_status_desc),!is.na(MILES_TO_CLUB),!is.na(income_average)) %>%
  mutate(Miles_category = ifelse(MILES_TO_CLUB > 100, ">100", floor(MILES_TO_CLUB/25)*25)) %>%
  mutate(Income_category = ifelse(income_average > 150, ">150", floor(income_average/5)*5)) %>%
  group_by(marital_status_desc,Income_category,MEMBERSHIP_TYPE_CODE,Miles_category,group) %>%
  summarise(num = n(),
            renew = sum(RENEW_IND)) %>%
  group_by(group) %>%
  mutate(per = num/sum(num)) %>%
  mutate(renew_rate = renew/num) %>%
  arrange(desc(renew),desc(renew_rate))

segmentation %>%
  filter(marital_status_desc == 1 & MEMBERSHIP_TYPE_CODE == 2 & Miles_category == 0) %>%
  group_by(Income_category) %>%
  summarize(avg_renew = sum(renew)/sum(num)) %>%
  ggplot(aes(x = Income_category, y = avg_renew)) +
  geom_col()

income_summary = data %>%
  filter(!is.na(income_average)) %>%
  mutate(Income_category = ifelse(income_average >= 150, ">150", floor(income_average/25)*25)) %>%
  group_by(Income_category) %>%
  summarise(num = n()) %>%
  mutate(per = num/sum(num)) %>%
  mutate(group = "All") %>%
  ungroup()

income = data %>%
  filter(!is.na(income_average)) %>%
  mutate(Income_category = ifelse(income_average >= 150, ">150", floor(income_average/25)*25)) %>%
  group_by(Income_category,group) %>%
  summarise(num = n()) %>%
  group_by(group) %>%
  mutate(per = num/sum(num)) %>%
  ungroup()

income_all= rbind.data.frame(income,income_summary)





######## diff in income

income %>%
  mutate(group = factor(group, levels = c('6','4','1','3','2','5','7'),labels=c('Lazy','Golden','Active','Silver','Mediocre','Square','Inactive'))) %>%
  filter(group %in% c("Lazy","Golden","Active")) %>%
  ggplot(aes(x = factor(Income_category, levels = c(seq(0,125,25),">150")), y = per, group = factor(group),fill = factor(group))) +
  geom_col(position = "dodge", width = 0.7) +
  geom_point(data = income_summary, aes(x = factor(Income_category, c(seq(0,125,25),">150")), y = per),size = 4, alpha = 0.7)+
  scale_fill_manual(limits = c("Lazy","Golden","Active","All"),values = c("#004B8D","#0081C6","#5D9732","black")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        line = element_blank(),
        panel.grid.major.y = element_line(color = "grey"),
        axis.text = element_text(size=30, face = 'bold'),
        legend.text=element_text(size=30,face = 'bold'),
        legend.title =element_blank(),
        legend.key.width = unit(2,"line"),
        legend.spacing.x = unit(2,"line")) +
  xlab("") +
  ylab("")



#### pos 
pos1 = read.csv("pos_one.csv")
pos2 = read.csv("pos_2.csv")
pos3 = read.csv("pos_3.csv")
pos4 = read.csv("pos_4.csv")
pos5 = read.csv("pos_5.csv")
pos6 = read.csv("pos_6.csv")
pos7 = read.csv("pos_7.csv")
pos8 = read.csv("pos_8.csv")
pos9 = read.csv("pos_9.csv")
pos10 = read.csv("pos_10.csv")
pos11 = read.csv("pos_11.csv")
pos12 = read.csv("pos_12.csv")
pos18_1 = read.csv("pos_18_1.csv")


pos1$month = "2017/01"
pos2$month = "2017/02"
pos3$month = "2017/03"
pos4$month = "2017/04"
pos5$month = "2017/05"
pos6$month = "2017/06"
pos7$month = "2017/07"
pos8$month = "2017/08"
pos9$month = "2017/09"
pos10$month = "2017/10"
pos11$month = "2017/11"
pos12$month = "2017/12"
pos18_1$month = "2018/01"


pos_all = rbind.data.frame(pos1,pos2,pos3,pos4,pos5,pos6,pos7,pos8,pos9,pos10,pos11,pos12,pos18_1)

pos_all$group = factor(pos_all$group, levels = c('6','4','1','3','2','5','7'),labels=c('Lazy','Golden','Active','Silver','Mediocre','Square','Inactive'))
levels(pos_all$group)

pos_all$month =  factor(pos_all$month, levels = c("2017/01","2017/02","2017/03","2017/04","2017/05","2017/06","2017/07","2017/08","2017/09","2017/10","2017/11","2017/12", "2018/01"))
levels(pos_all$month)

pos_all$DMM = factor(pos_all$DMM, levels = c("DMM CANDY-SNACKS-BEV-TOB-WHSL","DMM CONSUMABLES","DMM TOBACCO","DMM FREEZER - DELI - COOLER","DMM DRY GROCERY","DMM MEAT - SEAFOOD","DMM FUEL","DMM HBA - OTC AND BABY","DMM PRODUCE AND BAKERY","DMM HOME - HARDLINES - TIRE -","DMM APPAREL AND JEWELRY","DMM CONSUMER ELECTRONICS","DMM ENTERTAINMENT AND OFFICE S", "DMM SEASONAL","DMM ADULT BEVERAGES","DMM PREPARED MEALS","DMM","DMM HEALTH CARE", "DMM MOBILE AND SERVICES"))
levels(pos_all$DMM)

##### sales distribution over group and months of years
pos_all %>%
  group_by(group, month) %>%
  summarize(sales = sum(sales_DMM)) %>%
  ggplot(aes(x = factor(group), y = month, fill = sales)) +
  geom_tile() +
  scale_fill_gradient("Sales $",low ='White', high = "#004B8D")+
  theme_light()+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=30,face = 'bold'),
        axis.text.y = element_text(size=30, face = 'bold'),
        legend.text=element_blank(),
        legend.title =element_text(size=30,face = 'bold')) +
  xlab('')+
  ylab('')


###### sales distribution over group and category
pos_all %>%
  group_by(DMM, group) %>%
  summarize(sales = sum(sales_DMM)) %>%
  ggplot(aes(x = group, y = DMM, fill = sales)) +
  geom_tile() +
  scale_fill_gradient("Sales $",low ='White', high = "#004B8D")+
  theme_light()+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=30,face = 'bold'),
        axis.text.y = element_text(size=30, face = 'bold'),
        legend.text=element_blank(),
        legend.title =element_text(size=30,face = 'bold')) +
  xlab('')+
  ylab('')


pos1 %>%
  mutate(DMM = factor(DMM, levels = c("DMM CANDY-SNACKS-BEV-TOB-WHSL","DMM CONSUMABLES","DMM TOBACCO","DMM FREEZER - DELI - COOLER","DMM DRY GROCERY","DMM MEAT - SEAFOOD","DMM FUEL","DMM HBA - OTC AND BABY","DMM PRODUCE AND BAKERY","DMM HOME - HARDLINES - TIRE -","DMM APPAREL AND JEWELRY","DMM CONSUMER ELECTRONICS","DMM ENTERTAINMENT AND OFFICE S", "DMM SEASONAL","DMM ADULT BEVERAGES","DMM PREPARED MEALS","DMM","DMM HEALTH CARE", "DMM MOBILE AND SERVICES"))) %>%
  mutate(group = factor(group, levels = c('6','4','1','3','2','5','7'),labels=c('Lazy','Golden','Active','Silver','Mediocre','Square','Inactive'))) %>%
  ggplot(aes(x = group, y = DMM, fill = sales_DMM))+
  geom_tile()+
  scale_fill_gradient(low ='White', high = '#5D9732')+
  theme_light()+
  theme_classic()+
  theme(axis.text.x = element_text(size=45, angle = 70, hjust = 1 ,face = 'bold'),
        axis.text.y = element_text(size=30, face = 'bold'),
        legend.text=element_text(size=30,face = 'bold'),
        legend.title =element_text(size=30,face = 'bold'))+
  xlab('')+
  ylab('')


###### tobacco change

pos_all %>%
  filter(DMM == "DMM TOBACCO") %>%
  group_by(group, month) %>%
  summarize(sales = sum(sales_DMM)) %>%
  ggplot(aes(x = month, y = sales, group = group, color = group)) +
  geom_line(size = 2) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(size=30,angle = 45, vjust = 0.5,face = 'bold'),
        axis.text.y = element_text(size=30, face = "bold"),
        legend.text=element_blank(),
        legend.title =element_text(size=30,face = "bold"),
        axis.line = element_line(color = "grey")) +
  scale_color_manual(values = c("grey","#004B8D","grey","grey","grey","grey","#5D9732")) +
  scale_y_continuous(breaks = seq(0,3000000,1000000),labels = c("","$1M","$2M","$3M")) +
  xlab("")+
  ylab("")


