df = read.csv('all_member_segment.csv')

library(dplyr)
library(ggplot2)
library(shiny)
library(lubridate)

############# mutate a new column group
df$days_dif[is.na(df$days_dif)] <- 396
df$freq[is.na(df$days_freq)] <- 396

group = df %>%
  mutate(group = ifelse(sales <= 3726.22 & days_dif <= 11.5,1,
                        ifelse(sales <= 3726.22 & days_dif > 11.5 & days_dif <=26.5,2,
                               ifelse(sales > 3726.22 & sales <= 5831.45 & days_dif <=26.5,3,
                                      ifelse(sales > 5831.45 & days_dif <= 26.5,4,
                                             ifelse(days_dif > 26.5 & days_dif <= 99.5 & autorenew_ind <= 0.5,5,
                                                    ifelse(autorenew_ind > 0.5,6,7
                                                           )))))))

group$RENEW_IND = ifelse(group$RENEW_IND == 0, 0,1)
summary = group %>%
  group_by(group) %>%
  summarise(avg_renew = sum(RENEW_IND)/n(),
            member_count = n())

 group %>%
  filter(group %in% c('Golden', 'Lazy','Active','Inactive')) %>%
  ggplot(aes(x = income_upper, fill = group))+
  geom_histogram(position = 'identity', alpha = 0.3)

  
income =  group %>%
  group_by(group) %>%
  summarise( m_income_lower = mean(income_lower, na.rm = T),
             m_income_upper = mean(income_upper, na.rm = T))

Miles =  group %>%
  group_by(group) %>%
  summarise( miles = mean(MILES_TO_CLUB, na.rm = T))


group %>%
  group_by(group) %>%
  summarise(payroll = sum(payroll_deduct_ind, na.rm = T)/n())

children = group %>%
  group_by(group) %>%
  summarise(children = mean(nbr_children_desc, na.rm = T))

age = group %>%
  group_by(group) %>%
  summarise(age = mean(hhh_age_desc, na.rm = T))

marry = group %>%
  group_by(group) %>%
  summarise(marital_rate = mean(marital_status_desc, na.rm = T))

write.csv(group, file = 'group_initial.csv')

ggplot(summary, aes(x= reorder(group,-avg_renew), y = avg_renew))+
  geom_col(fill = 'blue')




####### Shiny Shows different demographic or behavior among groups
group = read.csv('group_initial.csv')
group$date_max = ymd(group$date_max)
group$date_min = ymd(group$date_min)
group$group = factor(group$group, levels = c('6','4','1','3','2','5','7'))
levels(group$group) = c('Lazy','Golden','Active','Silver','Mediocre','Square','Inactive')

ui = fluidPage(
  titlePanel("Sam's Club Dashboard",
             windowTitle = "Sam's Club Dashboard"),   
  sidebarLayout(
    sidebarPanel(
      helpText('This is to visualize inter-segment data'),
      selectInput( inputId = 'var', label = 'Variable',
                   choices = c('Age'='hhh_age_desc','First visit date' = 'date_min','Last visit date' = 'date_max',
                               'Days since last visit' ='days_dif', 'Lower income' ='income_lower','Household size' ='hh_size_desc',
                               'Visit frequency' ='freq', 'Count category' = 'num_category'))
    ),
    mainPanel(
      plotOutput( outputId = 'plot')
    )
  )
)


server = function(input, output){ 
  
  output$plot = renderPlot({
   
    group %>%
      ggplot( aes_string( x= 'group', y = input$var))+   ### the input x and y are string, so use aes_string
      geom_boxplot (color = 'royalblue4')+
      ggtitle( paste('The percentiles of',input$var,'in each segment'))+
      xlab('Segments')+
      ylab(input$var)+
      theme_classic()+
      theme(axis.text.x = element_text(size=15, angle = 40,hjust = 1, face = 'bold'),
            axis.text.y = element_text(size=15, face = 'bold'),
            axis.title.x  =element_text(size=17,face = 'bold'),
            axis.title.y  =element_text(size=17,face = 'bold', vjust = 2),
            title =element_text(size=20, face='bold'))
    
  })
}

shinyApp(ui, server)



