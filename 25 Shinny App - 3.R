library(dplyr)
library(ggplot2)
library(shiny)

load(file = 'group_month_sales.rda')
month_sales$month = factor(month_sales$month, levels = c("2017 1" ,  "2017 2" , "2017 3" , "2017 4",  "2017 5",  "2017 6",  "2017 7" , "2017 8" ,
                                                         "2017 9" , "2017 10", "2017 11" ,"2017 12", "2018 1" ,"NA NA"))
month_sales$group = factor(month_sales$group, levels = c('6','4','1','3','2','5','7'))
levels(month_sales$group) = c('Lazy','Golden','Active','Silver','Mediocre','Square','Inactive')
month_sales$DMM = factor(month_sales$DMM, levels= c("DMM CANDY-SNACKS-BEV-TOB-WHSL","DMM CONSUMABLES","DMM TOBACCO","DMM FREEZER - DELI - COOLER","DMM DRY GROCERY","DMM MEAT - SEAFOOD","DMM FUEL","DMM HBA - OTC AND BABY","DMM PRODUCE AND BAKERY","DMM HOME - HARDLINES - TIRE -","DMM APPAREL AND JEWELRY","DMM CONSUMER ELECTRONICS","DMM ENTERTAINMENT AND OFFICE S", "DMM SEASONAL","DMM ADULT BEVERAGES","DMM PREPARED MEALS","DMM","DMM HEALTH CARE", "DMM MOBILE AND SERVICES"))




ui = fluidPage(
  titlePanel("Sam's Club Dashboard - 3",
             windowTitle = "Sam's Club Dashboard -3"),   
  sidebarLayout(
    sidebarPanel(
      helpText('This is to visualize sales trend in the whole year of each group'),
      selectInput( inputId = 'var', label = 'Group',
                   choices = levels(month_sales$group))
    ),
    mainPanel(
      plotOutput( outputId = 'plot')
    )
  )
)


server = function(input, output){ 
  
  output$plot = renderPlot({
    
    month_sales %>%
      filter(month!= 'NA NA', group == input$var) %>%
      filter(!is.na(DMM)) %>%
      group_by(month,DMM) %>%
      summarise(sales_month = sum(RETAIL_PRICE)) %>%
      ggplot( aes(x = month, y= DMM, -sales_month, fill = sales_month))+
      geom_tile()+
      scale_fill_gradient(low ='White', high = '#006699')+
      theme_light()+
      theme_classic()+
      xlab('Month')+
      ylab('DMM Category')+
      ggtitle( paste('The sales of group',input$var,'in the whole year'))+
      theme(axis.text.x = element_text(size=9, angle = 80,hjust = 1, face = 'bold'),
            axis.text.y = element_text(size=9, face = 'bold'),
            axis.title.x  =element_text(size=17,face = 'bold'),
            axis.title.y  =element_text(size=17,face = 'bold', vjust = 2),
            title =element_text(size=14, face='bold'))
   
    
  })
}

shinyApp(ui, server)






