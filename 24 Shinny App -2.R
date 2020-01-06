library(dplyr)
library(ggplot2)
library(shiny)

load(file = 'group_month_sales.rda')
month_sales$month = factor(month_sales$month, levels = c("2017 1" ,  "2017 2" , "2017 3" , "2017 4",  "2017 5",  "2017 6",  "2017 7" , "2017 8" ,
                                                         "2017 9" , "2017 10", "2017 11" ,"2017 12", "2018 1" ,"NA NA"))

ui = fluidPage(
  titlePanel("Sam's Club Dashboard - 2",
             windowTitle = "Sam's Club Dashboard"),   
  sidebarLayout(
    sidebarPanel(
      helpText('This is to visualize sales trend in the whole year'),
      selectInput( inputId = 'var', label = 'Variable',
                   choices = levels(month_sales$DMM))
    ),
    mainPanel(
      plotOutput( outputId = 'plot')
    )
  )
)


server = function(input, output){ 
  
  output$plot = renderPlot({
    
    month_sales %>%
      filter(DMM == input$var) %>%
      group_by(month) %>%
      summarise(sales = sum(RETAIL_PRICE)) %>%
      ggplot( aes( x= month, y = sales))+   
      geom_col (fill = 'royalblue4')+
      xlab('Months')+
      ylab(paste(input$var,'Sales'))+
      ggtitle( paste('The sales of',input$var,'in the whole year'))+
      theme_classic()+
      theme(axis.text.x = element_text(size=15, angle = 40,hjust = 1, face = 'bold'),
            axis.text.y = element_text(size=15, face = 'bold'),
            axis.title.x  =element_text(size=17,face = 'bold'),
            axis.title.y  =element_text(size=17,face = 'bold', vjust = 2),
            title =element_text(size=20, face='bold'))
    
  })
}

shinyApp(ui, server)



