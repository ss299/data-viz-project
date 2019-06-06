library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

foodwaste_PPP <- read.csv("~/Desktop/info201finaldata.csv")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output_scatter_plot <- reactive({
    foodwaste_PPP %>% filter(X.1 == input$pick_income)
  }) 

  output$scatterPlot <- renderPlot({
    ggplot(output_scatter_plot(),aes(food.waste, PPP.capita)) + geom_point() + scale_y_log10() +geom_smooth(method = lm, se = FALSE) + labs(title = "Food Waste VS PPP Capita", x = "Food Waste", y = "PPP capita") + theme(plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5))
    
  })
  
  output_comparison <- reactive({
    foodwaste_PPP %>% filter(Country.Name %in% input$select_country) %>% 
      select(Country.Name, pick = input$select_columns)
  })
  
  
  output$compare_plot <- renderPlot({
    ggplot(output_comparison(), aes(Country.Name)) + geom_col(aes(y = pick), 
                                                      fill = "skyblue") + 
      scale_fill_hue(c=45, l=80) + 
      labs(title = paste("A graph to compare the countries",input$select_columns), 
           y = input$select_columns) +
      theme_classic() + 
      theme(plot.title = element_text(color = "black", size = 20, 
                                      face = "bold", hjust = 0.5))
    
  })
  
  
}