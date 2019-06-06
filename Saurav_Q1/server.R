library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

child_mortality_rate <- read.csv("data/child-mortality-around-the-world.csv")
spread_child_mortality <- child_mortality_rate %>%  tidyr::spread(key = Year, 
                                              value = Child.mortality.rate....)
clean_data_years <- select(spread_child_mortality, -Code,-contains("19"))
clean_data_years <- clean_data_years %>%  rename(Country = Entity)
clean_data_years <- gather(clean_data_years,Year,child_mortality,"2000":"2015") %>% 
  mutate(Year = as.numeric(Year))


medical_spending <- read.csv("data/medical_spending.csv",stringsAsFactors = FALSE)
medical_spending <- medical_spending %>%  rename(Country = Country.Name)
medical_spending_2000_2015 <- select(medical_spending,-X2016,-Country.Code)

medical_spending_reformat <- gather(medical_spending_2000_2015,Year,
                                    Medical_spending,X2000:X2015) %>% 
  mutate(Year = str_replace(Year, "X", ""),
         Year = as.numeric(Year))



merge_data_frames <- right_join(clean_data_years, 
                                na.omit(medical_spending_reformat), 
                                by = c("Year", "Country"))
merge_data_frames <- na.omit(merge_data_frames)



shinyServer(function(input, output) {
  
  output_country <- reactive({
    merge_data_frames %>% filter(Country == input$select_country)
    
  })
   
  output$year_plot <- renderPlot({
    ggplot() + geom_area(aes(y = child_mortality, x = Medical_spending),
                         data = output_country()) +
      labs(title = "A graph to show the relationship between Medical Spending 
           and Child Mortality", x = "Medical Spending",
           y = "Child Mortality (%)") + theme_classic() + 
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
  })
  
  output_compare <- reactive({
    merge_data_frames %>% filter(Year == input$select_year, 
                                 Country %in% input$select_first_country) %>% 
      select(Country, Year, results = input$select_column)
  })

  
  output$compare_plot <- renderPlot({
    ggplot(output_compare(), aes(Country)) + geom_col(aes(y = results), 
                                                      fill = "black") + 
      scale_fill_hue(c=45, l=80) + 
      labs(title = paste("A graph to compare the countries",input$select_column),
           y = input$select_column) +
      theme_classic() + 
      theme(plot.title = element_text(color = "black", size = 20, 
                                      face = "bold", hjust = 0.5))
    
  })
  
})
