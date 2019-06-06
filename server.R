#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(lubridate)
library(maps)
library(R.utils)
library(countrycode)
library(stringr)
library(tidyr)
library(tools)



foodwaste_PPP <- read.csv("data/info201finaldata.csv")



#Ben_Import

food_spending <- read.csv("data/FAO_Food_CPI.csv")
child_mortality <- read.csv("data/U5MR_mortality_rate_2018.csv")

child_mortality <- child_mortality %>% 
  filter(Uncertainty.bounds. == "Median") %>% 
  gather(Year, Mortality, X2002.5:X2017.5) %>% 
  dplyr::select(ISO.Code, Country.Name, Year, Mortality)
child_mortality$Year <- sub("X", "", child_mortality$Year)
child_mortality$Year <- sub(".5", "", child_mortality$Year)
child_mortality$merge_code <- paste0(child_mortality$ISO.Code,
                                     child_mortality$Year)

food_spending$ISO.Code <- countrycode(food_spending$Area, "country.name", "iso3c")
food_spending <- food_spending %>% 
  dplyr::select(Area, Year, CPI = Value, ISO.Code) %>% 
  filter(CPI <= 200)
food_spending$merge_code <- paste0(food_spending$ISO.Code,
                                   food_spending$Year)

food_mortality <- left_join(food_spending, child_mortality, by = "merge_code")
processed_data <- food_mortality %>% 
  mutate(CPI_mortality_matrix = CPI/Mortality)


########


#Saurav_Import

child_mortality_rate <- read.csv("data/child-mortality-around-the-world.csv")
spread_child_mortality <- child_mortality_rate %>% 
  tidyr::spread(key = Year, value = Child.mortality.rate....)
clean_data_years <- dplyr::select(spread_child_mortality, -Code,-contains("19"))
clean_data_years <- clean_data_years %>%  rename(Country = Entity)
clean_data_years <- gather(clean_data_years,Year,child_mortality,"2000":"2015") %>% 
  mutate(Year = as.numeric(Year))

medical_spending <- read.csv("data/medical_spending.csv",
                             fileEncoding="UTF-8-BOM",
                             stringsAsFactors = FALSE)
medical_spending <- medical_spending %>%  rename(Country = Country.Name)
medical_spending_2000_2015 <- dplyr::select(medical_spending,
                                            -X2016,-Country.Code)
medical_spending_reformat <- gather(medical_spending_2000_2015,Year,
                                    Medical_spending,X2000:X2015) %>% 
  mutate(Year = str_replace(Year, "X", ""),
         Year = as.numeric(Year))



merge_data_frames <- right_join(clean_data_years, 
                                na.omit(medical_spending_reformat), 
                                by = c("Year", "Country"))
merge_data_frames <- na.omit(merge_data_frames)

#######

world_map <- map_data("world")
world_map$ISO.Code.x <- countrycode(world_map$region, "country.name", "iso3c")





# Define server logic for plotting the UFO Sightings maps
shinyServer(function(input, output) {
  
  
  #Data is filtered according to which option is selected; first level is 
  # filtering based on the map selection, second level is based on dates. 
  plot_data <- reactive({
    processed_data %>% filter(Year.x <= input$year_select) %>%
      filter(Year.x >= input$year_select - 1) %>%
      group_by(Country.Name) %>% 
      mutate(change = CPI_mortality_matrix -
               lag(CPI_mortality_matrix)) %>% 
      filter(Year.x == input$year_select) %>% na.omit()
  })
  
  worldmap_data <- reactive({
    right_join(plot_data(), world_map, by = "ISO.Code.x")
  })
  
  
  country_data <- reactive ({
    processed_data %>% filter(Country.Name == input$country_search)
  })
  
  # takes in all of the selected data and renders a plot of it on the page.
  
  output$geo_plot <- renderPlot({
    ggplot() +
      geom_polygon(data = worldmap_data(), aes(x=long, y=lat, group = group,
                                               fill = change)) +
      ggtitle("Geographic plot of CPI/Child Mortality matrix by year") +
      labs(fill = "CPI/Mortality") + coord_fixed()
  })
  
  
  output$CPI_plot <- renderPlot({
    ggplot(country_data()) +
      geom_area(aes(x=Year.x, y = CPI), fill = "lightblue") +
      ylab("CPI") +xlab("Year") + theme(aspect.ratio = 1)
  })
  
  output$Mortality_plot <- renderPlot({
    ggplot(country_data()) +
      geom_area(aes(x=Year.x, y=Mortality),fill = "coral2") +
      ylab("Mortality per 1000") + xlab("Year") + theme(aspect.ratio = 1)
  })

  
#Saurav
  
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
      dplyr::select(Country, Year, results = input$select_column)
  })
  
  
  output$compare_plot <- renderPlot({
    ggplot(output_compare(), aes(Country)) +
      geom_col(aes(y = results), fill = "black") + 
      scale_fill_hue(c=45, l=80) + 
      labs(title = paste("A graph to compare the countries",
                         input$select_column), y =  input$select_column) +
      theme_classic() + 
      theme(plot.title = element_text(color = "black", size = 20, 
                                      face = "bold", hjust = 0.5)) 

    
  })
  
  #Emre
  
  output_scatter_plot <- reactive({
    foodwaste_PPP %>% filter(X.1 == input$pick_income)
  }) 
  
  output$scatterPlot <- renderPlot({
    ggplot(output_scatter_plot(),aes(food.waste, PPP.capita)) + geom_point() +
      scale_y_log10() +geom_smooth(method = lm, se = FALSE) + 
      labs(title = "Food Waste VS PPP Capita", x = "Food Waste",
           y = "PPP capita") + theme(plot.title = element_text(color = "black",
                            size = 20, face = "bold", hjust = 0.5))
    
  })
  
  output_comparison <- reactive({
    foodwaste_PPP %>% filter(Country.Name %in% input$select_country_1) %>% 
      dplyr::select(Country.Name, pick = input$select_columns)
  })
  
  
  output$compare_plot_1 <- renderPlot({
    ggplot(output_comparison(), aes(Country.Name)) + geom_col(aes(y = pick), 
                                                              fill = "skyblue") + 
      scale_fill_hue(c=45, l=80) + 
      labs(title = paste("A graph to compare the countries",input$select_columns), 
           y = input$select_columns) +
      theme_classic() + 
      theme(plot.title = element_text(color = "black", size = 20, 
                                      face = "bold", hjust = 0.5))
    
 })

  
})
