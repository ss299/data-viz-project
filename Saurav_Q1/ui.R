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


medical_spending <- read.csv("data/medical_spending.csv",
                             stringsAsFactors = FALSE)
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
View(merge_data_frames)

shinyUI(fluidPage(
  
  titlePanel("To what extent does purchase power impact medical spending and 
             child mortality?"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("select_country", label = h3("Pick a country"), 
                  choices = unique(merge_data_frames$Country)),
      sliderInput("select_year", label = h3("Select a year"), 
                  min = 2000, max = 2015, value = 2004),
      radioButtons("select_column", 
                   label = h3("Compare Medical Spending or Child Mortality"),
                   list("Medical Spending" = "Medical_spending",
                        "Child Mortality" = "child_mortality")),
      selectInput("select_first_country", label = h3("Pick a country"), 
                  choices = unique(merge_data_frames$Country), multiple = TRUE)

    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        
        tabPanel("Child_mortality VS Medical Spending", plotOutput("year_plot")),
        tabPanel("Compare 2 countries", plotOutput("compare_plot"))
      )
    )
  )
))
