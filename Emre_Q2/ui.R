library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)


foodwaste_PPP <- read.csv("~/Desktop/info201finaldata.csv")


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("To what extent does purchase power impact the consumption and wastage of food?"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      radioButtons("pick_income", label = h3("Pick a income level"), choices = list("High Income" = "HIC", "Upper Medium Income" = "UMC", "Lower Medium income" = "LMC", "Lower Income" = "LIC")),
      selectInput("select_columns", 
                   label = h3("Compare PPP per Capita or Food Waste"),
                   list("Food Waste" = "food.waste",
                        "PPP per capita" = "PPP.capita")),
      selectInput("select_country", label = h3("Pick a country"), 
                  choices = unique(foodwaste_PPP$Country.Name), multiple = TRUE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
    tabPanel("Food Waste VS PPP capita",plotOutput("scatterPlot")),
    tabPanel("Food Waste VS PPP capita Comparison", plotOutput("compare_plot"))
      )
    )
  )
))
