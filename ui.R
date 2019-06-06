#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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



shinyUI(fluidPage(
  theme = shinytheme("cerulean"),
  
  navbarPage("Global Health Research Group 19",
      tabPanel("Intro",
               mainPanel(
                 h2("Introduction"),
                 br(),
                 h4("Welcome to Group 19's research into the impacts of 
purchasing power and food access on child health and wellbeing. We wish to
                   understand:"),
                 em(h4("To what extent does purchasing power affect food
                    consumption and access, and what implications do they have
                    on child mortality?")),
                 h4("To understand this further, we divded the question into
                   three segments to explore the intracaies of this global
                   phenomenon. These questions are:"),
                 em(h4("To what extent does purchasing power impact medical
                    spending and child mortality?")),
                 em(h4("To what extent does purchasing power impact the consumption
                    and wastage of food?")),
                 em(h4("How does the ability to purchase food impact
                    child mortality?")),
                 h4("We hope that through our illustrations, you will be able to
                   discover for yourself the connection within the data."),
                 hr(),
                 h2("Background on Data"),
                 br(),
                 h4("The datasets for these illustrations have come from a 
                   variety of global reporting sources. Through extensive
                   searching, we eventually settled on data originating from
                   these sources:"),
                 h4("The World Bank"),
                 a("https://data.worldbank.org",
                   href = "https://data.worldbank.org"),
                 h4("Our data on PPP and food wastage came from the World Bank.
                   They provide a large amount of workable data regarding the
                   world economic status."),
                 br(),
                 h4("FAO"),
                 a("http://www.fao.org/faostat",
                   href = "http://www.fao.org/faostat"),
                 h4("FAO provided information about food status and the food
                   CPI on countries. They also contain a large amount of 
                   agriculture and food related data."),
                 br(),
                 h4("UNICEF"),
                 a("https://data.unicef.org", 
                   href = "https://data.unicef.org/"),
                 h4("UNICEF provided information on child mortality data. They
                   also have large repositiories of child and family
                   development data."),
                 hr(),
                 h3("The Team and Project Resources"),
                 br(),
                 h4("Benjamin Chan"),
                 h4("Saurav Sawansukha"),
                 h4("Emre Inceoglu"),
                 br(),
                 h4("This app was constructed in Shiny in R:"),
                 a("https://shiny.rstudio.com", 
                   href = "https://shiny.rstudio.com"),
                 br(),
                 h4("Special thanks to our TAs Andrey and Alvin for their
                    support, and our Professor Ott Toomet.")
                
               )
               ),
      
      
      
      tabPanel("Food CPI and Child Mortality",
               sidebarLayout(
                 sidebarPanel(
                   column(12,
                          sliderInput("year_select",
                                      label = h3("Geographic Reporting by Year"),
                                      min = 2007,
                                      max = 2017, value = 2007)
                   ),
                   h5("This silder controls the year displayed for the
                      geographical plot."),
                   textInput("country_search", label = h3("Country Search"),
                             value = "Country"),
                   h5("This searchbar allows you to search for a country to
                      visualize its CPI and child mortality trends."),
                   hr(),
                   fluidRow(
                     column(4, verbatimTextOutput("value")),
                     column(4, verbatimTextOutput("range"))
                   )
                 ),

                 # Show a geographic plot and the data summary on two tabs.
                 mainPanel(
                   h3("How does the ability to purchase food impact child
                      mortality?"),
                   tabsetPanel(

                     tabPanel("Geographic Plot", plotOutput("geo_plot"),
                              p("This plot illustrates the change of a
                                CPI/Mortality matrix across the world
                                for a given year. The change displayed
                                indicates that an increase in the CPI
                                or a decrease in mortality occured,
                                indicating that the affordablility of food
                                is negatively correlated with decreasing
                                mortality rate.")),

                     tabPanel("National CPI/Mortality", "Nataional CPI",
                              plotOutput("CPI_plot"),
                              "National Mortality", plotOutput("Mortality_plot"),
                              p("This plot indicates the tendacies of Food CPI
                                and Child Mortality for each individual country.
                                It can be seen throughout various nations that
                                the CPI generally increases over time, and the
                                child mortality rate decreases over time.")),

                     h4("Effects and Implications"),
                     p("This illustration uses a food CPI index and correlates
                       this with the child mortality in the country. In essence,
                       the changes in food CPI indicate the changing abilities
                       for people to purchase food, and child mortality is a
                       strong indicator of child health and wellbeing."),
                     p("As can be seen from the data, its interesting to note
                       that as the CPI for food increases (Indicating an
                       increase in the price of goods), in general, the
                       child mortality rate decreases. This applies generally
                       across the board regardless of geographic location.
                       Thus, it could be said that in spite of the costs
                       increase, people are still well able to afford food.
                       Thus, its can be extrapolated that the increasing
                       purchasing power of the people has a direct link to
                       the decreasing child mortality rate. However, further
                       research would be helpful in determining if the CPI
                       rises in tandem with the income of the country.")

                   )
                 )
               )
         ),
      tabPanel("Medical Spending and Child Mortality",


               # Sidebar with a slider input for number of bins
               sidebarLayout(
                 sidebarPanel(
                   selectInput("select_country", label = h3("Pick a country"),
                               choices = unique(merge_data_frames$Country)),
                   h5("Use this selection box to pick a country to visualize
                      that country's plot of child mortality and
                      medical spending."),
                   sliderInput("select_year", label = h3("Select a year"),
                               min = 2000, max = 2015, value = 2004),
                   h5("This slider controls which year the comparison plot
                      displays."),
                   radioButtons("select_column",
                                label = h3("Compare Medical Spending or Child Mortality"),
                                list("Medical Spending" = "Medical_spending",
                                     "Child Mortality" = "child_mortality")),
                  h5("This allows you to select if you wish to view the data
                       for child mortality or medical spending in the country
                       comparison."),
                    selectInput("select_first_country",
                                label = h3("Pick a country"),
                                choices = unique(merge_data_frames$Country),
                                multiple = TRUE),
                    h5("Use this to select two or more countries to compare their
                       medical spending or child mortality.")
               
                  ),
               
                  # Show a plot of the generated distribution
                  mainPanel(
                    h3("To what extent does purchase power impact medical spending and
                           child mortality?"),
                    tabsetPanel(
                      tabPanel("Child_mortality VS Medical Spending", plotOutput("year_plot"),
                      p("This graph illustrates the trend between rate of child
                        mortality and medical spending for a variety of countries
                        over time. Its clear from the data that as medical
                        spending increases within each country, the general
                        rate of infant mortality decreases. This indicates
                        that better healthcare has a distinct impact in general
                        on infant survival rates.")),
                      tabPanel("Compare 2 countries", plotOutput("compare_plot"),
                      p("This interactive chart allows you to compare the
                        medical spending and infant mortality rates of
                        varying countries around the world. Its clear through
                        individual examination that in general, a country that
                        spends more on its healthcare has a lower rate of
                        child mortality, regardless of their geographic
                        location.")),
                      h4("Effects and Implications"),
                      p("This illustration uses medical spending and child
                        mortality to illustrate the varying trends of child
                        mortality and the impacts medical spending has on this.
                        It is clear that regardless of geographical location,
                        increased spending on healthcare has a better impact
                        on child mortality rates."),
                      p("It is also interesting to note that medical spending
                        in general appears to be on the rise in varying
                        parts of the world over time. This in turn has an impact
                        on child mortality rates, which can thus be said that
                        the increased ability to afford medical care has
                        tangible benefits on child health and mortality. This
                        can indicate the serious importance of providing
                        effective medical care that people are willing and able
                        to spend on, as it has a significant impact on the
                        survival rates of children.")
               
                    )
                  )
                )
                ),
      tabPanel("Purchasing Power and Food Wastage",
                 
                 # Sidebar with a slider input for number of bins 
                 sidebarLayout(
                   sidebarPanel(
                     radioButtons("pick_income", label = h3("Pick a income level"),
                                  choices = list("High Income" = "HIC", 
                                                 "Upper Medium Income" = "UMC",
                                                 "Lower Medium income" = "LMC", 
                                                 "Lower Income" = "LIC")),
                     h5("Use this selection to select the display of food waste
                        and PPP per capita for different national PPP brackets."),
                     selectInput("select_columns", 
                                 label = h3("Compare PPP per Capita or Food Waste"),
                                 list("Food Waste" = "food.waste",
                                      "PPP per capita" = "PPP.capita")),
                     h5("Use this to choose if you wish to compare countries 
                        based on PPP per capita or food wastage amount."),
                     selectInput("select_country_1", label = h3("Pick a country"), 
                                 choices = unique(foodwaste_PPP$Country.Name),
                                 multiple = TRUE),
                     h5("Use this selection to choose two or more countries to
                        compare on the basis of food wastage or PPP per capita.")
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel(
                     h3("To what extent does purchase power impact the 
                        consumption and wastage of food?"),
                     tabsetPanel(
                       tabPanel("Food Waste VS PPP capita",plotOutput("scatterPlot"),
                                p("This chart illustrates the relationship
                                  between food waste and the PPP per capita
                                  of a country, segmented by the different
                                  development and income groups of each 
                                  country. Its interesting to note that for
                                  richer nations, its as expected; a greater
                                  income correlates positively with percent of
                                  food wasted. However, just as interesting is 
                                  the poorer nations have a larger proportion
                                  of food wastage. There could be multiple 
                                  explainataions for this, but the data does not
                                  give an indication of what it might be.")),
                       tabPanel("Food Waste VS PPP capita Comparison", 
                                plotOutput("compare_plot_1"),
                                p("This interactive chart selection illustrates
                                  the PPP per capita or food waste percentage 
                                  of a country in comparision to other countries.
                                  It is clear that throughout various regions
                                  of the world, the interesting trend is
                                  reinforced; poorer countries have a negative 
                                  correlation to income and food wastage, which
                                  is counter-intuitive.")),
                       h4("Effects and Implications"),
                       p("These illustrations compare and contrast the PPP per
                         capita of a country and the percentage of food waste
                         that occurs in these nations. It is curious to note that
                         while our expected outcome of food wastage increasing
                         with PPP, this only applied to richer nations. Poorer
                         nations tended to have an opposite trend."),
                       p("It cannot be definitvely said what is the root cause
                         of this counter-intuitive trend, but there are multiple
                         possibilities. One could be the potential increase of
                         agricultue reliance in these countries; the data does
                         not make a distinction between consumer and industrial
                         food waste, and the agricultural process could be
                         more wasteful than we think. Another could be the 
                         impact of income disparity in these countries; it could
                         be that the wealthy in the countries purchases a
                         large amount of food which goes to waste, which does
                         not reflect on the population as a whole. Thus, more
                         research would need to be done to explore this further.")
                       
                     )
                   )
                 )
               )
      
    
  )
  

))

