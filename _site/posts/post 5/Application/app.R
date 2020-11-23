# Libraries

library(tidyverse)
library(countrycode)
library(broom)
library(patchwork)
library(shiny)
library(shinydashboard)

# reading data 

height_country <- read.csv("data/NCD_RisC_eLife_2016_height_age18_countries.txt")
BP_country <- read.csv("data/NCD_RisC_Lancet_2016_BP_age_standardised_countries.txt")
BMI_country <- read.csv("data/NCD_RisC_Lancet_2017_BMI_age_standardised_country.txt")
colesteral_country <- read.csv("data/NCD_RisC_Nature_2020_Cholesterol_age_standardised_countries.txt")
country_status <- read.csv("data/country.csv") %>%
    mutate(Status = as.factor(Status))

# Data wrangling 

height <- height_country %>%
    mutate(continent = countrycode(sourcevar = Country,
                                   origin = "country.name",
                                   destination = "continent")) %>%
    select(Country,continent,ISO,Sex,Year.of.birth,Mean.height..cm.) %>%
    rename(height = Mean.height..cm.)

BP <- BP_country %>%
    select(Country.Region.World,ISO,Sex,Year,Mean.diastolic.blood.pressure..mmHg.,Mean.systolic.blood.pressure..mmHg.,Prevalence.of.raised.blood.pressure)

BP_ID <- BP %>%
    mutate(ID = paste(Country.Region.World,Sex,Year)) %>%
    select(-Country.Region.World,-ISO,-Sex,-Year)


BMI <- BMI_country %>%
    select(Country.Region.World,ISO,Sex,Year,Mean.BMI)

BMI_ID <- BMI %>%
    mutate(ID = paste(Country.Region.World,Sex,Year))%>%
    select(-Country.Region.World,-ISO,-Sex,-Year)

Col <- colesteral_country %>%
    select(Country.Region.World,ISO,Sex,Year,Mean.total.cholesterol..mmol.L.,Mean.non.HDL.cholesterol..mmol.L.,Mean.HDL.cholesterol..mmol.L.)

Col_ID <- Col %>%
    mutate(ID = paste(Country.Region.World,Sex,Year))%>%
    select(-Country.Region.World,-ISO,-Sex,-Year)

height_join <- height %>%
    mutate(ID = paste(Country,Sex,Year.of.birth))

Joined <- height_join %>%
    inner_join(BP_ID,
               by = "ID") %>%
    inner_join(BMI_ID,
               by = "ID") %>%
    inner_join(Col_ID,
               by = "ID") %>%
    rename(Year = Year.of.birth)

country_plot <- country_status %>%
    mutate(continent = countrycode(sourcevar = Country,
                                   origin = "country.name",
                                   destination = "continent"))

height_status <- height %>%
    left_join(country_status,
              by = "Country") %>%
    group_by(Status,Year.of.birth,Sex)

#Section 1 plots 

h1 <- height %>%
    group_by(Sex,Year.of.birth) %>%
    summarise(height = mean(height)) %>%
    mutate(Country = "World")

BMI2 <- BMI %>%
    group_by(Sex, Year) %>%
    summarise(Mean.BMI = mean(Mean.BMI)) %>%
    mutate(Country.Region.World = "World")

BP2 <- BP %>%
    group_by(Sex, Year) %>%
    summarise(Prevalence.of.raised.blood.pressure = mean(Prevalence.of.raised.blood.pressure)) %>%
    mutate(Country.Region.World = "World")

Col2 <- Col %>%
    group_by(Sex, Year) %>%
    summarise(Mean.non.HDL.cholesterol..mmol.L. = mean(Mean.non.HDL.cholesterol..mmol.L.)) %>%
    mutate(Country.Region.World = "World")

height_country_plot <- height %>%
    ggplot(aes(x = Year.of.birth,
               y = height,
               group = Country)) +
    geom_line(alpha = 1/10) +
    geom_line(data = h1, aes(), colour = "blue", size = 1.1) +
    facet_wrap(~Sex) +
    labs(x = "Year",
         y = "Average Height - cm")+
    theme(legend.position = "none")

BP_country_plot <- BP %>%
    ggplot(aes(x = Year,
               y = Prevalence.of.raised.blood.pressure,
               group = Country.Region.World)) +
    geom_line(alpha = 1/10) +
    geom_line(data = BP2, colour = "Blue", size = 1.1) +
    facet_wrap(~Sex) +
    labs(y = "Prevalence of raised blood pressure")+
    theme(legend.position = "none")

BMI_country_plot <- BMI %>%
    ggplot(aes(x = Year,
               y = Mean.BMI,
               group = Country.Region.World)) +
    geom_line(alpha = 1/10) +
    geom_line(data = BMI2, colour = "Blue", size = 1.1) +
    facet_wrap(~Sex) +
    labs(y = "BMI - kg/m2") +
    theme(legend.position = "none")


Col_country_plot <- Col %>%
    ggplot(aes(x = Year,
               y = Mean.non.HDL.cholesterol..mmol.L.,
               group = Country.Region.World)) +
    geom_line(alpha = 1/10) +
    geom_line(data = Col2, colour = "Blue", size = 1.1) +
    facet_wrap(~Sex) +
    labs(y = "mean non HDL cholesterol - mmol.L")+
    theme(legend.position = "none")

# Section 2

height.BMI <- lm(height ~ Mean.BMI, data = Joined)
col.BMI <- lm(Mean.non.HDL.cholesterol..mmol.L. ~ Mean.BMI, data = Joined)
BP.BMI <- lm(Prevalence.of.raised.blood.pressure ~ Mean.BMI, data = Joined)

# Section 3 plots
## Height 
h1 <- height %>%
    group_by(continent,Year.of.birth,Sex) %>%
    summarise(height = mean(height)) %>%
    ggplot(aes(x = Year.of.birth, y = height,color = continent)) +
    geom_line(size = 1) +
    facet_wrap(~Sex) +
    labs(x = "Year",
         y = "Average Height - cm",
         title = "Trend in Average Height by Continent")


h2 <- height_status %>%
    summarise(height = mean(height)) %>%
    ggplot(aes(x = Year.of.birth, y = height,color = Status)) +
    geom_line(size = 1) +
    facet_wrap(~Sex) +
    labs(x = "Year",
         y = "Average Height - cm",
         title = "Trend in Average Height between Wealth Levels")

## BMI
BMI2 <- BMI %>%
    mutate(continent = countrycode(sourcevar = Country.Region.World,
                                   origin = "country.name",
                                   destination = "continent")) %>%
    left_join(country_status,
              by = c("Country.Region.World"= "Country"))

BMI_continent <- BMI2 %>%
    group_by(Sex, Year, continent) %>%
    summarise(BMI = mean(Mean.BMI)) %>%
    ggplot(aes(x = Year,
               y = BMI,
               color = continent)) +
    geom_line(size = 1) +
    facet_wrap(~Sex) +
    labs(y = "BMI - kg/m2",
         title = "Trend in Average BMI across continents")

BMI_wealth <- BMI2 %>%
    group_by(Status,Year,Sex) %>%
    summarise(BMI = mean(Mean.BMI)) %>%
    ggplot(aes(x = Year,
               y = BMI,
               color = Status )) +
    geom_line(size = 1) +
    facet_wrap(~Sex) +
    labs(y = "BMI - kg/m2",
         title = "Trend in Average BMI across Wealth Levels")

## Cholesterol 

BP2 <- BP %>%
    mutate(continent = countrycode(sourcevar = Country.Region.World,
                                   origin = "country.name",
                                   destination = "continent"))%>%
    left_join(country_status,
              by = c("Country.Region.World"= "Country"))

BP_cont <- BP2 %>%
    group_by(Sex, Year, continent) %>%
    summarise(Prevalence.of.raised.blood.pressure = mean(Prevalence.of.raised.blood.pressure)) %>%
    ggplot(aes(x = Year,
               y = Prevalence.of.raised.blood.pressure,
               color = continent)) +
    geom_line(size = 1) +
    facet_wrap(~Sex) +
    labs(y = "Prevalence of raised blood pressure",
         title = "Trend in prevalence of raised blood pressure by continent")

BP_wealth <- BP2  %>%
    group_by(Sex, Year, Status) %>%
    summarise(Prevalence.of.raised.blood.pressure = mean(Prevalence.of.raised.blood.pressure)) %>%
    ggplot(aes(x = Year,
               y = Prevalence.of.raised.blood.pressure,
               color = Status)) +
    geom_line(size = 1) +
    facet_wrap(~Sex) +
    labs(y = "Prevalence of raised blood pressure",
         title = "Trend in prevalence of raised blood pressure by wealth level")

## Cholesterol 

Col2 <- Col %>%
    mutate(continent = countrycode(sourcevar = Country.Region.World,
                                   origin = "country.name",
                                   destination = "continent")) %>%
    left_join(country_status,
              by = c("Country.Region.World"= "Country"))

Col_cont <- Col2 %>%
    group_by(Sex, Year, continent) %>%
    summarise(Mean.non.HDL.cholesterol..mmol.L. = mean(Mean.non.HDL.cholesterol..mmol.L.)) %>%
    ggplot(aes(x = Year,
               y = Mean.non.HDL.cholesterol..mmol.L.,
               color = continent)) +
    geom_line(size = 1) +
    facet_wrap(~Sex) +
    labs(y = "mean non HDL cholesterol - mmol.L")

Col_wealth <- Col2  %>%
    group_by(Sex, Year, Status) %>%
    summarise(Mean.non.HDL.cholesterol..mmol.L. = mean(Mean.non.HDL.cholesterol..mmol.L.)) %>%
    ggplot(aes(x = Year,
               y = Mean.non.HDL.cholesterol..mmol.L.,
               color = Status)) +
    geom_line(size = 1) +
    facet_wrap(~Sex) +
    labs(y = "mean non HDL cholesterol - mmol.L") 

## Section 3 cont-wealth
country_plot <- country_status %>%
    mutate(continent = countrycode(sourcevar = Country,
                                   origin = "country.name",
                                   destination = "continent"))

## Reference 

reftab <- tibble(References = c("Arel-Bundock, Vincent, Nils Enevoldsen, and CJ Yetman. 2018. “Countrycode: An R Package to Convert Country Names and Country Codes.” Journal of Open Source Software 3 (28): 848. https://doi.org/10.21105/joss.00848.",
                                "Auguie, Baptiste. 2017. GridExtra: Miscellaneous Functions for 'Grid' Graphics. https://CRAN.R-project.org/package=gridExtra.",
                                "“Body Mass Index - Bmi.” 2020. World Health Organization. World Health Organization. https://www.euro.who.int/en/health-topics/disease-prevention/nutrition/a-healthy-lifestyle/body-mass-index-bmi.",
                                "Health & Human Services, Department of. 2014. “Cholesterol.” Better Health Channel. Department of Health & Human Services. https://www.betterhealth.vic.gov.au/health/conditionsandtreatments/cholesterol.",
                                "“High Cholesterol Is Responsible for About 3.9 Million Worldwide Deaths.” n.d. RisC. http://ncdrisc.org/.",
                                "“List of Developing Countries as Declared by the Minister for Foreign Affairs.” n.d. DFAT. https://www.dfat.gov.au/about-us/publications/Pages/list-of-developing-countries-as-declared-by-the-minister-for-foreign-affairs.",
                                "“Physical Inactivity: A Global Public Health Problem.” 2014. World Health Organization. World Health Organization. https://www.who.int/dietphysicalactivity/factsheet_inactivity/en/.",
                                "“Raised Blood Pressure.” 2015. World Health Organization. World Health Organization. https://www.who.int/gho/ncd/risk_factors/blood_pressure_prevalence_text/en/#:~:text=Globally, the overall prevalence of,modestly between 1980 and 2008.",
                                "Robinson, David, Alex Hayes, and Simon Couch. 2020. Broom: Convert Statistical Objects into Tidy Tibbles. https://CRAN.R-project.org/package=broom",
                                "Tierney, Nicholas. 2017. “Visdat: Visualising Whole Data Frames.” JOSS 2 (16): 355. https://doi.org/10.21105/joss.00355.",
                                "Wickham, Hadley, Mara Averick, Jennifer Bryan, Winston Chang, Lucy D’Agostino McGowan, Romain François, Garrett Grolemund, et al. 2019. “Welcome to the tidyverse.” Journal of Open Source Software 4 (43): 1686. https://doi.org/10.21105/joss.01686",
                                "Zhu, Hao. 2020. KableExtra: Construct Complex Table with ’Kable’ and Pipe Syntax. https://CRAN.R-project.org/package=kableExtr"))



ui <- dashboardPage(
    dashboardHeader(title = "FIT5147: Narrative Visualisation Project Project"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Title", tabName = "dashboard1"),
            menuItem("Introduction", tabName = "dashboard2"),
            menuSubItem("Data Wrangling", tabName = "dashboard3"),
            menuSubItem("Data Checking", tabName = "dashboard4"),
            menuItem("World and Country Trends", tabName = "dashboard5"),
            menuItem("Health Factor Regression Analysis", tabName = "dashboard6"),
            menuItem("Continent and Wealth Level Trends", tabName = "dashboard7"),
            menuItem("Conclusion", tabName = "dashboard8"),
            menuSubItem("Reflection", tabName = "dashboard9"),
            menuItem("References", tabName = "dashboard10")
            )
        ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard1",
                    fluidPage(
                        titlePanel("FIT5147 Narrative Visualisation Project: Exploring World Health Data"),
                        h3("Vinny Vu ")
                    )
            ),
            tabItem(tabName = "dashboard2",
                    fluidPage(
                        titlePanel("Introduction"),
                        textOutput("intro1"),
                        br(),
                        textOutput("intro2"),
                        br(),
                        textOutput("intro3"),
                        br(),
                        textOutput("intro4"),
                        br(),
                        textOutput("intro5"),
                        br(),
                        textOutput("intro6"),
                        h4("•	What is the trend in the world health data across time and what are the specific trends for each country (BMI, Height, Blood Pressure and Cholesterol)?"),
                        h4("•	Is there a relationship between BMI and other health factors (Height, Blood Pressure and Cholesterol) and what is it?"),
                        h4("•	Do countries within the same continent or with similar cultures share similar results and trends and what are these results/trends?")
                    )
            ),
            tabItem(tabName = "dashboard3",
                    fluidPage(
                        titlePanel("Data Wrangling"),
                        textOutput("Wran1"),
                        br(),
                        textOutput("Wran2"),
                        br(),
                        textOutput("Wran3"),
                        br(),
                        textOutput("Wran4"),
                        br(),
                        textOutput("Wran5"),
                        br(),
                        textOutput("Wran6"),
                        br(),
                        textOutput("Wran7")
                    )
            ),
            tabItem(tabName = "dashboard4",
                    fluidPage(
                        titlePanel("Data Checking"),
                        textOutput("Check1"),
                        br(),
                        textOutput("Check2")
                    )
            ),
            tabItem(tabName = "dashboard5",
                    fluidPage(
                        titlePanel("What is the trend in the world health data across time and what are the specific trends for each country (BMI, Height, Blood Pressure and Cholesterol)?"),
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("healthfac", "Select Health Factor", choices = c("Height", "BMI", "Blood Pressure", "Cholesterol")),
                                selectInput("country", "Select Country", height$Country, selected = "Australia"),
                                textOutput("Description1")
                                ),
                            mainPanel(
                                h3("Trend in World Health Data with Men in Red and Women in Blue"),
                                plotOutput("world"),            
                                h3("Trend in Country Health data - with specified country in red and world average in blue"),
                                plotOutput("Country")
                                )
                            )
                        )
                    ),
            tabItem(tabName = "dashboard6",
                    fluidPage(
                        titlePanel("Is there a relationship between BMI and other health factors (Height, Blood Pressure and Cholesterol) and what is it?"),
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("trend", "Select Variable", choices = c("BMI vs Height", "BMI vs Blood Pressure", "BMI vs Cholesterol")),
                                textOutput("description2")
                                ),
                            mainPanel(
                                plotOutput("trend"),
                                plotOutput("regression"),
                                tableOutput("reg")
                                )
                            )
                        )
                    ),
            tabItem(tabName = "dashboard7",
                    fluidPage(
                        titlePanel("Do countries within the same continent or with similar cultures share similar results and trends and what are these results/trends?"),
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("health", "Select Health Factor", choices = c("Height","BMI","Cholesterol","Blood Pressure")),
                                selectInput("continent", "Select Continent", height$continent, selected = "Oceania"),
                                selectInput("wealth", "Select Wealth Level", country_plot$Status),
                                plotOutput("prop_plot"),
                                textOutput("descriptions3"),
                                br(),
                                textOutput("descriptions4")
                                ),
                            mainPanel(
                                plotOutput("heightcont"),
                                plotOutput("heightwealth")
                                )
                            )
                        )
                    ),
            tabItem(tabName = "dashboard8",
                    fluidPage(
                        titlePanel("Conclusion"),
                        textOutput("Conc")
                    )
            ),
            tabItem(tabName = "dashboard9",
                    fluidPage(
                        titlePanel("Reflection"),
                        textOutput("reflect")
                    )
            ),
            tabItem(tabName = "dashboard10",
                    fluidPage(
                        titlePanel("References"),
                        tableOutput("reftab")
                    )
            )
            )
        )
    )


server <- function(input, output) {
    output$Description1 <- renderText({
        if (input$healthfac == "Height"){
            print("To assess the trend in the world average height the height data set was used to calculate the average height across all countries for males and females to be used to develop the line graph.
                  From the graph we can see there is an upward trend in average height that plateaus and slightly dips around 1975. 
                  The average difference in men and women height appear to be fairly constant each year being around 10-15cm.") 
        }
        else if (input$healthfac == "BMI"){
            print("To assess the trend in the world average BMI levels the BMI data set was used to calculate the average BMI across all countries for males and females to be used to develop the line graph. 
                  For the purposes of analysis we will only be looking at the average BMI not the prevalence of each BMI level. 
                  From the graph we can see there is an upward trend in average BMI across all years. The average difference in men and women height appear to be fairly constant each year being around 5-8 kg/m2 with women being higher than men across all years.")
        }
        else if (input$healthfac == "Blood Pressure"){
            print("To assess the trend in the world average blood pressure levels the blood pressure data set was used to calculate the average blood pressure for each type across all countries for males and females to be used to develop the line graph. 
                  From the graph we can see mean systolic and diastolic blood pressure remains fairly constant with a slight downward trend for women. 
                  We do see a larger downward trend in the prevalence of raised blood pressure, however.")
        }
        else if (input$healthfac == "Cholesterol"){
            print("To assess the trend in the world average cholesterol levels the cholesterol data set was used. 
                  For purposes of analysis only the trend in non HDL or Low-density lipoprotein (LDL) cholesterol was assessed. 
                  To calculate the average non HDL cholesterol across all countries for males and females to be used to develop the line graph. 
                  From the graph we can see an upward trend from 1980 peaking in 1990 followed by a downward trend after. 
                  The levels between men and women appear fairly similar across all years.")
        }
    })
    output$world <- renderPlot({
        if (input$healthfac == "Height"){
            height %>%
                group_by(Sex,Year.of.birth) %>%
                summarise(height = mean(height)) %>%
                ggplot(aes(x = Year.of.birth, y = height, color = Sex)) +
                geom_line() +
                labs(x = "Year",
                     y = "Average Height - cm",
                     title = "Trend in World Average Height")+
                theme(legend.position = "none")+
                theme_minimal()
        }
        else if (input$healthfac == "BMI"){
            BMI %>%
                group_by(Sex, Year) %>%
                summarise(BMI = mean(Mean.BMI)) %>%
                ggplot(aes(x = Year,
                           y = BMI,
                           color = Sex)) +
                geom_line() +
                labs(y = "BMI - kg/m2",
                     title = "Trend in World Average BMI")+
                theme(legend.position = "none")+
                theme_minimal()
        }
        else if (input$healthfac == "Blood Pressure"){
            BP1 <- BP_country %>%
                group_by(Sex, Year) %>%
                summarise("mean systolic blood pressure" = mean(Mean.systolic.blood.pressure..mmHg.),
                          "mean diastolic blood pressure" = mean(Mean.diastolic.blood.pressure..mmHg.),
                          "mean prevalence of raised blood pressure" = mean(Prevalence.of.raised.blood.pressure)) %>%
                ggplot(aes(x = Year, color = Sex)) +
                geom_line(aes(y = `mean systolic blood pressure`)) +
                labs(y = "" , title = "Mean systolic blood pressure in mmHg" ) +
                theme(legend.position = "none")+
                theme_minimal()
            
            BP2 <- BP_country %>%
                group_by(Sex, Year) %>%
                summarise("mean systolic blood pressure" = mean(Mean.systolic.blood.pressure..mmHg.),
                          "mean diastolic blood pressure" = mean(Mean.diastolic.blood.pressure..mmHg.),
                          "mean prevalence of raised blood pressure" = mean(Prevalence.of.raised.blood.pressure)) %>%
                ggplot(aes(x = Year, color = Sex)) +
                geom_line(aes(y = `mean diastolic blood pressure`)) +
                labs(y = "" , title = "Mean diastolic blood pressure in mmHg")+
                theme(legend.position = "none")+
                theme_minimal()
            
            BP3 <- BP_country %>%
                group_by(Sex, Year) %>%
                summarise("mean systolic blood pressure" = mean(Mean.systolic.blood.pressure..mmHg.),
                          "mean diastolic blood pressure" = mean(Mean.diastolic.blood.pressure..mmHg.),
                          "mean prevalence of raised blood pressure" = mean(Prevalence.of.raised.blood.pressure)) %>%
                ggplot(aes(x = Year, color = Sex)) +
                geom_line(aes(y = `mean prevalence of raised blood pressure`)) +
                labs(y = "" , title = "Mean prevalence of raised blood pressure")+
                theme(legend.position = "none")+
                theme_minimal()
            grid.arrange(BP1,BP2,BP3)
        }
        else if (input$healthfac == "Cholesterol"){
            Col %>%
                group_by(Sex, Year) %>%
                summarise(non.HDL = mean(Mean.non.HDL.cholesterol..mmol.L.)) %>%
                ggplot(aes(x = Year,
                           y = non.HDL,
                           color = Sex)) +
                geom_line() +
                labs(y = "mean non HDL cholesterol - mmol.L")+
                theme(legend.position = "none")+
                theme_minimal()
        }
    })
    output$Country <- renderPlot({
        if (input$healthfac == "Height"){
            height_country_plot + 
                geom_line(data = dplyr::filter(height, Country == input$country), aes(colour = "red"), size = 1.1) +
                theme_minimal()
        }
        else if (input$healthfac == "BMI"){
            BMI_country_plot +     
                geom_line(data = dplyr::filter(BMI, Country.Region.World == input$country), aes(colour = "red"), size = 1.1)+
                theme_minimal()
        }
        else if (input$healthfac == "Blood Pressure"){
            BP_country_plot +    
                geom_line(data = dplyr::filter(BP, Country.Region.World == input$country), aes(colour = "red"), size = 1.1)+
                theme_minimal()
        }
        else if (input$healthfac == "Cholesterol"){
            Col_country_plot +
                geom_line(data = dplyr::filter(Col, Country.Region.World == input$country), aes(colour = "red"), size = 1.1)+
                theme_minimal()
        }
    })
    output$description2 <- renderText({
        if (input$trend == "BMI vs Height"){
            print("To assess the relationship between height and BMI plot shows on the left the regression line for BMI vs Height facet by gender and continent and the dot plot of all BMI and height with the linear regression line added on the right. 
            Summary of the R squared value of the regression is shown in Table 4.1. From the plot we can see there doesn’t appear to be much of a relationship between height and BMI. 
                  This is further supported by the small R squared value shown in the table.")
        }
        else if (input$trend == "BMI vs Blood Pressure"){
            print("To assess the relationship between the prevalence of raised blood pressure and BMI plots shows on the left the regression line for BMI vs Prevalence of raised blood pressure facet by gender and continent and the dot plot of all blood pressure and height points with the linear regression line added on the right. 
                  Summary of the R squared value of the regression is shown in the table.
                  From the plot there appears to be a weak positive relationship between BMI and raised blood pressure, however, this trend is not prominent in the Americas men plot. 
                  This is further supported by the small R squared value shown in the table.")
        }
        else if (input$trend == "BMI vs Cholesterol"){
            print("To assess the relationship between the mean non HDL cholesterol and BMI the plots shows on the left the regression line for mean BMI vs mean non HDL facet by gender and continent and the dot plot of all mean BMI and mean Cholesterol readings with the linear regression line added on the right. 
                  Summary of the R squared value of the regression is shown in the table. 
                  From the plot there appears to be a positive relationship between BMI. 
                  This trend is prominent across all the plots for men and women across all continents. 
                  We can see a stronger relationship compared to that of in for height vs BMI and blood pressure and BMI. 
                  This is further supported by the larger R squared value shown in the table.")
        }
    })
    output$reg <- renderTable({
         
        if (input$trend == "BMI vs Height"){
            height.BMI %>% glance()
        }
        else if (input$trend == "BMI vs Blood Pressure"){
            BP.BMI %>% glance()
        }
        else if (input$trend == "BMI vs Cholesterol"){
            col.BMI %>% glance()
        }
    })
    output$trend <- renderPlot({
        if (input$trend == "BMI vs Height"){
            Joined %>%
                ggplot(aes(y = height,
                           x = Mean.BMI)) +
                geom_point() +
                geom_smooth() +
                facet_grid(continent~Sex) +
                labs(x = "Mean BMI")+
                theme_minimal()
        }
        else if (input$trend == "BMI vs Blood Pressure"){
            Joined %>%
                ggplot(aes(x = Mean.BMI,
                           y = Prevalence.of.raised.blood.pressure)) +
                geom_point() +
                geom_smooth() +
                facet_grid(continent~Sex) +
                labs(y = "Prevalence of raised blood pressure",
                     x = "Mean BMI")+
                theme_minimal()
        }
        else {
            Joined %>%
                ggplot(aes(x = Mean.BMI,
                           y = Mean.non.HDL.cholesterol..mmol.L.)) +
                geom_point() +
                geom_smooth() +
                facet_grid(continent~Sex) +
                labs(y = "mean non HDL cholesterol - mmol.L",
                     x = "Mean BMI")+
                theme_minimal()
        }
    })
    
    output$regression <- renderPlot({
        if (input$trend == "BMI vs Height"){
            Joined %>%
                ggplot(aes(y = height,
                           x = Mean.BMI)) +
                geom_point() +
                geom_smooth(method = 'lm') +
                labs(x = "Mean BMI")+
                theme_minimal()
        }
        else if (input$trend == "BMI vs Blood Pressure"){
            Joined %>%
                ggplot(aes(x = Mean.BMI,
                           y = Prevalence.of.raised.blood.pressure)) +
                geom_point() +
                geom_smooth(method = 'lm') +
                labs(y = "Prevalence of raised blood pressure",
                     x = "Mean BMI")+
                theme_minimal()
        }
        else {
            Joined %>%
                ggplot(aes(x = Mean.BMI,
                           y = Mean.non.HDL.cholesterol..mmol.L.)) +
                geom_point() +
                geom_smooth(method = 'lm') +
                labs(y = "mean non HDL cholesterol - mmol.L",
                     x = "Mean BMI")+
                theme_minimal()
        }
    })
    output$prop_plot <- renderPlot(
        country_plot %>%
            ggplot(aes(x = continent,
                       fill = Status)) +
            geom_bar(position = "fill") +
            labs(x = "",
                 y = "",
                 title = "Proportion of Developed and Developing Countries by Continent")+
            theme_minimal()
    )
    output$descriptions3 <- renderText({
        if (input$health == "Height"){
            print("The first plot shows the trend in average height between different continents by gender. 
                  From the plot we can see there is a clear upward trend in average from 1900 to around 1960. 
                  From this point most continents begin to stabilize whereas Africa and Oceania appear to trend downwards. 
                  This trend is apparent across both men and women. 
                  For both men and women Europe dominates in average height followed by Oceania. 
                  Asia appears to have the lowest average across most years, however, for men the average overtakes Africa around 1980.")
        }
        else if (input$health == "BMI"){
            print("The first plot shows the trend in average BMI across each continent. 
                  From the plot we can see there is a clear upward trend in average BMI cross all continent for men and women. 
                  Oceania dominates with the highest average across all years for both genders. 
                  For men Africa has the lowest average, followed by Asia, then the Americas and Europe across all years. 
                  For women however, the upward trend for Europe is much smaller as we see the Americas average overtaking Europe in the late 1980s.")
        } 
        else if (input$health == "Blood Pressure"){
            print("The first plot shows the trend in the prevalence of raised blood pressure across continents. 
                  From the plot we can see a clear downward trend in raised blood pressure across both genders for all continents accept Africa which has an increasing trend from 1975 to 1995 followed by a downward trend after. 
                  Europe begins with the highest level across both genders and the largest reduction. 
                  For men they are still at the highest level in 2015 but for women reaches the second lowest continent.")
        }
        else if (input$health == "Cholesterol"){
            print("The first plot shows the trend in mean non HDL Cholesterol across continents. 
                  From the plot we can see a clear downward trend for both men and women for Europe and a clear upward trend for both genders for Africa. 
                  The trend for the other three continents appears constant. 
                  We can see the mean non-HDL cholesterol for European women crossing over all continents except Africa in the later years.")
        }        
    })
    
    output$descriptions4 <- renderText({
        if (input$health == "Height"){
            print("The second plot shows the trend in average height between wealth levels (developed and developing.) 
                  From the graph we can see developed countries dominate in average height across all years for both men in women. 
                  There is a clear upward trend in average height from 1900 to around 1960 across both genders and wealth levels. 
                  For the developed countries, the average height appears to stabilize from 1960 onward. 
                  For the developing countries however, there appears to be a dip from 1960 onward increasing the gap between developed and developing countries.")
        }
        else if (input$health == "BMI"){
            print("The second plot shows the trend in average BMI across wealth levels. 
                  From the plot we can see a clear upward trend in BMI for both genders and wealth levels with developed countries dominating with the higher average BMI across both genders for all years. 
                  The upward trend in BMI for men appear similar between developed and developing countries maintaining a similar gap across all years. 
                  For women however, there is a sharper increase in average BMI for developing compared to developed countries closing the gap between the two averages.")
        } 
        else if (input$health == "Blood Pressure"){
            print("The second plot shows the trend in the prevalence of raised blood pressure by wealth level. 
                  From the plot we can see there is a clear downward trend in raised blood pressure across both genders and wealth levels. 
                  The downward trend is more prominent for developed countries. 
                  We can see the developing country line crossing over overtaking the developed countries for both genders at some point.")
        }
        else if (input$health == "Cholesterol"){
            print("The second plot shows the trend in mean non HDL cholesterol across wealth level. 
                  From the plot we can see a clear downward trend for developed countries across both genders and upward trend for developing countries across both genders. 
                  The gap between developed and developing countries reduces reaching its lowest level in 2018.")
        }        
    })
    
    output$heightcont <- renderPlot({
        if (input$health == "Height"){
            h1 +  geom_line(data = dplyr::filter(height, continent == input$continent), aes(group = Country), colour = "grey", alpha = 1/2)+
                theme_minimal()
        }
        else if (input$health == "BMI"){
            BMI_continent + geom_line(data = dplyr::filter(BMI2, continent == input$continent), aes(y = Mean.BMI, group = Country.Region.World), colour = "grey", alpha = 1/2)+
                theme_minimal()
        } 
        else if (input$health == "Blood Pressure"){
            BP_cont + geom_line(data = dplyr::filter(BP2, continent == input$continent), aes(group = Country.Region.World), colour = "grey", alpha = 1/2)+
                theme_minimal()
        }
        else if (input$health == "Cholesterol"){
            Col_cont + geom_line(data = filter(Col2, continent == input$continent), aes(group = Country.Region.World), colour = "grey", alpha = 1/2)+
                theme_minimal()
        }
    })
    output$heightwealth <- renderPlot({
        if (input$health == "Height"){
            h2 + geom_line(data = dplyr::filter(height_status, Status == input$wealth), aes(group = Country), colour = "grey", alpha = 1/2)+
                theme_minimal()
        }
        else if (input$health == "BMI"){
            BMI_wealth + geom_line(data = dplyr::filter(BMI2, Status == input$wealth), aes(y = Mean.BMI, group = Country.Region.World), colour = "grey", alpha = 1/2)+
                theme_minimal()
        } 
        else if (input$health == "Blood Pressure"){
            BP_wealth + geom_line(data = dplyr::filter(BP2, Status == input$wealth), aes(group = Country.Region.World), colour = "grey", alpha = 1/2)+
                theme_minimal()
        }
        else if (input$health == "Cholesterol"){
            Col_wealth + geom_line(data = dplyr::filter(Col2, Status == input$wealth), aes(group = Country.Region.World), colour = "grey", alpha = 1/2)+
                theme_minimal()
        }
    })
    
    output$intro1 <- renderText("In today’s it is common belief that we are becoming increasingly unhealthy because of the move in diets towards often convenient unhealthy food and increased inactivity (World Health Organisation, 2014). To assess this statement, we will be looking into the following health factors, height, body mass index, blood pressure and cholesterol and their changes across time.")
    output$intro2 <- renderText("It is common belief that due to the better access to quality food, nutrients and wealth humans are growing taller over time and therefore it is of interest to analysis the changes in height data.")
    output$intro3 <- renderText("Per the World Health Organization (World Health Organisation,2015) Hypertension or raised blood pressure is the condition in which the blood vessels have persistently raised pressure and the higher the pressure, the harder the heart must pump. Hypertension is a serious medical condition and can increase the risk of heart, brain, kidney, and other diseases being one of the major causes of premature deaths worldwide (World Health Organisation, 2015.) Therefore, it is of interest to assess the trends of raised blood pressure data.")
    output$intro4 <- renderText("Body mass index or BMI for short, is a measure for indicating nutritional status in adults defined as a person’s weight in kilograms divided by the square of the person’s height in meters (kg/m2) (World Health Organisation, 2020). BMI was developed as a risk indicator of diseases, the higher the BMI is often resulted in a higher level of excessive body fat resulting the increased risk of some diseases such as premature death, cardiovascular diseases, blood pressure, osteoarthritis, some cancers and diabetes (World Health Organisation, 2020). Therefore, it is of interest to assess the trends in BMI levels.")
    output$intro5 <- renderText("Per betterhealth Health & Human Services (2014) Cholesterol is defined as a type of fat that is part of all animal cells that is essential for many of the body’s metabolic processes, including the production of hormones, bile and vitamin D. Specifically Low-density lipoprotein (LDL) cholesterol, carries most of the cholesterol that is delivered to cells and is often referred to as “bad” cholesterol as when its level in the bloodstream is high, it can clog up your arteries . High cholesterol leads to fatty deposits developing in the arteries which causes the vessel to narrow eventually becoming blocked leading to heart disease and stroke (BHC, 2014.) Therefore, it is of interest to assess the trends in LDL cholesterol levels.")
    output$intro6 <- renderText("Specifically, we will be analysing world health data to answer the following questions:")
    output$Wran1 <- renderText("Data for the analysis has been taken from the NCD Risk Factor Collaboration (NCD-RisC) a network of health scientist around the world that provides rigorous and timely data on risk factors for non-communicable diseases (NCDs) for 200 countries and territories (RisC, n.d.).")
    output$Wran2 <- renderText("Body-Mas Index data: This data set measures the yearly average national adult body-mass index for all countries from 1975 to 2016. The variables include the country, sex, year, mean BMI with the lower and upper 95% uncertainty intervals, and the prevalence of individuals falling into each BMI level with the lower and upper 95% uncertainty intervals. For the purposes of analysis, we will only be looking at the mean BMI for each country.")
    output$Wran3 <- renderText("The data sets used include the following:")
    output$Wran4 <- renderText("Height data: This data set measures the yearly mean height for adults (age 18) for all countries from 1896 to 1996. The variables include the country, sex, year of birth, mean height in cm with the lower and upper 95% uncertainty intervals.")
    output$Wran5 <- renderText("Blood Pressure data: This data set measures the yearly mean blood pressure readings (systolic, diastolic and prevalence of raised blood pressure) for adults for all countries from 1975 to 2015. The variables include the country, sex, year, mean systolic blood pressure reading (in mmHg) with the upper and lower 95% uncertainty interval, mean diastolic blood pressure reading (in mmHg) with the upper and lower 95% uncertainty interval and the prevalence of raised blood pressure with the upper and lower 95% uncertainty interval. For the purposes of analysis, we will only be looking at the prevalence of raised blook pressure for each country. ")
    output$Wran6 <- renderText("Cholesterol data: This data set measures the yearly mean cholesterol readings (total cholesterol, non-HDL cholesterol, HDL cholesterol) for adults for all countries from 1980 to 2018. The variables include the country, sex, year, mean total cholesterol reading (in mmol.L) with the upper and lower 95% uncertainty interval, mean non HDL cholesterol reading (in mmol.L) with the upper and lower 95% uncertainty interval and the mean HDL cholesterol reading (in mmol.L) with the upper and lower 95% uncertainty interval. For the purposes of analysis, we will only be looking at the mean non-HDL cholesterol readings for each country.")
    output$Wran7 <- renderText("For the analysis of question 3 we are interested in analysing reading across each continent and developed/ developing countries. To obtain which countries fall into each continent the r package countrycode (Arel-Bundock, Enevoldsen, and Yetman, 2018) was used and to obtain which countries fall into the developed/ developing categories the list was obtained from the Australian Government Department of Foreign Affairs and Trade website (DFAT, n.d.). The developed/developing countries list was converted into excel form to allow importing into R. A left join was used to add the continent and wealth status to each data set. To compare the relationship between BMI with height, blood pressure and cholesterol all data sets were joined using an inner join.")
    output$Check1 <- renderText("The visdat R package was the main tool used in data checking (Tierney, 2017). After checking the NCD-RisC data for completeness it was found there was no missing data. As all the health data was compiled by NCD-RisC naming conventions of countries, country codes and years were all the same and therefore no modifications were needed to variables to allow joining data sets. However, the joined data used for the analysis in section 2 was reduced between the years of 1980-1996 being the only common years across all data sets.")
    output$Check2 <- renderText("Adding the continents with the countrycode package (Arel-Bundock, Enevoldsen, and Yetman, 2018) was able to be used without any missing values and therefore, manipulation of the input countries was not needed. However, naming conventions of countries between differ between the NDC-RisC data and the Developing/Developed country list obtained from Australian Government Department of Foreign Affairs and Trade website. The Developing/Developed country excel list was therefore modified to ensure same naming conventions between both data sets.")
    output$Conc <- renderText("From the analysis conducted we were able to analyse the world health data to explore the trends in country and world height, BMI, blood pressure and cholesterol. In section 4.1 we can see there is an overall upward trend in world height and BMI but an overall downward trend in high blood pressure and cholesterol. However, due to the large amount of countries analysed we were unable to assess individual country trends but use stacked line graphs to assess the overall trend and differences between countries. From section 4.2 we can see there is not a strong relationship between BMI and height or blood pressure, however, there appears to be a strong positive relationship between BMI and Cholesterol. In section 4.3 we were able to analyse the trends in health data between different continents and wealth levels. From the analysis we can see that countries with lower wealth levels appear to record better health readings having lower BMI, blood pressure and Cholesterol readings. However, over time lower wealth countries experience worsening reading whereas, higher wealth countries experience improvements in averages.")
    output$reflect <- renderText("From this analysis I was able to use to use Wickham et al. (2019), Auguie (2017), Arel-Bundock, Enevoldsen, and Yetman (2018), Zhu (2020) and Robinson, Hayes, and Couch (2020) to analyse the world health data using various line graphs, dot plots, regression lines and analyses using ordinary least squares. To further improve this report however, specific countries could have been chosen for analysis as comparing 200 countries on one graph did not reveal much information. Further, other models could be used to assess the relationship between BMI and other health factors could be used given the weak outputs obtained.")
    
    output$reftab <- renderTable(
        reftab
    )

}

 
shinyApp(ui = ui, server = server)
