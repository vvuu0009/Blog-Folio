# Libraries

library(tidyverse)
library(countrycode)
library(broom)
library(patchwork)
library(shiny)
library(shinydashboard)
library(gridExtra)

# reading data 

height_country <- read.csv("data/NCD_RisC_eLife_2016_height_age18_countries.csv")
BP_country <- read.csv("data/NCD_RisC_Lancet_2016_BP_age_standardised_countries.csv")
BMI_country <- read.csv("data/NCD_RisC_Lancet_2017_BMI_age_standardised_country.csv")
colesteral_country <- read.csv("data/NCD_RisC_Nature_2020_Cholesterol_age_standardised_countries.csv")
country_status <- read.csv("data/country.csv") %>%
    mutate(Status = as.factor(Status))

# Data wrangling 

height <- height_country %>%
    mutate(continent = countrycode(sourcevar = Country,
                                   origin = "country.name",
                                   destination = "continent")) %>%
    select(Country,continent,Sex,Year.of.birth,Mean.height..cm.) %>%
    rename(height = Mean.height..cm.)

BP <- BP_country %>%
    select(Country.Region.World,Sex,Year,Mean.diastolic.blood.pressure..mmHg.,Mean.systolic.blood.pressure..mmHg.,Prevalence.of.raised.blood.pressure)

BP_ID <- BP %>%
    mutate(ID = paste(Country.Region.World,Sex,Year)) %>%
    select(-Country.Region.World,-Sex,-Year)


BMI <- BMI_country %>%
    select(Country.Region.World,Sex,Year,Mean.BMI)

BMI_ID <- BMI %>%
    mutate(ID = paste(Country.Region.World,Sex,Year))%>%
    select(-Country.Region.World,-Sex,-Year)

Col <- colesteral_country %>%
    select(Country.Region.World,Sex,Year,Mean.non.HDL.cholesterol..mmol.L.)

Col_ID <- Col %>%
    mutate(ID = paste(Country.Region.World,Sex,Year))%>%
    select(-Country.Region.World,-Sex,-Year)

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


ui <- dashboardPage(
    dashboardHeader(title = "FIT5147: Narrative Visualisation Project Project"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("World and Country Trends", tabName = "dashboard5"),
            menuItem("Health Factor Regression Analysis", tabName = "dashboard6"),
            menuItem("Continent and Wealth Level Trends", tabName = "dashboard7")
            )
        ),
    dashboardBody(
        tabItems(
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
}

 
shinyApp(ui = ui, server = server)
