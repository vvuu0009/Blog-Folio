#Library
library(shiny)
library(coronavirus)
library(tidyverse)
library(plotly)
library(countrycode)
library(kableExtra)
library(DT)

#Data
data("coronavirus")

#Wrangling 
corona_cont <- coronavirus %>%
    mutate(continent = countrycode(sourcevar = country, 
                                   origin = "country.name",
                                   destination = "continent")) %>%
    group_by(date,continent,type) %>%
    summarise(cases = sum(cases)) %>%
    drop_na()

cum <- coronavirus %>%
    filter(type == "confirmed") %>%
    group_by(country, date) %>%
    summarise(cases = sum(cases)) %>%
    group_by(country) %>%
    mutate('total confirmed' = cumsum(cases))

tab <- coronavirus %>%
    group_by(date,country,type) %>%
    summarise(cases = sum(cases)) %>%
    pivot_wider(names_from = type,
                values_from = cases) %>%
    group_by(country) %>%
    mutate('Total Confirmed' = cumsum(confirmed),
           'Total Deaths' = cumsum(death),
           'Total Recovered' = cumsum(recovered)) %>%
    filter(date == "2020-07-31") %>%
    select(-date,-confirmed,-death,-recovered) %>%
    arrange(-`Total Confirmed`) %>%
    rename(Country = country) %>%
    mutate(`Total Confirmed`= as.numeric(`Total Confirmed`),
           `Total Deaths` = as.numeric(`Total Deaths`),
           `Total Recovered` = as.numeric(`Total Recovered`)) %>%
    mutate(`Total Confirmed`= scales::comma(`Total Confirmed`,1),
           `Total Deaths` = scales::comma(`Total Deaths`,1),
           `Total Recovered` = scales::comma(`Total Recovered`,1))

p1_data <- cum %>%
    filter(country %in% c("US","Brazil","India","Russia","South Africa","Mexico")) 

p1_data$country <- p1_data$country %>% factor(levels = c("US","Brazil","India","Russia","South Africa","Mexico"))

reftab <- tibble(References = c("Arel-Bundock, Vincent, Nils Enevoldsen, and CJ Yetman. 2018. “Countrycode: An R Package to Convert Country Names and Country Codes.” Journal of Open Source Software 3 (28): 848. https://doi.org/10.21105/joss.00848.", 
                      "Chang, Winston, Joe Cheng, JJ Allaire, Yihui Xie, and Jonathan McPherson. 2020. Shiny: Web Application Framework for R. https://CRAN.R-project.org/package=shiny.",
                      "Krispin, Rami, and Jarrett Byrnes. 2020. Coronavirus: The 2019 Novel Coronavirus Covid-19 (2019-nCoV) Dataset. https://CRAN.R-project.org/package=coronavirus.",
                      "Sievert, Carson. 2020. Interactive Web-Based Data Visualization with R, Plotly, and Shiny. Chapman; Hall/CRC. https://plotly-r.com.",
                      "Wickham, Hadley, Mara Averick, Jennifer Bryan, Winston Chang, Lucy D’Agostino McGowan, Romain François, Garrett Grolemund, et al. 2019. “Welcome to the tidyverse.” Journal of Open Source Software 4 (43): 1686. https://doi.org/10.21105/joss.01686.",
                      "Xie, Yihui, Joe Cheng, and Xianying Tan. 2020. DT: A Wrapper of the Javascript Library ’Datatables’. https://CRAN.R-project.org/package=DT.",
                      "Zhu, Hao. 2020. KableExtra: Construct Complex Table with ’Kable’ and Pipe Syntax. https://CRAN.R-project.org/package=kableExtra.",
                      "Health, Australian Government Department of. 2020. “What You Need to Know About Coronavirus (Covid-19).” Australian Government Department of Health. Australian Government Department of Health. https://www.health.gov.au/news/health-alerts/novel-coronavirus-2019-ncov-health-alert/what-you-need-to-know-about-coronavirus-covid-19.",
                      "“Coronavirus.” n.d. World Health Organization. World Health Organization. https://www.who.int/health-topics/coronavirus#tab=tab_1."
))

ui <- fluidPage(
    titlePanel("ETC5523: Shiny Assessment"),
    mainPanel(
        h3("About this Application"),
        textOutput("about"),
        br(),
        textOutput("about2"),
        h3(textOutput("heading")),
        textOutput("section1"),
        br(),
        selectInput("type","Select the case type", choices = corona_cont$type, selected = "confirmed"),
        selectInput("continent", "Select the continent",choices = corona_cont$continent, selected = "Africa"),
        plotOutput("cases"),
        dataTableOutput("sumtab"),
        br(),
        h3("Line Graph Showing Cumulative Confirmed Coronavirus Cases of the Top 6 Countries"),
        textOutput("section2"),
        plotlyOutput("p1"),
        verbatimTextOutput("hover"),
        h3("Line Graph Showing Cumulative Confirmed Coronavirus Cases of the Top Countries excluding the US"),
        textOutput("section3"),
        plotlyOutput("p2"),
        br(),
        h3("Cumulative Case Numbers by Country as at 31st July 2020"),
        textOutput("tab"),
        dataTableOutput("table"),
        h3("References"),
        br(),
        textOutput("refs"),
        br(),
        tableOutput("ref")
        
    ),
    includeCSS("styles.css")
)

server <- function(input, output) {
    
    output$about <- renderText({
        "Coronavirus disease (COVID-19) is an infectious disease caused by a newly discovered coronavirus (WHO, 2020) first reported in December 2019 in Wuhan City in China (AGDOH, 2020) Currently there are no specific vaccines or treatments for the disease, (AGDOH, 2020) causing many countries to enter quarantines and lockdowns to prevent the spread of the disease. 
        For the purposes of analysis I have chosen to assess the state of coronavirus across different continents and the top few countries as at the July 31st 2020."
    })
    
    output$about2 <- renderText({
        "Data has been taken from the coronavirus package from Github (Krispin and Byrnes, 2020) which contains data pulled from the Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE) Coronavirus repository which aggregates data from sources such as the World Health Organization and European Center for Disease Prevention and Control (Krispin and Byrnes, 2020). 
        Data from the coronavirus package contains tabular data with dates, locations, case numbers and case type included daily confirmed, death and recovered."
    })
    
    output$heading <- renderText({
        paste("Line Graph Showing Daily", input$type, "Coronavirus Cases by Continent -", input$continent)
    })
    
    output$cases <- renderPlot(
        corona_cont %>%
            filter(type == input$type) %>%
            filter(continent == input$continent) %>%
            ggplot(aes(x = date, 
                       y = cases,
                       colour = continent)) +
            geom_line(color = "yellow") +
            theme(axis.title.x = element_text(margin = margin(t = 1)),
                  panel.grid.major = element_line(colour = "white", size = 0.1, linetype = "dashed"),
                  panel.grid.minor = element_line(colour = "white", size = 0.1, linetype = "dashed"),
                  legend.position = "none",
                  panel.background = element_rect(fill = "black"),
                  plot.background = element_rect(fill = "black", color = NA),
                  text = element_text(color = "white"),
                  axis.line = element_line(color = "white"),
                  axis.text = element_text(color = "white")) +
            labs(x = "Date",
                 y = paste("Daily", input$type , "Cases"))
    )
    
    output$sumtab <- renderDataTable({
        tab2 <- coronavirus  %>% group_by(date,country,type) %>%
            summarise(cases = sum(cases)) %>%
            pivot_wider(names_from = type,
                        values_from = cases) %>%
            group_by(country) %>%
            mutate('Total confirmed' = cumsum(confirmed),
                   'Total death' = cumsum(death),
                   'Total recovered' = cumsum(recovered)) %>%
            filter(date == "2020-07-31") %>%
            select(-date,-confirmed,-death,-recovered) %>%
            arrange(-`Total confirmed`) %>%
            rename(Country = country) %>%
            mutate(`Total confirmed`= scales::comma(`Total confirmed`,1),
                   `Total death` = scales::comma(`Total death`,1),
                   `Total recovered` = scales::comma(`Total recovered`,1))%>% 
            mutate(continent = countrycode(sourcevar = Country, 
                                           origin = "country.name",
                                           destination = "continent")) %>%
            filter(continent == input$continent)
        
        datatable(tab2, 
                  options = list(columnDefs = list(list(
                      className = 'dt-right', targets = c(2,3,4)
                  )))) %>% 
            formatStyle(columns = c(0,1,2,3,4,5), color = "white", backgroundColor = "black") %>% 
            formatStyle(columns = paste("Total", input$type), color = "white", backgroundColor = "grey")

    })
    
    output$p1 <- renderPlotly({
        p1 <- p1_data %>%
            ggplot(aes(x = date,
                       y = `total confirmed`,
                       colour = country)) +
            geom_line() +
            scale_color_manual(breaks = c("US","Brazil","India","Russia","South Africa", "Mexico"),
                               values = c("yellow","purple", "cyan4", "darkorange", "red", "blue")) +
            theme(axis.title.x = element_text(margin = margin(t = 25)),
                  panel.background = element_rect(fill = "transparent"),
                  plot.background = element_rect(fill = "transparent", color = NA),
                  legend.background = element_rect(fill = "transparent"),
                  text = element_text(color = "white"),
                  axis.line = element_line(color = "white"),
                  axis.text = element_text(color = "white")) +
            labs(y = "Cumulative Coronavirus Cases",
                 x = "Date")
        p1
    })
    
    output$hover <- renderPrint({
        event_data("plotly_hover")
    })
    
    output$p2 <- renderPlotly({
        p2 <- p1_data %>%
            filter(country %in% c("Brazil","India","Russia","South Africa", "Mexico")) %>%
            filter(date <= "2020-06-30" & date >= "2020-04-01") %>%
            ggplot(aes(x = date,
                       y = `total confirmed`,
                       colour = country)) +
            geom_line() +
            scale_color_manual(breaks = c("Brazil","India","Russia","South Africa", "Mexico"),
                               values = c("purple", "cyan4", "darkorange", "red", "blue")) +
            theme(axis.title.x = element_text(margin = margin(t = 25)),
                  panel.background = element_rect(fill = "transparent"),
                  plot.background = element_rect(fill = "transparent", color = NA),
                  legend.background = element_rect(fill = "transparent"),
                  text = element_text(color = "white"),
                  axis.line = element_line(color = "white"),
                  axis.text = element_text(color = "white")) +
            labs(y = "Cumulative Coronavirus Cases",
                 x = "Date")
        p2
    })
    
    output$table <- renderDataTable({
        datatable(tab, 
                  options = list(columnDefs = list(list(
                      className = 'dt-right', targets = c(2,3,4)
                  )))) %>% formatStyle(columns = c(0,1,2,3,4), color = "white", backgroundColor = "black")
    })
    
    output$section1 <- renderText({
        "Below displays the line graph showing the daily coronavirus cases by each continent. 
        The case type and continent displayed wihtin the linegraph can be changed by using the input selector.
        To further compliment the graph data tables are displayed below showing the cumulative cases for the continent selected with the case type column highlighted.
        From the graph and table the Americas clearly has the highest trend in confirmed cases which is still trending upwards as at 31 July.
        Oceania clearly as the lowest counts across all categories, however, this category only has 4 countries compared to the 30-50 countries in eachother continent.
        Another intersting point to note from the Europe daily confirmed cases, we see a downward trend in cases during April but subsequently a pick up in mid Junesuggesting a second wave."
    })
    
    output$section2 <- renderText({
        "The plotly displays the linegraphs for the cumulative coronavirus cases for the 6 countries with the most cases as at 31 July 2020. From the plotly we can clearly see the US has highest cumulative cases across the whole period.
        In fact, in late June we see a significant pick up in cases showing the US has not currently managed to control the spread of cases also. 
        Notably we see in late June Brazil and India sees a significant pickup in cases. 
        The other three countries show significant counts in cases, however, it is hard to view their trends being overshadowed by the large levels of case numbers in the US."
    })
    
    output$section3 <- renderText({
        "To better view the trends across the top countries cumulative confirmed cases, the same plot has been produced restricted the time period from April onwards given the low levels appearing before April.
        Further, the US has been removed to better view the other 5 countries trends. From the plot we can see the trend in Russia and Mexico appear fairly stable. 
        Across the other 3 countries we see a sharpening increase in cases from mid June showing the lack of control over spread." 
    })
    
    output$tab <- renderText({
        "The table below shows the cumulative confirmed, death and recovered coronavirus cases as at 31st July 2020 for each country. 
        From the table we can clearly see the US has the highest total confirmed and death cases being significantly hiehgt than all other countries.
        Relatively, Australia has significantly lower case number ranking 69th in total confirmed cases as at 31st July. 
        However, updating to more recent figures may show different results given the recent spikes in Australian cases from their second wave."
    })
    
    output$refs <- renderText({
        "The following packages have been used to create this application Krispin and Byrnes (2020) Wickham et al. (2019) Sievert (2020) Arel-Bundock, Enevoldsen, and Yetman (2018) Zhu (2020) Xie, Cheng, and Tan (2020) Chang et al. (2020)"
    })
    
    output$ref <- renderTable(
        reftab
    )
    
}

shinyApp(ui = ui, server = server)
