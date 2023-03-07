library(shiny)
library(shinyWidgets)
library(tidyverse)
df <- read.delim("UAH-lower-troposphere-long.csv.bz2")
ui <- fluidPage(
  titlePanel("UAH Lower Troposphere Temperature Data"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition="input.conditionedPanels==2", 
                       helpText("This panel shows you various graphs on the overall temperature distribution depending on the region. You can select any region you are interested in seeing. You can also click to see a trend line of the graph in question."),
                       selectInput("region_selection",
                                   "Region",
                                   choices = df %>% 
                                     select(region) %>% 
                                     group_by(region) %>% 
                                     distinct()), 
                       pickerInput(inputId = "color",
                                   label = "Select a color:",
                                   choices = c("orange", "darkgreen", "purple"),
                                   selected = "orange"),
                       checkboxGroupInput("trendline", 
                                          label = "Trend Line",
                                          choices = c("Trend Line" = 1), 
                                          selected = c(1))
      ),
      conditionalPanel(condition="input.conditionedPanels==3", 
                       helpText("This panel shows you different time periods and the maximum and minimum temperature for each. The time periods are split up into months, years, and decades."),
                       radioButtons(
                         "time_interval", 
                         "Select time interval:",
                         choices = c("Month", "Year", "Decade")
                       )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", 
                 br(),
                 h1("Temperature Data Summary"),
                 p("This data measures the various temperature data found in different regions."),
                 p("Temperature ",
                   em("temp"),
                   "is measured in Celsius. The temperature is measured in 27 different unique regions from the year 1978-2023."),
                 p("Below is a small random sample of the data"),
                 br(),
                 tableOutput("head"), value=1),
        tabPanel("Plot", plotOutput("yeartempPlot"),textOutput(outputId = "subset_avg"), value=2),
        tabPanel("Table", textOutput("subset_maxmin"), tableOutput("summarystats"), value=3), 
        id = "conditionedPanels"
      )
      
    )))
server <- function(input, output) {
  output$head <- renderTable({
    df [sample (1:nrow (df), 6),]
  })
  output$yeartempPlot <- renderPlot({
    if(1 %in% input$trendline){
      df %>% filter(region==input$region_selection) %>% ggplot(aes(x=year, y=temp)) + 
        geom_point(color=input$color) + geom_smooth(method="lm") + labs(x = "Year", y = "Temperature In Celsisus") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    else{
      df %>% filter(region==input$region_selection) %>% ggplot(aes(x=year, y=temp)) + 
        geom_point(color=input$color) + labs(x = "Year", y = "Temperature In Celsisus") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  subset_data <- reactive({
    df %>% filter(region==input$region_selection)
  })
  output$subset_avg <- renderText({
    avg <- round(mean(subset_data()$temp), 2)
    paste("This selected region has an average temperature of", avg, "degrees celsisus from 1978-2023")
  })
  data_react <- reactive({
    if(input$time_interval == "Month") {
      df %>%
        mutate(Month = month) %>%
        group_by(Month) %>%
        summarise("Max Temp" = max(temp), "Min Temp" = min(temp))
    } else if (input$time_interval == "Year") {
      df %>%
        mutate(Year = year) %>%
        group_by(Year) %>%
        summarise("Max Temp" = max(temp), "Min Temp" = min(temp))
    } else if (input$time_interval == "Decade") {
      df %>%
        mutate(Decade = floor(year/10)*10) %>%
        group_by(Decade) %>%
        summarise("Max Temp" = max(temp), "Min Temp" = min(temp))
    }
  })
  output$subset_maxmin <- renderText({
    var <- input$time_interval
    max_val <- mean(data_react()[["Max Temp"]], 2)
    min_val <- mean(data_react()[["Min Temp"]], 2)
    paste("The average Maximum temperature in this data set is", max_val, "\n",
          "and the average Minimum temperature in this data set is", min_val, "\n")
  })
  output$summarystats <- renderTable({
    data_react()
  })
}
shinyApp(ui = ui, server = server)