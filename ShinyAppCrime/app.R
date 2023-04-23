library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(showtext)
library(lubridate)
library(viridis) 
library(zoo)
library(DT)
library(shinythemes)
library(thematic)
library(bslib)


dispatch_calls2023 <- read.csv("https://raw.githubusercontent.com/karolo89/Raw_Data/main/dispatchedcalls_opendata_2023_1.csv")|>
  select(-ReportDateTime)|>
  mutate(Priority = as.factor(Priority))|>
  mutate(FinalCallCategory = as.factor(FinalCallCategory)) |>
  mutate(FinalCallGroup = as.factor(FinalCallGroup))|>
  mutate(Neighborhood = as.factor(Neighborhood))|>
  mutate(ReportMonthYear = mdy(ReportMonthYear))|>
  mutate_if(is.character, as.double)|>
  mutate(ResponseTime= ResponseTime_sec/60)|>
  separate("ReportMonthYear", c("Year", "Month", "Day"), sep = "-")|>
  select(-Day)|>
  mutate(date= as.yearmon(paste(Year, Month), "%Y %m"))

dispatch_calls2022 <-read.csv("https://raw.githubusercontent.com/karolo89/Raw_Data/main/dispatchedcalls_opendata_2022_0.csv")|>
  mutate(Priority = as.factor(Priority))|>
  mutate(FinalCallCategory = as.factor(FinalCallCategory)) |>
  mutate(FinalCallGroup = as.factor(FinalCallGroup))|>
  mutate(Neighborhood = as.factor(Neighborhood))|>
  mutate(ReportMonthYear = mdy(ReportMonthYear))|>
  mutate_if(is.character, as.double)|>
  mutate(ResponseTime= ResponseTime_sec/60)|>
  separate("ReportMonthYear", c("Year", "Month", "Day"), sep = "-")|>
  select(-Day)|>
  mutate(date= as.yearmon(paste(Year, Month), "%Y %m"))

calls <- rbind(dispatch_calls2022, dispatch_calls2023)

month_call <- calls |>
  group_by(date, Year, FinalCallGroup)|>
  filter(FinalCallGroup!= "NULL") |>
  summarise(count = n()) |>
  arrange(desc(count))|>
  na.omit() |> 
  ungroup()

group_month <- calls |>
  group_by(date, FinalCallGroup)|>
  summarise(num= n())|>
  mutate(date= as.Date(date))|>
  arrange(desc(num))|>
  top_n(5) |>
  ungroup()



custom_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = "#000000",
  primary = "#0199F8",
  secondary = "#FF374B",
  base_font = "Maven Pro"
)

thematic::thematic_shiny()



# Define the user interface
ui <- fluidPage(theme = custom_theme,
                # Application title
                
                titlePanel("Dispatched Calls for Service- Portland Police Bureau"),
                # sidebar
                sidebarLayout(
                  sidebarPanel(
                    dateRangeInput(inputId = "date",
                                   label   = "Enter date",
                                   start   = "2022-01-01",
                                   end     = "2023-03-01",
                                   min     = "2022-01-01",
                                   max     = "2023-03-01"),
                    
                    selectInput(inputId  = "FinalCallGroup",
                                label    = "Select Call Type",
                                choices  = group_month$FinalCallGroup,
                                multiple = TRUE,
                                selected = "Disorder"),
                    plotOutput("density")),
                  # main panel
                  mainPanel(
                    plotOutput(outputId = "plot"),
                    dataTableOutput(outputId = "table")
                  )
                )
)


# Create server function

server <- function(input, output) {
  GM <- reactive({ group_month |> 
      filter(date >= input$date[1], 
             date <= input$date[2]) |> 
      filter(FinalCallGroup == input$FinalCallGroup)
  })
  TC <- reactive({  calls |> 
      filter(FinalCallGroup == input$FinalCallGroup) |> 
      arrange(date)})
  
  output$plot <- renderPlot({
    GM() |> 
      ggplot(aes(x= fct_rev(fct_reorder(FinalCallGroup, num)), y= num))+
      geom_bar(stat = 'identity', aes(color=FinalCallGroup, fill= FinalCallGroup))+
      
      scale_y_continuous(labels = scales::number_format(scale = .001, suffix = "K"))+
      
      scale_color_viridis_d()+
      scale_fill_viridis_d()+
      
      labs(title= "Total Calls by Group, 2022-23",
           x="",
           y= "") +
      theme_minimal()+
      theme(
            legend.position = "none",
            panel.background = element_rect(colour = "#fdf8ec", fill = "#fdf8ec"),
            plot.background = element_rect(colour = "#fdf8ec", fill = "#fdf8ec"),
            legend.background = element_rect(colour = "#fdf8ec", fill = "#fdf8ec"))
  })
  
  output$table <- DT::renderDataTable({
    TC() |> 
      datatable(options = list(scrollX = T))
  })
  output$density <- renderPlot({
    TC() |> ggplot() + aes(y=ResponseTime, x=FinalCallGroup, color=FinalCallGroup) +
      geom_boxplot()+
      labs(title= "Response Time by Call Type",
           x="",
           y= "min")+
      theme(legend.position = "none")+
      theme_minimal()+
      theme(
            legend.position = "none",
            panel.background = element_rect(colour = "#fdf8ec", fill = "#fdf8ec"),
            plot.background = element_rect(colour = "#fdf8ec", fill = "#fdf8ec"),
            legend.background = element_rect(colour = "#fdf8ec", fill = "#fdf8ec"))
  })
  
}





# Build and run the application
run_with_themer(shinyApp(ui = ui, server = server))
