library(readr)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(sf)


# Load data
kc.potholes <- read_csv("KC Pothole Data.csv")
kc.potholes <- kc.potholes %>% 
  filter(!is.na(Longitude), !is.na(Latitude), Longitude != 0, Latitude != 0)

# Load county shapes for Missouri counties
mo_counties <- counties(state = "MO", cb = TRUE, class = "sf")
mo_counties <- st_set_crs(mo_counties, 4326)
# Restrict to just the counties in Kansas City, MO
kcmo_counties <- mo_counties %>%
  filter(NAME %in% c("Jackson", "Clay", "Platte"))

class(kc.potholes$Open.Date.Time)
kc.potholes$Open.Date.Time <- as.Date(kc.potholes$Open.Date.Time, format = "%m/%d/%Y")
class(kc.potholes$avg.h.income)
kc.potholes$avg.h.income <- as.numeric(kc.potholes$avg.h.income)

# Define global variables for the date range
min_date <- min(kc.potholes$Open.Date.Time, na.rm = TRUE)
max_date <- max(kc.potholes$Open.Date.Time, na.rm = TRUE)

# Check the county lines loaded
ggplot(data = kcmo_counties) +
  geom_sf(fill = "lightblue", color = "black") +
  labs(title = "Map of Kansas City Counties in Missouri") +
  theme_minimal()


################### UI ########################
ui <- dashboardPage(
  dashboardHeader(title = "Kansas City Pothole Data"),
  dashboardSidebar(
    selectInput("county", "Select a County:",
                choices = c("All" = "All of Kansas City", unique(kc.potholes$county))),
    sliderInput("dateRange",
                "Select Date Range:",
                min = min_date,
                max = max_date,
                value = c(min_date, max_date),
                timeFormat = "%Y-%m-%d",
                step = 1)
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
            .box-header {
              text-align:center;
            }
            .box-body {
              text-align:center;
            }
        "))),
    fluidRow(
      box(title = "Pothole Map", status = "primary", solidHeader = TRUE,
          plotlyOutput("potholeMap"), width = 12)  # Set width to full row
    ),
    fluidRow(
      column(4, box(title = "Average Response Time", width = 14, status = "warning", solidHeader = TRUE,
                    textOutput("avgResponseTime"))),
      column(4, box(title = "Average Household Income", width = 14, status = "success", solidHeader = TRUE,
                    textOutput("avgHIncome"))),
      column(4, box(title = "Number of Potholes", width = 14, status = "danger", solidHeader = TRUE,
                    textOutput("potholeCount")))
    ),
    
    fluidRow(
      box(title = "Income vs. Pothole Repair Time Regression", status = "primary", solidHeader = TRUE,
          plotOutput("regressionPlot"), width = 12)
    )
  )
)



################## SERVER ########################
server <- function(input, output) {
  # Interactive filter for data based on county and date range
  filtered.data <- reactive({
    data <- kc.potholes
    if (input$county != "All of Kansas City") {
      data <- data %>% filter(county == input$county)
    }
    data %>% filter(Open.Date.Time >= as.Date(input$dateRange[1]) & Open.Date.Time <= as.Date(input$dateRange[2]))
  })
  
  # Map output
  output$potholeMap <- renderPlotly({
    data <- filtered.data()
    p <- ggplot() +
      geom_sf(data = kcmo_counties, fill = "darkolivegreen3") +
      geom_point(data = data, aes(x = Longitude, y = Latitude, 
                                  text = paste("<b>Address:</b>\n", Incident.Address)),
                 color = "gray4", size = 0.1) +
      ggtitle(paste("Potholes in", input$county))
    ggplotly(p, tooltip = "text") %>% layout(hovermode = "closest")
  }) 
  
  # Calculate average response time
  output$avgResponseTime <- renderText({
    data <- filtered.data()
    avg_days <- mean(data$Days.to.resolve, na.rm = TRUE)
    paste(round(avg_days, 1), "days")
  })
  
  # Calculate average household income
  output$avgHIncome <- renderText({
    data <- filtered.data()
    avg_income <- mean(data$avg.h.income, na.rm = TRUE)
    paste("$", format(avg_income, big.mark = ","))
  })
  
  # Display number of potholes
  output$potholeCount <- renderText({
    data <- filtered.data()
    count <- nrow(data)
    paste(format(count, big.mark = ","), "potholes")
  })
  
  # Regression analysis output
  output$regressionPlot <- renderPlot({
    data <- filtered.data()
    # Have to add loop to check if there's enough data to run a regression
    if(nrow(data) > 1) { 
      model <- lm(Days.to.resolve ~ avg.h.income, data)
      r_squared <- summary(model)$r.squared
      plot <- ggplot(data, aes(x = avg.h.income, y = Days.to.resolve)) +
        geom_point(aes(color = county), alpha = 0.5) +
        geom_smooth(method = "lm", color = "black") +
        labs(x = "Average Household Income ($)",
             y = "Days to Resolve Pothole",
             color = "County") +
        theme_minimal()
      print(plot)
    } else {
      print(ggplot() + 
              labs(title = "More data is required to display regression",
                   x = "", y = "") +
              theme_minimal())
    }
  })
}




################### RUN APP ########################
shinyApp(ui = ui, server = server)

