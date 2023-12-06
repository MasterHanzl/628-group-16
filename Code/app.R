rm(list=ls())

library(shiny)
library(leaflet)
library(dplyr)
# Read the CSV file
data <- read.csv("coffee_review_merge.csv")
# Sample data frame based on the screenshot provided
df <- data.frame(
  name = data$name,
  city = data$city,
  state = data$state,
  postal = data$postal_code,
  lati = data$latitude,
  long = data$longitude,
  stars_y = data$stars_y,
  review = data$text,
  stringsAsFactors = FALSE
)

# Define the UI
ui <- fluidPage(
  titlePanel("Restaurant Reviews and Location"),
  sidebarLayout(
    sidebarPanel(
      textInput("nameInput", "Enter Restaurant Name:"),
      textInput("cityInput", "Enter City:"),
      textInput("stateInput", "Enter State:"),
      textInput("postalInput", "Enter Postal Code:"),
      actionButton("searchButton", "Search"),
      # Add an image below the input elements
      tags$br(),  # Add a line break for spacing
      img(src = "coffee2.webp", height = "300px", width = "auto")  # Adjust height and width as needed
    ),
    mainPanel(
      leafletOutput("map", height = 200),
      h3("High Star Reviews"),
      htmlOutput("highStarReview1"),
      htmlOutput("highStarReview2"),
      htmlOutput("highStarReview3"),
      h3("Low Star Reviews"),
      htmlOutput("lowStarReview1"),
      htmlOutput("lowStarReview2"),
      htmlOutput("lowStarReview3")
    )
  )
)


# Define the server logic
server <- function(input, output) {
  
  # Event to trigger the search and re-search
  search_results <- eventReactive(input$searchButton, {
    isolate({
      # Filter data based on input
      filtered_data <- df %>%
        filter(
          tolower(name) == tolower(input$nameInput),
          tolower(city) == tolower(input$cityInput),
          tolower(state) == tolower(input$stateInput),
          postal == input$postalInput
        )
      
      # Shuffle and separate high and low star reviews
      high_stars <- filtered_data %>% filter(stars_y >= 4) %>% sample_frac(1) %>% head(3)
      low_stars <- filtered_data %>% filter(stars_y <= 2) %>% sample_frac(1) %>% head(3)
      
      list(high_stars = high_stars, low_stars = low_stars)
    })
  }, ignoreNULL = FALSE)
  
  # Output high star reviews
  for (i in 1:3) {
    local({
      ii <- i
      output[[paste0("highStarReview", ii)]] <- renderUI({
        res <- search_results()
        if (nrow(res$high_stars) >= ii) {
          HTML(paste(res$high_stars$review[ii], "<br><br>"))
        } else {
          HTML("None")
        }
      })
    })
  }
  
  # Output low star reviews
  for (i in 1:3) {
    local({
      ii <- i
      output[[paste0("lowStarReview", ii)]] <- renderUI({
        res <- search_results()
        if (nrow(res$low_stars) >= ii) {
          HTML(paste(res$low_stars$review[ii], "<br><br>"))
        } else {
          HTML("None")
        }
      })
    })
  }
  
  # Update the map
  output$map <- renderLeaflet({
    res <- search_results()
    if (nrow(res$high_stars) > 0) {
      location <- res$high_stars[1,] # Assuming all entries have the same lat-long
      leaflet() %>%
        addTiles() %>%
        setView(lng = location$long, lat = location$lati, zoom = 15) %>%
        addMarkers(lng = location$long, lat = location$lati, popup = location$name)
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)