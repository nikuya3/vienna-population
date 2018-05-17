library(shiny)
library(tidykml)
library(ggplot2)
library(magrittr)


mappings <- read.csv("data/mappings.csv", header = FALSE)
to_name <- function(n) {
  n <- mappings[1][mappings[2] == n,]
  paste("BEZIRKSGRENZEOGD.", 8000 + n, sep = "")
}
from_name <- function(n) {
  num <- gsub("BEZIRKSGRENZEOGD.", "", n)
  num <- as.numeric(num)
  num <- num - 8000
  mappings[2][num,]
}
polys <- kml_polygons("data/bezirke.kml")

ui <- fluidPage(
   titlePanel("Correlation of population change between districts"),
   sidebarLayout(
      sidebarPanel(
         numericInput(inputId = "dist",
                      label = "District",
                      min = 1,
                      max = 23,
                      value = 1)
      ),
      mainPanel(
         plotOutput("corrsp")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$corrsp <- renderPlot({
     data <- read.csv2("data/vie_101.csv", skip = 1)
     # clean dataset
     data <- data[,!(names(data) %in% c("NUTS1", "NUTS2", "NUTS3", "SUB_DISTRICT_CODE"))]  # remove artificial identifiers
     data$REF_DATE <- as.character(data$REF_DATE)
     data$REF_DATE <- as.Date(data$REF_DATE, format = "%Y%m%d")
     data$DISTRICT_CODE <- as.factor((data$DISTRICT_CODE - 90000) / 100)  # convert District codes (90100) to categorical district numbers (1)
     # calculate correlation of pop totals per district
     corr <- function (n, m) {
       cor(data[data$DISTRICT_CODE == n,]$POP_TOTAL, data[data$DISTRICT_CODE == m,]$POP_TOTAL)
     }
     corrs <- sapply(1:23, function (n) { sapply(1:23, function (m) { corr(n, m) }) })
     polys[polys$name != to_name(input$dist),] %>%
       ggplot(aes(longitude, latitude, group = name, fill = corrs[input$dist,from_name(name)])) +
       geom_polygon(color = "white")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

