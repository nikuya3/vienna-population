library(shiny)
library(maps)
library(mapproj)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Correlation of population change between districts"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         numericInput(inputId = "dist",
                      label = "District",
                      min = 1,
                      max = 23,
                      value = 1)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         textOutput("corrs")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$corrs <- renderText({
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
     corrs[input$dist,]
   })
   
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
     corrs[input$dist,]
     vienna.road <- get_map("Vienna", maptype = "roadmap")
     ggmap(vienna.road)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

