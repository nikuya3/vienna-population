library(shiny)
library(ggplot2)
library(tidykml)
library(magrittr)
library(sp)

mappings <- read.csv("data/mappings.csv", header = FALSE)
polys <- kml_polygons("data/bezirke.kml")
data <- read.csv2("data/vie_101.csv", skip = 1)
# clean dataset
data <-
  data[, !(names(data) %in% c("NUTS1", "NUTS2", "NUTS3", "SUB_DISTRICT_CODE"))]  # remove artificial identifiers
data$REF_DATE <- as.character(data$REF_DATE)
data$REF_DATE <- as.Date(data$REF_DATE, format = "%Y%m%d")
data$DISTRICT_CODE <-
  as.factor((data$DISTRICT_CODE - 90000) / 100)  # convert District codes (90100) to categorical district numbers (1)

to_name <- function(n) {
  n <- mappings[1][mappings[2] == n, ]
  paste("BEZIRKSGRENZEOGD.", 8000 + n, sep = "")
}

from_name <- function(n) {
  num <- gsub("BEZIRKSGRENZEOGD.", "", n)
  num <- as.numeric(num)
  num <- num - 8000
  mappings[2][num, ]
}

corr <- function (n, m) {
  cor(data[data$DISTRICT_CODE == n, ]$POP_TOTAL, data[data$DISTRICT_CODE == m, ]$POP_TOTAL)
}

ui <- fluidPage(
  fluidRow(
    column(2),
    column(8, h1("Correlation of population change between districts (1869--2015)")),
    column(2)
  ),
  fluidRow(
    column(3),
    column(6,
           plotOutput("corrsp", click = "dist_click")
    ),
    column(3)
  ),
  fluidRow(
    column(2),
    column(8,
           wellPanel(
             p("Click on district to display correlations"),
             a("Source: https://www.data.gv.at/katalog/dataset/091a085f-2652-429f-8dde-c69199440ddf (Stadt Wien)", href = "https://www.data.gv.at/katalog/dataset/091a085f-2652-429f-8dde-c69199440ddf")
           )
    ),
    column(2)
  )
)

server <- function(input, output) {
  dist <- 1
  makeReactiveBinding('dist')
  output$corrsp <- renderPlot({
    # calculate correlation of pop totals per district
    corrs <- sapply(1:23, function (m) {
      corr(dist, m)
    })
    corrs[dist] <- NA
    polys %>%
      ggplot(aes(longitude, latitude, group = name, fill = corrs[from_name(name)])) +
      geom_polygon(color = "black") +
      theme_void() +
      guides(fill = guide_legend(paste(
        "Correlation to district", formatC(dist, width = 2, flag = "0")
      ), alpha = FALSE)) +
      scale_fill_gradient2(
        low = "#822a4d",
        mid = "white",
        high = "#2a5486",
        breaks = c(-1, -0.5, 0, 0.5, 1),
        limits = c(-1, 1)
      )
  })

  observeEvent(input$dist_click, {
    sad <- lapply(unique(polys$name), function (b) {
      poly.x <- polys[polys$name == b, ]$longitude
      poly.y <- polys[polys$name == b, ]$latitude
      if (point.in.polygon(input$dist_click$x, input$dist_click$y, poly.x, poly.y)) {
        from_name(b)
      }
      else {
        0
      }
    })
    if (length(unique(sad)) > 1) {
      dist <<- as.numeric(sad[which(sad != 0)])
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
