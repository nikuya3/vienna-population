library(corrplot)

data <- read.csv2("data/vie_101.csv", skip = 1)

# clean dataset
data <- data[,!(names(data) %in% c("NUTS1", "NUTS2", "NUTS3", "SUB_DISTRICT_CODE"))]  # remove artificial identifiers
data$REF_DATE <- as.character(data$REF_DATE)
data$REF_DATE <- as.Date(data$REF_DATE, format = "%Y%m%d")
data$DISTRICT_CODE <- as.factor((data$DISTRICT_CODE - 90000) / 100)  # convert District codes (90100) to categorical district numbers (1)

# plot population trend for each district
plot.pop <- function(district) {
  data <- data[data$DISTRICT_CODE == district,]
  title <- paste("Population in ", district, ". district per year", sep = "")
  plot(POP_TOTAL ~ REF_DATE, data, main = title, xlab = "Date", ylab = "Population")
  axis.Date(1, at = c(seq(min(data$REF_DATE), max(data$REF_DATE), by = "50 years"), max(data$REF_DATE)))
}

plot.pop(1)
sapply(data$DISTRICT_CODE, plot.pop)

# regress trend lines
model <- lm(POP_TOTAL ~ DISTRICT_CODE + REF_DATE, data)
summary(model)

model <- lm(POP_TOTAL ~ poly(REF_DATE, 6), data[data$DISTRICT_CODE == 23,])
pred <- predict(model)
plot.pop(23)
lines(data[data$DISTRICT_CODE == 23,]$REF_DATE, pred)

# calculate correlation of pop totals per district
corr <- function (n, m) {
  cor(data[data$DISTRICT_CODE == n,]$POP_TOTAL, data[data$DISTRICT_CODE == m,]$POP_TOTAL)
}
corrs <- sapply(1:23, function (n) { sapply(1:23, function (m) { corr(n, m) }) })
corrplot(corrs, type = "upper")
