#################################################
# Sunspot example with ARIMA and time series
#################################################

library(forecast)

download.file(
  "https://...",
  "ex0323.csv", method = "wget", extra = "--no-check-certificate")
ex0323 <- read.csv("ex0323.csv", header = TRUE)
View(ex0323)
attach(ex0323)

# Let's start by plotting the data and separating things
# out by sunspot activities
plot(Year, CancerRate,
     xlab = "Year", ylab = "Cancer Rate",
     main = "Cancer Rates by Year and Sunspot Activity",
     type = "n", xlim = c(1940, 1985), ylim = c(0, 6))
with(ex0323[ex0323$SunspotActivity == "Low",],
     points(Year, CancerRate,
            col = "dodgerblue4", pch = 16,
            type = "p"))
with(ex0323[ex0323$SunspotActivity == "High",],
     points(Year, CancerRate,
            col = "firebrick3", pch = 16,
            type = "p"))
text(x = 1943, y = 5.5, "Key:",
     col = "black")
text(x = 1943, y = 5, "Low activity",
     col = "dodgerblue4")
text(x = 1943, y = 4.5, "High activity",
     col = "firebrick3")

# Fit a linear model using only
# sunspot activity
fit <- lm(CancerRate ~ SunspotActivity, data=ex0323)
summary(fit)

# Adding in year as a predictor
fit2 <- lm(CancerRate ~ Year, data = ex0323)
summary(fit2)
abline(fit2, col = "gray",
       lwd = 2)

# And a model for both explanatory variables
fit3 <- lm(CancerRate ~ Year + SunspotActivity, data = ex0323)
summary(fit3)

# Low sunspot activity
abline(a = -212.3748, b = 0.1100280,
       col = "firebrick3", lwd = 2)

# High sunspot activity
abline(a = -211.9314286, b = 0.1100280,
       col = "dodgerblue4", lwd = 2)

# Converting to a time series
timeSeries <- ts(CancerRate, frequency = 1, 
                 start = min(Year), end = max(Year))

# Auto-fitting to an ARIMA model
fit.ts <- auto.arima(timeSeries)
summary(fit.ts)
forecasted = forecast(fit.ts)

# The forecasted points
points(c(1973:1982), forecasted$mean@.Data, col = "black", pch = 16)

# Conversely, the fitted lm predicted points
points(x = c(1973:1982), y = -212.184762+0.110028*c(1973:1982),
       col = "black", pch = 2)
