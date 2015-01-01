###############################################################################
# Hubble Example
###############################################################################

# install.packages("ggplot2")
# library(ggplot2)

###############################################################################
# Input data
###############################################################################

object <- c("S.Mag.","L.Mag.","N.G.C. 6822","N.G.C. 598",
            "N.G.C. 221","N.G.C. 224","N.G.C. 5457","N.G.C. 4763",
            "N.G.C. 5194","N.G.C. 4449","N.G.C. 4214","N.G.C. 3031",
            "N.G.C. 3627","N.G.C. 4826","N.G.C. 5236","N.G.C. 1068",
            "N.G.C. 5055","N.G.C. 7331","N.G.C. 4258","N.G.C. 4151",
            "N.G.C. 4382","N.G.C. 4472","N.G.C. 4486","N.G.C. 4649")

distance <- c(0.032,0.034,0.214,0.263,0.275,0.275,0.45,0.5,
              0.5,0.63,0.8,0.9,0.9,0.9,0.9,1.0,1.1,1.1,
              1.4,1.7,2.0,2.0,2.0,2.0)

velocity <- c(170,290,-130,-70,-185,-220,200,290,270,
              200,300,-30,650,150,500,920,450,500,
              500,960,500,850,800,1090)

###############################################################################
# Plot with ggplot2
###############################################################################

# hubble <- data.frame(object,distance,velocity)
# 
# fit <- lm(hubble$distance~hubble$velocity)
# 
# hubble <- data.frame(hubble, predict(fit, interval = 'prediction'))
# 
# ggplot(hubble, aes(x = velocity, y = distance)) +
#   geom_smooth(method = 'lm', aes(fill = 'confidence'),alpha = 1.0) +
#   geom_point(colour = 'black', size = 3) +
#   labs(x ="Recession Velocity (km/sec)",
#        y ="Distance (megaparsecs)",
#        title ="Measured distance versus velocity 
#        for 24 extra-galactic nebulae") +
#   theme(legend.position = "none")

###############################################################################
# Plot data
###############################################################################

plotCI <- function (x, y, xlab, ylab, main) {
  plot(x, y, pch = 16, col = "dodgerblue4",
       xlab = xlab, ylab = ylab,
       main = main, cex.main = 0.8,
       cex.axis = 0.8, cex.lab = 0.9,
       las = 1, xlim = c(min(x), max(x)),
       ylim = c(min(y), max(y)))
  
  fit <- lm(y~x)
  summary(fit)

  abline(fit, col = "dodgerblue4")
  
  CI <- predict(fit, interval = "confidence")
  
  cint <- data.frame(x, lower = CI[,2], upper = CI[,3])
  cint <- cint[order(x),]
  with(cint, points(x, lower, type = "l",
                    col = "firebrick3", lwd = 2))
  with(cint, points(x, upper, type = "l",
                    col = "firebrick3", lwd = 2))
}

plotCI(x = velocity, y = distance,
       xlab = "Recession Velocity (km/sec)",
       ylab = "Distance (megaparsecs)",
       main = "Measured distance versus velocity")

###############################################################################
# Specify Model
###############################################################################

fit <- lm(distance ~ velocity)
summary(fit)

plot(fitted(fit), resid(fit),
     xlab = "Predicted Distances (Megaparsecs)",
     ylab = "Residuals",
     main = "Residuals by Fitted Values",
     las = 1, pch = 16, col = "dodgerblue4",
     xlim = c(0,2), ylim = c(-1,1))




###############################################################################
###############################################################################
##
## Regression background: least squares, residuals, diagnostics
##
###############################################################################
###############################################################################




###############################################################################
# LS animation for web
###############################################################################

install.packages("animation")
library(animation)

oopt = ani.options(interval = 0.3, nmax = ifelse(interactive(), 50, 2))
least.squares()

###############################################################################
# LS static for print
###############################################################################

set.seed(0)
x <- 1:15
y <- x + rnorm(15)

fit <- coef(lm(y ~ x))
a <- fit[1]
b <- fit[2]

par(mfrow = c(1,1))
ab.col <- c('gray', 'black')
est.pch <- 19
v.col <- 'red'
v.lty <- 2
v.lwd <- 2
rss.pch <- 19
rss.type <- 'o'

bseq = tan(seq(pi/10, 3.5 * pi/10, length = 15))
plot(x, y)
abline(fit, col = ab.col[1])
abline(a, bseq[6], col = ab.col[2])
points(x, bseq[6] * x + a, pch = est.pch)
segments(x, bseq[6] * x + a, x, y, col = v.col, lty = v.lty, lwd = v.lwd)

text(3, 11, "Residual difference", col="red")
text(3, 12.5, "Line of best fit", col="gray")
text(3, 14, "Possible line of fit", col="black", adj=NULL)

###############################################################################
# Explained variance
###############################################################################

set.seed(18)
x <- seq(from = 1, to = 15, length.out = 50)
y <- x + rlogis(50)^2

fit <- coef(lm(y ~ x))
a <- fit[1]
b <- fit[2]

par(mfrow = c(1,1))
ab.col <- c('gray', 'black')
est.pch <- 19
v.col <- 'red'
v.lty <- 2
v.lwd <- 2

plot(x, y, pch = 16)
abline(fit, col = ab.col[1])
segments(x, b * x + a, x, y, col = v.col, lty = v.lty, lwd = v.lwd)

text(2.5, 25, "R-squared = 0.39")

###############################################################################
# Residual
###############################################################################

x <- c(0.5, 1.4, 2.5, 3)
y <- c(0.8, 2, 2.3, 2.9)

plotResid <- function(i) {
  fit <- coef(lm(y ~ x))
  a <- fit[1]
  b <- fit[2]
  
  # Sets margins for the plot
  par(mar=c(2, 5, 2, 1) + 0.1)
  
  # Plots our data without any axis labels or tics
  plot(x, y, pch = 16, yaxt="n",
       xaxt="n", ylab="")
  
  # Plots line of best fit
  abline(fit)
  
  # Adds vertical line from y = 0 to y = y[i]
  if(y[i] < a+b*x[i]) {
    segments(x[i], 0, x[i], a+b*x[i], col = "grey",
             lwd = 2, lty = 1)
  } else {
    segments(x[i],0,x[i],y[i], col = "grey", lwd = 2, lty = 1)
  }
  
  # Add horizontal lines from x = 0 to x = x[i]
  # at y[i] and y-hat[i], respectively
  segments(0,y[i],x[i],y[i], col = "grey", lwd = 2, lty = 1)
  segments(0, a+b*x[i], x[i], a+b*x[i], col = "grey", lwd = 2, lty = 1)
  
  # Redraws data points so they are layered over the segments
  points(x, y, pch = 16)
  
  # Adds all text to the plot
  mtext("X", side = 1, line = 0.5, at = x[i],
        cex.lab = 1, las = 1, col = "grey")
  mtext("Y", side = 2, line = 0.5, at = y[i],
        cex.lab = 1, las = 2, col = "grey")
  mtext("Fitted Y", side = 2, line = 0.5, at = a+b*x[i],
        cex.lab = 1, las = 2, col = "grey")
  
  # Adds arrow between observed and predicted y[i]
  arrows(x[i]/2, a+b*x[i], x[i]/2, y[i], length = 0.125,
         angle = 30, code = 3, col = "black",
         lwd = 2, lty = 1)
  
  text(x = x[i]/1.5, y = y[i] + 0.5*((a+b*x[i])-y[i]),
       "Residual")
}

plotResid(2)

###############################################################################
# Sum of Squares
###############################################################################

x <- c(0.5, 1, 2.9, 3)
y <- c(0.8, 1.6, 2.3, 2.9)

fit <- coef(lm(y ~ x))
a <- fit[1]
b <- fit[2]

layout(matrix(c(1,2),1,2))

plot(x, y, pch = 16, main = "Simple Average of Y",
     xlim = c(0, 3.5), ylim = c(0, 3.5))
abline(h = mean(y), col = "black")
rect(x, y, x + (mean(y)-y), mean(y),
     col = "red", density = 55)
points(x,y, pch = 16)
text(1, 3.25, "Total Sum of Squares", col = "red", cex = 0.75)

plot(x,y, pch = 16, main = "Fitted Values of Y",
     xlim = c(0, 3.5), ylim = c(0, 3.5))
abline(fit, col = "black")
rect(x, y, x + ((a+b*x)-y), a+b*x,
     col = "blue", density = 55)
text(1.25, 3.25, "Residual Sum of Squares", col = "blue", cex = 0.75)
points(x,y, pch = 16)

###############################################################################
# Residual Plots
###############################################################################

residPlot <- function(x, y, main) {
  plot(x, y, yaxt="n",xaxt="n",
       ylab="", xlab = "",
       main = main, pch = 16,
       col = "dodgerblue3",
       cex.main = 1.2)
}

layout(matrix(c(1:6),2,3))
par(mar=c(0.2, 0.2, 4.1, 0.2))
set.seed(0)

# unbiased and homoscedastic
resid <- rnorm(100, mean = 0, sd = 1)
fitted <- rnorm(100, mean = 0, sd = 1)
residPlot(resid, fitted, main = "Unbiased and Homoscedastic")
rect(-2, -1.5, 2, 2, col = "lightsteelblue2", density = 50)

# unbiased and heteroscedastic
resid <- rnorm(100, mean = 4, sd = 1)
fitted <- resid*sort(rnorm(100, 0, 0.3))
residPlot(resid, fitted, main = "Unbiased and Heteroscedastic")
polygon(x = c(1, 6, 6), y = c(0, -3, 3),
        col = "lightsteelblue2", density = 50)

# biased and homoscedastic
resid <- rnorm(100, mean = 0, sd = 1)
fitted <- resid+resid*rnorm(100, mean = 0, sd = 0.3)
residPlot(resid, fitted, main = "Biased and Homoscedastic")
polygon(x = c(-2, -2, 2, 2),
        y = c(-2.5, -1.5, 3, 2),
        col = "lightsteelblue2", density = 50)

# biased and heteroscedastic
resid <- rnorm(100, mean = 4, sd = 1)
fitted <- resid+resid*sort(rnorm(100, 2, 0.4))
residPlot(resid, fitted, main = "Biased and Heteroscedastic")
polygon(x = c(2, 2, 6, 6),
        y = c(5, 6, 20, 15),
        col = "lightsteelblue2", density = 50)

# biased and homoscedastic
resid <- rnorm(100, mean = 0, sd = 1)
fitted <- -1*(resid^2+rnorm(100, mean = 0, sd = 1))
residPlot(resid, fitted, main = "Biased and Homoscedastic")
curve(-(x^2-2), -pi, pi, add = TRUE, lwd = 3, col = "lightsteelblue2")
curve(-(x^2+2), -pi, pi, add = TRUE, lwd = 3, col = "lightsteelblue2")

# biased and heteroscedastic
resid <- sort(rnorm(100, mean = 0, sd = 1))
fitted <- -1*(resid^2+rnorm(100, mean = 0, sd = 1.5))
residPlot(-1*resid, fitted, main = "Biased and Heteroscedastic")
curve(-((x+0.5)^2+3), -3, 3, add = TRUE, lwd = 3, col = "lightsteelblue2")
curve(-((x-0.5)^2-3), -3, 3, add = TRUE, lwd = 3, col = "lightsteelblue2")