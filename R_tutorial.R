###############################################################################
###############################################################################
###
###                         Basic R Syntax and Usage
###
###############################################################################
###############################################################################


###############################################################################
# Vectors
###############################################################################

# Vectors are analogous to columns of
# data in Excel. The only catch is that
# a vector can only contain a SINGLE data
# type (i.e., only numbers, letters, or
# TRUE/FALSE)

# NOTE: We use the notation OBJECT <- DATA
# to tell R to assign data to a variable.
# This way, we can type in, for example,
# myNumbers and R will know what data
# we're talking about.

myNumbers <- c(seq(from=1, to=10, by=1))
moreNumbers <- c(1,2,3,4,5,6,7,8,9,10)

# a character vector:
characters <- c("one","two","three","four","five")

# a boolean (logical) vector:
boolean <- c(TRUE, FALSE, FALSE, FALSE, TRUE)


###############################################################################
# Matrices
###############################################################################

# You can think of a matrix in R as either
# (1) a single vector split up into multiple
# rows/columns or (2) multiple vectors of
# the same length piled up next to one
# another. It's roughly analogous to a
# spreadsheet. However, the one caveat is
# that, just like with vectors, all elements
# in a matrix must have the same type (i.e.,
# all numeric, all character, or all logical).
# There's no mixing and matching here.
# The basic syntax for creating a matrix is:

# myMatrix <- matrix(vector, nrow=r, ncol=c, byrow=FALSE, 
#      dimnames=list(c("rownames"), c("colnames"))

matrixVector <- c(21, 5.9, 147,
                  28, 5.7, 168,
                  18, 5.5, 126,
                  24, 6.1, 195)
myMatrix <- matrix(matrixVector, nrow=4,
                   ncol=3, byrow=TRUE,
                   dimnames=list(
                     c("Chris", "John",
                       "Amy", "Max"),
                     c("Age", "Height",
                       "Weight")))


###############################################################################
# Data frames
###############################################################################

# A data frame in R is a generalized instance
# of a matrix: this you can truly think of as
# a page of an Excel spreadsheet. Each column
# represents a vector of a single type; however,
# each column can be of a different data type.
# So, for instance, if we wanted to turn the
# matrix above into a data frame but also add
# in a column for hair color, we would do
# something like:

name <- c("Chris", "John", "Amy", "Max")
age <- c(21, 28, 18, 24)
height <- c(5.9, 5.7, 5.5, 6.1)
weight <- c(147, 168, 126, 195)
hair <- c("brown", "blonde", "red", "brown")

myData <- data.frame(name, age, height, weight, hair)
colnames(myData) <- c("name", "age", "height", "weight",
                      "hair")


###############################################################################
# Factors
###############################################################################

# The variable "drugCondition" contains 20 experimental
# trials and 20 control trials. Currently, these values
# are stored as characters. You can check this by running
# str(drugCondition). You should see "chr" indicating
# the elements are characters

drugCondition <- c(rep("experimental",20),
                   rep("control",20))

# We will now convert the elements to factors.
# If you run str(drugCondition) again, you should see:
# Factor w/ 2 levels "control","experimental"

drugCondition <- factor(drugCondition)


###############################################################################
###############################################################################
###
###                                 T-Tools
###
###############################################################################
###############################################################################


###############################################################################
# One-Sample t-Test
###############################################################################

# The average volume of a Douglas Fir
# is about 39,000 cubic feet. A logging
# company just received a shipment with
# a mean volume of 36,500 cubic feet
# (sd = 2,000 cubic feet). Are these
# trees significantly smaller than
# the average?

# This ensures that any pseudo-randomizer functions
# return the same result each time called from this
# point
set.seed(0)

# Generate a normal distribution with mean = 36500
# and sd = 2000 consisting of 1000 samples
treeVolume <- c(rnorm(1000, mean = 36500, sd = 2000))

# Conduct a one-sample t-test for equality of means
t.test(treeVolume, mu = 39000)

# Comstruct a frequency histogram from our data
hist(treeVolume, freq = TRUE, density = 150, col = "dodgerblue4",
     border = "black", breaks = 15, 
     main = "Histogram of Douglas Fir Volumes",
     xlab = "Volume (cubic feet)", xlim = c(30000, 44000),
     ylim = c(0, 225))

# Illustrate where the sample and population means
# lie relative to one another
segments(39000, 0, 39000, 225, lwd = 3,
         lty = 2)
text(41000, 190, "Population mean")
text(41000, 175, "volume")

segments(mean(treeVolume), 0, mean(treeVolume), 225,
         lwd = 3, lty = 2, col = "gray70")
text(33500, 190, "Sample mean volume", col = "gray35")



###############################################################################
# Paired-Samples
###############################################################################

# Let's say that we're measuring systolic
# blood pressures of patients before and
# after treatment with a drug meant to 
# reduce hypertension

set.seed(2820)

# We have 500 patients with a preTreat systolic
# bp of M = 145, SD = 11
preTreat <- c(rnorm(1000, mean = 145, sd = 9))

# And a postTreat systolic bp M = 130
# and SD = 15
postTreat <- c(rnorm(1000, mean = 138, sd = 8))

# Did the treatment reduce hypertension?
t.test(preTreat, postTreat, paired=TRUE)

# Convert our data to a data frame
bloodPressures <- data.frame(Systolic = c(preTreat, postTreat),
                             Treatment = c(rep("Pre-Treatment", 1000), 
                                           rep("Post-Treatment", 1000)))

# Graph the distributions of our data
# before and after treatment
x <- seq(from = 110, to = 174, by = 0.5)
y1 <- dnorm(x, mean = 145, sd = 9)
y2 <- dnorm(x, mean = 138, sd = 8)
plot(x, y1, type="l", lwd=2, col="red",
     main="Systolic Blood Pressure Before and After Treatment",
     xlab = "Systolic Blood Pressure (mmHg)",
     ylab = "Frequency", yaxt="n",
     xlim = c(110, 175), ylim = c(0, 0.05))
lines(x, y2)
polygon(c(110,x,175),c(0,y2,0), col="firebrick3", density = 100,
        border = "black")
polygon(c(117,x,175),c(0,y1,0), col="dodgerblue4", density = 100,
        border = "black")

ylab=c(seq(from=0, to=175, by=25))
y=c(seq(from=0, to=0.05, length.out = 8))
axis(2,at=y,labels=ylab)

text(x = 120, y = 0.045, "- Pre-Treatment BP", col = "dodgerblue4", cex = 0.9)
text(x = 120, y = 0.04, " - Post-Treatment BP", col = "firebrick3", cex = 0.9)
points(109, 0.0445, pch = 15, col = "dodgerblue4")
points(109, 0.0395, pch = 15, col = "firebrick3")



###############################################################################
# Equality of Variance
###############################################################################

# For Levene's Test:
library(car)
leveneTest(bloodPressures$Systolic, bloodPressures$Treatment)

# Or:
var.test(preTreat, postTreat)



###############################################################################
# Independent-groups t-tests
###############################################################################

# Let's look at annual healthcare spending
# in Cleveland, OH and New York, NY. Is there
# a significant difference in the healthcare
# spending patterns between the two cities?


# Independent 2-group t-test
# where y1 and y2 are numeric
set.seed(0)
ClevelandSpending <- rnorm(50, mean = 250, sd = 75)
NYSpending <- rnorm(50, mean = 300, sd = 80)
t.test(ClevelandSpending, NYSpending, var.equal=TRUE)

# Independent 2-group t-test
# where y1 is numeric and y2 is binary
spending <- c(ClevelandSpending, NYSpending)
city <- c(rep("Cleveland", 50), rep("New York", 50))
t.test(spending~city, var.equal=TRUE)

# Independent 2-group t-test
# Equal variances not assumed
t.test(ClevelandSpending, NYSpending, var.equal=FALSE)



########################################
# Plotting group means with error bars
########################################

# Our initial data with kinase name and
# plaque forming units per mL
data2 <- data.frame(kinase = c(rep("NSV",3),rep("RIO3",3),
                               rep("TGFa",3),rep("SKAP1",3),
                               rep("EPHA1",3),rep("CSNK2B",3),
                               rep("GUCY2D",3)),
                    vial = c(rep(seq(from = 1, to = 3, by = 1),7)),
                    PFU = c(400000000, 100000000, 200000000,
                            100000000, 100000000, 150000000,
                            300000000, 175000000, 300000000,
                            100000000, 100000000, 200000000,
                            250000000, 500000000, 200000000,
                            100000000, 200000000,  80000000,
                            100000000, 100000000, 400000000))

# Specify that the kinase targeted should be
# treated as a factor
data2$kinase <- factor(data2$kinase)

# How our bars will be labeled when we graph them
names <- c("NSV", "RIO3", "TGFa", "SKAP1", "EPHA1", "CSNK2B", "GUCY2D")

# Calculate the mean for each of our kinase knock-downs
mean <- aggregate(data2$PFU, list(Kinases = data2$kinase), FUN="mean")
mean <- mean[order(match(mean$Kinases, names)), ]

# And the standard error
se <- aggregate(data2$PFU, list(Kinases = data2$kinase), FUN=sd)
se$x <- se$x/sqrt(3)
se <- se[order(match(se$Kinases, names)), ]

# The par() function lets us change the margins
# so we can fit all of our data labels
par(mar = c(5, 6, 4, 1))

# Specify the max y-axis value
plotTop <- max(1e+9)

# Construct our barplot and specify some more
# graphical parameters
barCenters <- barplot(height = mean$x, names.arg=names,
                      col=c("darkslategray", "gray80", "black",
                            "gray40", "gray60", "white", "darkgray"),
                      space=0.25,
                      las=1, ylim=c(1e+06,plotTop), cex.names = 0.7,
                      main = "Rotaviral titers following LV transduction",
                      ylab = "", xlab = "", border = "black",
                      axes = TRUE, log = "y", yaxt="n")

# Specify the range and labels for our y-axis
y=c(1e+06,1e+07,1e+08,1e+09)
ylab=c("1x10^6","1x10^7","1x10^8","1x10^9")

# Finally, construct the axes!
axis(2, at=y, labels=ylab,
     cex.axis = 0.75, las = 1)

# Give the y-axis label
mtext("PFU / mL", side=2,
      line=4, cex.lab=2, las=0, col="black")

# Same for the x-axis
mtext("Lentiviral vector", side=1,
      line=3, cex.lab=2, las=1, col="black")

# Lastly, the error bars
segments(barCenters, mean$x-se$x, barCenters, mean$x+se$x, lwd=1.5)
arrows(barCenters, mean$x-se$x, barCenters, mean$x+se$x, lwd=1.5,
       angle=90, code=3, length = 0.05)


###############################################################################
###############################################################################
###
###                                 Correlation
###
###############################################################################
###############################################################################


###############################################################################
# Scatterplots
###############################################################################

# Download our data: US estimated populations from
# 1610 through 2010 by decade
download.file(
  "https://raw.githubusercontent.com/faulconbridge/appliedStats/master/LaTeX/part03/data/correlationData01.csv",
  "population.csv", "wget", extra="--no-check-certificate")

# Assign our data to a data frame
population <- read.csv("population.csv",header=TRUE)

# View it for the funsies
View(population)

# Construct a scatterplot of population (y-axis)
# by year (x-axis)
plot(population$Year, population$Est..US.Population/1000000,
     xlab="Year", ylab="Population (Millions)", main="U.S. Population by Year",
     pch = 16, col = "dodgerblue4", cex.main = 0.9)


###############################################################################
# Scatterplot with Categorical Indicators
###############################################################################

# Adjust graph margins
par(mar = c(4.1,4.1,4.1,2.1))

set.seed(0)

# Data frame of patient depression and anxiety
# diagnoses with clinical severity (from mild
# to severe)
diagnosis <- data.frame(diagnosis = c(rep("mild", 25),
                                      rep("moderate", 25),
                                      rep("severe", 25)),
                        depression = c(rnorm(25, 3, 0.4),
                                       rnorm(25, 5, 0.3),
                                       rnorm(25, 7, 0.5)),
                        anxiety = c(rnorm(25, 4, 1.2),
                                    rnorm(25, 6, 0.8),
                                    rnorm(25, 9, 0.6))
)

# Plot anxiety (y-axis) by depression (x-axis) and
# color code by clinical severity
with(diagnosis, plot(depression, anxiety, type = "n",
                     xlab = "", ylab = "Anxiety",
                     main = "Clinical Anxiety and Depression Scores",
                     cex.main = 0.9))
mtext("Depression", side = 1, line = 2)

# Color mild severity
with(diagnosis[diagnosis$diagnosis=="mild",], 
     points(depression, anxiety, pch = 15,
            col = "dodgerblue", )
)

# Color moderate severity
with(diagnosis[diagnosis$diagnosis=="moderate",], 
     points(depression, anxiety, pch = 16,
            col = "dodgerblue2", )
)

# Color severe
with(diagnosis[diagnosis$diagnosis=="severe",], 
     points(depression, anxiety, pch = 17,
            col = "dodgerblue4", )
)

# Add legend for color coding
text(x = 7, y = 5.5, "Mild bipolar", col = "dodgerblue")
text(x = 7, y = 4.25, "Moderate bipolar", col = "dodgerblue2")
text(x = 7, y = 3, "Severe bipolar", col = "dodgerblue4")


###############################################################################
# Correlation 
###############################################################################

download.file(
  "https://raw.githubusercontent.com/faulconbridge/appliedStats/master/LaTeX/part03/data/correlationData03.csv",
  "adiposity.csv", "wget", extra="--no-check-certificate")

bodyFat <- read.csv("adiposity.csv", header=TRUE)

# Test correlation between neck and chest circumferences
cor.test(bodyFat$Neck, bodyFat$Chest)


###############################################################################
# Correlation Matrix 
###############################################################################

# Treat the matrix of cross-correlations
# as a distance matrix
as.dist(cor(bodyFat))

# load the required packages
install.packages("Hmisc")
library(Hmisc)

# Compute matrix of cross-correlations
rcorr(as.matrix(bodyFat))

# Graph bivariate correlations as an
# upper triangular matrix for the
# specified variables
pairs(~Neck+Chest+Abdomen+Hip+Thigh, data=bodyFat, upper.panel=NULL)


###############################################################################
# Partial Correlation 
###############################################################################

# There aren't any official R packages for
# partial correlation, so we'll use a function
# from Soojin Yi (http://www.yilab.gatech.edu/)
source("https://raw.githubusercontent.com/faulconbridge/stats/master/scripts/pcor.r")

# Compute partial correlation between
# neck and chest circumferences, controlling
# for every other variable in the dataset
neck <- bodyFat$Neck
chest <- bodyFat$Chest
others <- subset(bodyFat, select=c(Abdomen:Thigh))

pcor.test(neck,chest,others)


###############################################################################
# Linearity 
###############################################################################

# All of the four graphs output have
# the SAME correlation coefficient.
# Obviously, this is more meaningful
# for some of the datasets than
# others. Correlations are only
# meaningful if your data are
# approximately linear

layout(matrix(c(1,2,3,4),2,2))
par(mar=c(5, 5, 1, 5) + 0.1)

ff <- y ~ x
for(i in 1:4) {
  ff[[2]] <- as.name(paste("y", i, sep=""))
  ff[[3]] <- as.name(paste("x", i, sep=""))
  lmi <- lm(ff, data= anscombe)
  xl <- substitute(expression(x[i]), list(i=i))  
  yl <- substitute(expression(y[i]), list(i=i))
  plot(ff, data=anscombe, col="black", pch=21, cex=1, bg = "firebrick2", 
       xlim=c(3,19), ylim=c(3,13), 
       xlab="x", ylab="y"
  )  
  abline(lmi, col="firebrick")
}

###############################################################################
# Truncated Distributions 
###############################################################################

# If you are working with a bounded
# subset of the full data that doesn't
# represent the entire distribution
# of the data, your correlation
# can become highly skewed and
# largely uninterpretable

layout(matrix(1),1,1)
set.seed(0)

r <- 0.9
X <- rnorm(100)
Y = r*X + sqrt(1-r^2)*rnorm(100)
XY <- cbind(X,Y)
subXY <- as.data.frame(subset(XY, X>0 & X<1, select=c(X:Y)))

cor.test(X,Y)
cor.test(subXY$X, subXY$Y)

plot(X,Y, col="orange", pch=16,
     main="Effects of Distribution on Correlation")
rect(0,-2,1,3, col="grey", density=25)
points(subXY$X,subXY$Y, col="blue", pch=16)
text(-2, 2.5, "r = 0.897", col="orange")
text(-2, 2.0, "r = 0.387", col="blue")


###############################################################################
###############################################################################
##
##                         Simple Linear Regression
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
# Residuals
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

# Typical patterns for biased
# and unbiased, heteroscedastic and
# homoscedastic residuals. Heteroscedasticity
# means that not all observations in
# a sample or population have the same
# variance and usually indicates that the
# data should be transformed. A residual
# plot is biased if the fitted values at
# any given value of the residual do not
# average out to be 0 and generally
# indicates that you have a poorly
# fitted model.

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


###############################################################################
# Simple Linear Regression
###############################################################################

# This example uses data from Hubble, E. (1929).
# ``A relation between distance and radial
# velocity among extra-galactic nebulae.''
# Proceedings of the National Academy of
# Sciences of the United States of America,
# 15, 168-173.
#
#
# These data are what were used to lend
# support to the Big Bang theory.
# Isn't that kinda nifty?
#
# The idea is that if a galaxy were moving in
# approximately the same direction as ours,
# then (1) it would be moving faster than ours;
# (2) ours would be moving faster than it;
# or (3) they would be moving at approximately
# the same speeds. In either case 1 or 2, this
# would result in the two galaxies moving further
# apart. In the third case, as long as the two
# are not moving in exactly the same direction
# (i.e., θ != 0 where θ is the interior angle
# between the trajectories of the two galaxies),
# we will also continue to move farther apart
# (but at a slower rate). And of course if the
# galaxies are moving in opposite directions,
# their recession velocity will be greater still.
#
# The moral here? If the Big Bang is true, the
# farther apart two galaxies are, the faster
# they are moving away from one another.

# Body name
object <- c("S.Mag.","L.Mag.","N.G.C. 6822","N.G.C. 598",
            "N.G.C. 221","N.G.C. 224","N.G.C. 5457","N.G.C. 4763",
            "N.G.C. 5194","N.G.C. 4449","N.G.C. 4214","N.G.C. 3031",
            "N.G.C. 3627","N.G.C. 4826","N.G.C. 5236","N.G.C. 1068",
            "N.G.C. 5055","N.G.C. 7331","N.G.C. 4258","N.G.C. 4151",
            "N.G.C. 4382","N.G.C. 4472","N.G.C. 4486","N.G.C. 4649")

# Distance from Earth in megaparsecs
distance <- c(0.032,0.034,0.214,0.263,0.275,0.275,0.45,0.5,
              0.5,0.63,0.8,0.9,0.9,0.9,0.9,1.0,1.1,1.1,
              1.4,1.7,2.0,2.0,2.0,2.0)

# Recession velocity from Earth in Km/sec
velocity <- c(170,290,-130,-70,-185,-220,200,290,270,
              200,300,-30,650,150,500,920,450,500,
              500,960,500,850,800,1090)

########################################
# Plot with ggplot2
########################################

library(ggplot2)
hubble <- data.frame(object,distance,velocity)

fit <- lm(hubble$distance~hubble$velocity)

hubble <- data.frame(hubble, predict(fit, interval = 'prediction'))

ggplot(hubble, aes(x = velocity, y = distance)) +
  geom_smooth(method = 'lm', aes(fill = 'confidence'), alpha = 1.0) +
  geom_point(colour = 'black', size = 3) +
  labs(x ="Recession Velocity (km/sec)",
       y ="Distance (megaparsecs)",
       title ="Measured distance versus velocity
       for 24 extra-galactic nebulae") +
  theme(legend.position = "none")

########################################
# Plot data with built-in functions
########################################

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

########################################
# Specify Model
########################################

fit <- lm(distance ~ velocity)

# Look at the results of our model
summary(fit)

# Some basic residual diagnostics
plot(fitted(fit), resid(fit),
     xlab = "Predicted Distances (Megaparsecs)",
     ylab = "Residuals",
     main = "Residuals by Fitted Values",
     las = 1, pch = 16, col = "dodgerblue4",
     xlim = c(0,2), ylim = c(-1,1))


###############################################################################
###############################################################################
###
###                            ANOVAs
###
###############################################################################
###############################################################################


###############################################################################
# One-Way ANOVA
###############################################################################

# Download our data file
download.file("https://raw.githubusercontent.com/faulconbridge/stats/master/data/ex0525.csv",
              "educationData.csv", "wget", extra="--no-check-certificate")

# Read data and view it
educationData <- read.csv("educationData.csv", header = TRUE)
View(educationData)

# Convert level of education to a factor
educationData$Educ <- as.factor(educationData$Educ)
str(educationData$Educ)

# Specify our ANOVA model. Syntax is
# aov(y ~ x, data = MYDATA)
# View results using the summary() function
model1 <- aov(Income2005 ~ Educ, data=educationData)
summary(model1)

# Conduct pairwise tests for significance
# using Bonferroni correction
pairwise <- with(educationData, 
                 pairwise.t.test(x = Income2005, g = Educ,
                                 p.adjust.method = "bonferroni",
                                 alternative = "two.sided"))
pairwise

# Plot our groupwise means
# First construct a vector of means
means <- aggregate(educationData$Income2005,
                   by=list(educationData$Educ),
                   FUN="mean")

# Name the colums something useful
colnames(means) <- c("Education","Income")

# This will change the order in which
# our variables appear when we graph them
means <- means[order(match(means$Education, c("<12","12","13-15","16",">16"))), ]

# Specify a vector of colors for bar graph shading
colors <- c("royalblue1", "royalblue2", "royalblue", "royalblue3", "royalblue4")

# Construct our bar graph and specify axis and main titles
barplot(means$Income, names.arg = means$Education,
        xlab = "Years of Education", ylab = "2005 Income (USD)",
        main = "Income by Education", col = colors)

###############################################################################
# Two-Way ANOVA (Between-measures)
###############################################################################

# Download our data file
download.file("https://raw.githubusercontent.com/faulconbridge/stats/master/data/ex1319.csv",
              "IQ.csv", "wget", extra="--no-check-certificate")

# Read data and view it
IQ <- read.csv("IQ.csv", header = TRUE)
View(IQ)

# Convert Adoptive and Biological to factors
IQ <- within(IQ, {
  Adoptive <- factor(Adoptive)
  Biological <- factor(Biological)
})

# Specify a model without testing for interactions
model2 <- aov(IQ ~ Adoptive + Biological, data = IQ)
summary(model2)

# Specify a model testing for interactions
model3 <- aov(IQ ~ Adoptive * Biological, data = IQ)
summary(model3)

# Construct a boxplot of child IQ by, respectively,
# biological and adoptive parent IQ
par(mfrow=c(1,2))
plot(IQ ~ Adoptive + Biological, data=IQ)

# Construct an interaction plot
par(mfrow=c(1,1))
with(IQ, interaction.plot(Adoptive, Biological, IQ))

# Compute groupwise means for a barplot
means <- aggregate(IQ$IQ,
                   by=list(IQ$Adoptive, IQ$Biological),
                   FUN="mean")
colnames(means) <- c("Adoptive", "Biological", "IQ")

# Convert to matrix representation for ease of graphing
means <- matrix(data = means $IQ, nrow=2, ncol=2,
                dimnames = list(c("High Biological","Low Biological"),
                                c("High Adoptive","Low Adoptive")))

# Construct bar plot
par(mar =  c(5, 4, 4, 2))
barplot(means, beside=TRUE, xlab = "Parent IQs",
        ylab = "Child's IQ", ylim = c(0, 120),
        main = "Child by Biological\n and Adoptive Parent IQ",
        legend = rownames(means),
        args.legend = list(x = ncol(means) + 4,
                           y = ncol(means) + 150))

###############################################################################
# Two-Way ANOVA (Repeated-measures)
###############################################################################

# Download our data
download.file("https://raw.githubusercontent.com/faulconbridge/stats/master/data/satiation.csv",
              "satiation.csv", "wget", extra="--no-check-certificate")

# Attach as a data frame
satiation <- read.csv("satiation.csv", header = TRUE)

# Omit null response values
satiation <- na.omit(satiation)

# Omit irrelevant values (filler items)
satiation <- satiation[!satiation$BIAS=="filler",]

# Specify factors
satiation <- within(satiation, {
  BIAS <- factor(BIAS)
  REPS <- factor(REPS)
  RELATEDNESS <- factor(RELATEDNESS)
  PID <- factor(PID)
})

# Compute groupwise means
satiationMeans <- aggregate(satiation$MS,
                            by = list(satiation$PID, satiation$BIAS,
                                      satiation$REPS, satiation$RELATEDNESS),
                            FUN = "mean")

# Name the columns usefully
colnames(satiationMeans) <- c("PID", "BIAS", "REPS", "RELATEDNESS", "MS")
View(satiationMeans)

# Specify our model. Each within-terms factor
# must also be included in the error term
# as seen below
model4 <- aov(MS ~ REPS * RELATEDNESS * BIAS
              + Error(PID / (REPS * RELATEDNESS * BIAS)),
              data = satiationMeans)
summary(model4)

# Construct interaction plots for our data
par(mfrow=c(3,2))
par(mar = c(2,4,2,5))
with(satiation, interaction.plot(REPS, RELATEDNESS, MS))
with(satiation, interaction.plot(RELATEDNESS, REPS, MS))
with(satiation, interaction.plot(BIAS, REPS, MS))
with(satiation, interaction.plot(REPS, BIAS, MS))
with(satiation, interaction.plot(RELATEDNESS, BIAS, MS))
with(satiation, interaction.plot(BIAS, RELATEDNESS, MS))