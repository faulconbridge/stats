###############################################################################
# One-Sample t-Test Example
###############################################################################

set.seed(0)
treeVolume <- c(rnorm(1000, mean = 36500, sd = 2000))
t.test(treeVolume, mu = 39000)

hist(treeVolume, freq = TRUE, density = 150, col = "dodgerblue4",
     border = "black", breaks = 15, 
     main = "Histogram of Douglas Fir Volumes",
     xlab = "Volume (cubic feet)", xlim = c(30000, 44000),
     ylim = c(0, 225))

segments(39000, 0, 39000, 225, lwd = 3,
         lty = 2)
text(41000, 190, "Population mean")
text(41000, 175, "volume")

segments(mean(treeVolume), 0, mean(treeVolume), 225,
         lwd = 3, lty = 2, col = "gray70")
text(33500, 190, "Sample mean volume", col = "gray35")

###############################################################################
# Paired-Samples Example
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

bloodPressures <- data.frame(Systolic = c(preTreat, postTreat),
                         Treatment = c(rep("Pre-Treatment", 1000), 
                                       rep("Post-Treatment", 1000)))

# Optional histogram using ggplot

# library(ggplot2)
# ggplot(bloodPressures, aes(x = Systolic, fill = Treatment)) +
#   geom_bar(binwidth = 3, colour = "black", position = "identity", alpha = 0.6) + 
#   xlim(117, 174) +
#   xlab("Systolic Blood Pressure (mmHg)") + 
#   ggtitle("Systolic Blood Pressure Before and After Treatment")

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
# Implementation in R
###############################################################################

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

# Paired-samples t-test
# where y1 & y2 are numeric
set.seed(2820)
preTreat <- c(rnorm(1000, mean = 145, sd = 9))
postTreat <- c(rnorm(1000, mean = 138, sd = 8))
t.test(preTreat, postTreat, paired=TRUE)

# One-sample t-test
set.seed(0)
treeVolume <- c(rnorm(75, mean = 36500, sd = 2000))
t.test(treeVolume, mu = 39000) # Ho: mu = 39000

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
mean <- c(mean(data2[data2$kinase=="NSV",]$PFU),
          mean(data2[data2$kinase=="RIO3",]$PFU),
          mean(data2[data2$kinase=="TGFa",]$PFU),
          mean(data2[data2$kinase=="SKAP1",]$PFU),
          mean(data2[data2$kinase=="EPHA1",]$PFU),
          mean(data2[data2$kinase=="CSNK2B",]$PFU),
          mean(data2[data2$kinase=="GUCY2D",]$PFU))

# And the standard error
se <- c(sd(data2[data2$kinase=="NSV",]$PFU)/sqrt(3),
        sd(data2[data2$kinase=="RIO3",]$PFU)/sqrt(3),
        sd(data2[data2$kinase=="TGFa",]$PFU)/sqrt(3),
        sd(data2[data2$kinase=="SKAP1",]$PFU)/sqrt(3),
        sd(data2[data2$kinase=="EPHA1",]$PFU)/sqrt(3),
        sd(data2[data2$kinase=="CSNK2B",]$PFU)/sqrt(3),
        sd(data2[data2$kinase=="GUCY2D",]$PFU)/sqrt(3))

# The par() function lets us change the margins
# so we can fit all of our data labels
par(mar = c(5, 6, 4, 1))

# Specify the max y-axis value
plotTop <- max(1e+9)

# Construct our barplot and specify some more
# graphical parameters
barCenters <- barplot(height = mean, names.arg=names,
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
segments(barCenters, mean-se, barCenters, mean+se, lwd=1.5)
arrows(barCenters, mean-se, barCenters, mean+se, lwd=1.5,
       angle=90, code=3, length = 0.05)