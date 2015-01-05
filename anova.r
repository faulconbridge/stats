###############################################################################
# One-Way ANOVA
###############################################################################

# Download our data file
download.file("https://raw.githubusercontent.com/faulconbridge/stats/master/ex0525.csv",
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
download.file("https://raw.githubusercontent.com/faulconbridge/stats/master/ex1319.csv",
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