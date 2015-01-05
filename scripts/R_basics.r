########################################
# Basic R Syntax and Usage
########################################

####################
# Constants
####################

# Constants are just what they sound
# like: things. They don't do anything;
# they're just static objects. They're
# basically just shortcuts to refer
# to larger sets of data. We assign
# them as:

myConstant <- c(1, 2, 3, 4, 5, 6)


####################
# Vectors
####################

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


####################
# Matrices
####################

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


####################
# Data frames
####################

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


####################
# Factors
####################

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
