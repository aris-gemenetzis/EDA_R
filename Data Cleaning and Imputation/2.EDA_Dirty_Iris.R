#read the file into R
library(dplyr)
data <- read.csv("https://raw.githubusercontent.com/edwindj/datacleaning/master/data/dirty_iris.csv")
head(data)
str(data)

#make sure that strings are not converted to factor
data %>% mutate_if(is.factor, as.character) -> data
str(data)

#calculate the number and percentage of observations that are complete
(complete_number <- sum(!is.na(data)))
(complete_percentage <- sum(!is.na(data))/prod(dim(data)))

#replace other special values with NA
is.special <- function(x){
  if (is.numeric(x)) !is.finite(x) else is.na(x)
}
is.na(data) <- sapply(data, is.special)
head(data)

#define these rules in a separate text file and read them into R using editfile 
library(editrules)
(E <- editfile("rules.txt")) #print the resulting constraint object

#determine how often each rule is broken (violatedEdits)
ve <- violatedEdits(E, data)
#summarize and plot the result
summary(ve)
plot(ve)

# 60% of data has no errors
(no_errors <- summary(ve)['rel'][1,1])

# records 35 & 43 have too long petals
(too_long_petals <- which(ve[, "num7"]))

#find outliers in sepal length using boxplot and boxplot.stats
boxplot(data$Petal.Length)
outliers <- boxplot.stats(data$Sepal.Length)$out
data[which(data$Petal.Length %in% outliers), ]

#set the outliers to NA
i<-match(outliers,data$Sepal.Length)
data[i, "Sepal.Length"] <- NA

#Q: what might have happened? 
#A: perhaps some of the values were measured in mm instead of cm

library(deducorrect)
# read the correction rules
(C <- correctionRules("corrections.txt"))

#replace non positive values from Petal.Width with NA using correctWithRules
cor <- correctWithRules(C, data)
(data <- cor$corrected)

#replace all erronous values with NA using localizeErrors
le <- localizeErrors(E, data)
data[le$adapt] <- NA


library(VIM)
#use kNN imputation (VIM) to impute all missing values
data %>% mutate_if(is.character, as.factor) -> data
data1 <- kNN(data)

# x : vector to be imputed
# last : value to use if last value of x is empty
seqImpute <- function(x,last){
  n <- length(x)
  x <- c(x,last)
  i <- is.na(x)
  while(any(i)){
    x[i] <- x[which(i) + 1]
    i <- is.na(x)
  }
  x[1:n]
}

#use sequential hotdeck imputation to impute Petal.Width by sorting the dataset on Species
data2 <- data[order(data$Species),]
data2$Petal.Width <- seqImpute(data2$Petal.Width, median(data2$Petal.Width, na.rm = TRUE))
head(data2)

#use sequential hotdeck imputation to impute Petal.Width by sorting the dataset on Species and and Sepal.Length
data3 <- data[order(data$Species, data$Sepal.Length),]
data3$Petal.Width <- seqImpute(data3$Petal.Width, median(data3$Petal.Width, na.rm = TRUE))
head(data3)

#comparing dataframes data2 & data3
compare <- data.frame(result2 = data2$Petal.Width, result3 = data3$Petal.Width)
head(compare)
