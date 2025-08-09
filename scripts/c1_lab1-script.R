# Class 1 - Lab 1 Script

# determine current version of R and the OS in which it operates
R.version
# determine the current working directory
getwd()
# create x variable
x <- 5
# create y variable
y <- 3
# add x + y
x + y
# alternately, we can use the sum function
sum(10,5,7.5)
sum(x,y)
# help option for the function sum
?sum()
# create an atomic vector
NYC_unemp <- c(5.6, 5.6, 5.6, 5.5, 5.3, 5.1, 5.0, 4.8, 4.7)
# mean value
mean(NYC_unemp)
# apply NA
NYC_unemp <- c(5.6, 5.6, 5.6, 5.5, 5.3, 5.1, 5.0, 4.8, 4.7, NA, NA, NA)
# check length of vector
length(NYC_unemp) 
# remove NA; recheck length
mean(NYC_unemp, na.rm = TRUE)
# random list creation
random_list <- list(1, "a", TRUE, c(1,2,3))
# create a character vector
grades <- c("A", "B", "C", "D", "F", "Exceptional" )
# print the vector
print(grades)
# transform vector to factor
NYC_unemp_grade_factor <- factor(grades)
# print factor
print(NYC_unemp_grade_factor)
# create vectors for data frame creation
NYC_unemp_rate <- c(5.6, 5.6, 5.6, 5.5, 5.3, 5.1, 5.0, 4.8, 4.7, NA, NA, NA)
NYC_unemp_grade <- c("D", "D", "D", "C", "C", "C", "B", "A", "Exceptional", "Not Available", "Not Available", "Not Available")
NYC_unemp_month <- c("october-24","november-24","december-24","january-25","february-25","march-25","april-25","may-25","june-25","july-25","august-25","september-25")
# create data frame
NYC_unemp_dframe <- data.frame(month = NYC_unemp_month, rate = NYC_unemp_rate, score = NYC_unemp_grade)
# print the data frame
print(NYC_unemp_dframe)




















