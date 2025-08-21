#lab_2 script for lab steps

#determines the current working directory
getwd()
#shows files and subdirectories within the main directory
list.files()
#shows contents of the scripts directory - assumption is that lab2.R is current loaded in the scripts subdirectory
list.files("scripts")
# show all contents and their file paths
> list.files("~/Desktop/lab_2", recursive = TRUE, include.dirs = TRUE, full.names = TRUE)
#import csv as data frame to Global Environment
arrestNYC <- read.csv("data/NYPD_Arrest_Data__Year_to_Date__20250818.csv") 
# report structure of the dataset
str(arrestNYC)
# report summary of variable types and profiles
summary(arrestNYC) 
# report aggregation count of dataset
dim(arrestNYC) 
# note any missing values
anyNA(arrestNYC)
# profile missing values
arrestNYC[!complete.cases(arrestNYC),]
# character to date type
arrestNYC$ARREST_DATE <- as.Date(arrestNYC$ARREST_DATE, format = "%m/%d/%Y")
# create a character column list
char_cols <- sapply(arrestNYC, is.character)
# transform the character column list to a factor dataframe
arrestNYC[char_cols] <- lapply(arrestNYC[char_cols], as.factor)
# save the dataframe to the results folder
save(arrestNYC, file = "results/arrestNYC.RData")
# load into session the new dataframe from the results folder
load("results/arrestNYC.RData")
# create histogram of latitude
hist(arrestNYC$Latitude,
     main = "Histogram of Latitude of Arrest Points",
     xlab = "Degree of Latitude",
     ylab = "Frequency",
     col = "black",
     border = "black")
# create histogram of longitude
hist(arrestNYC$Longitude,
     main = "Histogram of Longitude of Arrest Points",
     xlab = "Degree of Longitude",
     ylab = "Frequency",
     col = "black",
     border = "black")
# frequency table for unique arrests per borough
arrest_counts_boro <- table(arrestNYC$ARREST_BORO)
# frequency table for unique arrests per precinct
arrest_counts_precinct <- table(arrestNYC$ARREST_PRECINCT)
# bar plot for borough arrests
barplot(arrest_counts_boro,
        main = "Arrest in NYC by Arrest Borough (Jan-June 2025)",
        xlab = "Borough",
        ylab = "Count",
)
#bar plot for precinct arrests
barplot(arrest_counts_precinct,
        main = "Arrest in NYC by Arrest Precinct (Jan-June 2025)",
        xlab = "Borough",
        ylab = "Count",
        names.arg = names(arrest_counts_precinct),
        las = 2,
        cex.names = 0.5,
)
# create daily count table of arrests
daily_count <- table(arrestNYC$ARREST_DATE)
# plot the daily countsa line plot with smooth line
plot(daily_counts, type = "l",
     main = "NYC Arrest Trend (Jan-June 2025) ",
     xlab = "Beginning > End | Months", ylab = "Daily Arrest Totals")
lines(lowess(daily_counts, f = 0.2), col = "orange", lwd = 3)
#format the arrest dates to year-month format, removing the individual days
reformat_date <- format(arrestNYC$ARREST_DATE, "%Y-%m")
# create aggregation table for months
monthly_counts <- table(reformat_date)
# plot the monthly aggregation as bar plot
barplot(monthly_counts, 
        main = "NYC Arrest Trend (Jan-June 2025)", 
        xlab = "Month", 
        ylab = "Number of Arrests",
)
# check for missing values- latitude and longitude
sum(is.na(arrestNYC$Latitude)) 
sum(is.na(arrestNYC$Longitude)) 
# check for zero values
is_zero <- (arrestNYC$Latitude == 0)
sum(is_zero)
is_zero <- (arrestNYC$Longitude == 0)
sum(is_zero)
# create new dataset for mapping 
arrestNYC_mapping <- arrestNYC[, c("Latitude", "Longitude")]
# remove zeros from new dataset
arrestNYC_mapping_no_zero <- arrestNYC_mapping[arrestNYC_mapping$Longitude != 0,]
arrestNYC_mapping_no_zero <- arrestNYC_mapping[arrestNYC_mapping$Latitude != 0,]
# calculate extent of lat lon in arrest data
min_lon <- min(arrestNYC_mapping_no_zero$Longitude)
max_lon <- max(arrestNYC_mapping_no_zero$Longitude)
min_lat <- min(arrestNYC_mapping_no_zero$Latitude)
max_lat <- max(arrestNYC_mapping_no_zero$Latitude)
# pass the extents to the plot
plot(NULL, xlim = c(min_lon, max_lon), ylim = c(min_lat, max_lat),
     xlab = "Longitude", ylab = "Latitude",
     main = "Arrest Location Points in NYC (Jan-June 2025)")
# add points to the plot and style the points
points(arrestNYC_mapping_no_zero$Longitude, arrestNYC_mapping_no_zero$Latitude, pch = 3, cex = 0.08)



# for rmd
in console:
  
  save(arrestNYC, file = "results/arrestNYC.RData")

in r markdown:
  
  load("~/Desktop/lab_2/results/arrestNYC.RData")