#lab_2 script for lab steps

#determines the current working directory
getwd()
#shows files and subdirectories within the main directory
list.files()
#shows contents of the scripts directory - assumption is that lab2.R is current loaded in the scripts subdirectory
list.files("scripts")
# show all contents and their file paths
> list.files("~/Desktop/lab_2", recursive = TRUE, full.names = TRUE)
#import csv as data frame to Global Environment
arrestNYC <- read.csv("data/NYPD_Arrest_Data__Year_to_Date__20250817.csv") 




#save data to results following analysis as a data frame
save(arrestNYC, file = "results/arrestNYC.RData")

# for rmd
in console:
  
  save(arrestNYC, file = "results/arrestNYC.RData")

in r markdown:
  
  load("~/Desktop/lab_2/results/arrestNYC.RData")