# this is an example of a comment for assignment: 1from primary observations to R data frame.

# list your name and assignment number and date.
# give a brief introduction and description of your data set theme and purpose.

# the first step will be to develop your observation attributes into coherent vectors using correct R syntax. The following example can be adapted to the specifics or your own data set vectors.
sa <- c("122 2nd street", "157 3rd street", "87 4th street", "25 5th street")
advert <- c(TRUE, FALSE, TRUE, FALSE)
enclosed <- c(FALSE, TRUE, TRUE, FALSE)
name <- c("BofA", "Capital One", "Bob's Bakery", "Starbucks")
fee <- c(3.45, 4.00, 2.50, 5.00)

# The following example is a combination of the element vectors into an R data frame.
local_ATMS <- data.frame(name = name, address = sa, advertisement = advert, inside = enclosed, fee = fee)

# Print to the console the final R data frame.
print(local_ATMS)