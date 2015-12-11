
# set working directory (or comment this and use the menu: Session > Set Working Directory > To Source File Location)
setwd("~/Documents/Projects/AP_Repex Work")

# import source data (2nd argument forces datatypes to character string)
data <- read.csv("LSTD_All_Current_With_LatLong_T1.csv", colClasses = "character")

# create new install date column using R date objects (for use with > and < operators)
data$RInstall_Date <- as.POSIXct(as.Date(data$DATE_INSTALLED, '%d/%m/%Y'))

# bin dates and crosstab
cuts <- seq(from=as.POSIXct("1950-01-01 00:00"), by = "10 years", to=as.POSIXct("2020-01-01 00:00"))
data$RDecade <- cut(data$RInstall_Date, cuts, right = TRUE)
table(data$STOCK_CODE,data$RDecade)
