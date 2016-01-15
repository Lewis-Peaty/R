# set working directory (or comment this and use the menu: Session > Set Working Directory > To Source File Location)
setwd("C:\\Users\\n043341\\Documents\\Documents\\Projects\\AP_Info Packs\\Dx Streetlights\\Stock_Code_Investigation")

# import source data (2nd argument forces datatypes to character string)
data <- read.csv("Stock_Code_Dates_Revised.csv", colClasses = "character")
 
# create new install date column using R date objects (for use with > and < operators)
data$RInstall_Date <- as.POSIXct(as.Date(data$INSTALLED_DATE, '%d/%m/%Y'))
data$RRevised_Install_Date <- as.POSIXct(as.Date(data$Revised_Installed_Date, '%d/%m/%Y'))

# bin dates and crosstab
cuts <- seq(from=as.POSIXct("1900-01-01 00:00"), by = "1 years", to=as.POSIXct("2017-01-01 00:00"))
data$RInstallYear <- cut(data$RInstall_Date, cuts, right = TRUE)
data$RRevisedInstallYear <- cut(data$RRevised_Install_Date, cuts, right = TRUE)

tbl_RInstallYear <- (table(data$RInstallYear))
tbl_RRevisedInstallYear <- (table(data$RRevisedInstallYear))


library(plotly)
p <- plot_ly(
  x = rownames(tbl_RInstallYear),
  y = tbl_RInstallYear,
  fill = "tozeroy"
)

add_trace(p,
  x = rownames(tbl_RRevisedInstallYear),
  y = tbl_RRevisedInstallYear,
  fill = "tonexty"
)
