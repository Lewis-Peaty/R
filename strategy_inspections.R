#######---------------- 1.0 Boilerplate----------------#########
library(RODBC) #odbc library
library(sqldf) #use sql on data frames
library(xlsx)  #export data to xlsx
setwd("")

#Set some dates
Start1213 <- as.Date("2012-7-1")
Start1314 <- as.Date("2013-7-1")
Start1415 <- as.Date("2014-7-1")
Start1516 <- as.Date("2015-7-1")
Start1617 <- as.Date("2016-7-1")
Start1718 <- as.Date("2017-7-1")







#################------------ 2.0 Data Extraction----------------#################
#Extract data from Streetlight Info Pack data sources.
#Extract Inspections table
db<-file.path("Lamp Standard Poles  Inspection Data.accdb")
channel<-odbcConnectAccess2007(db)
inspections<-sqlFetch(channel,"Tbl_Lamp_Standard_LSTD_Inpsecton_Data_Current")
#inspections<-sqlFetch(channel,"Most_Recent_Inspections")

#Extract (most recent) Profile table
db<-file.path("Profile Data.accdb")
channel<-odbcConnectAccess2007(db)
profile<-sqlFetch(channel,"Profile")

#Extract SOTI Profile Table
db<-file.path("Profile_Source_Data_SOTI.mdb")
channel<-odbcConnectAccess2007(db)
profile_SOTI<-sqlFetch(channel,"Profile_SOTI")







#################----------------------- 3.0 Data Processing------------------------#################

#Remove SD rows
profile_SOTI <- profile_SOTI[profile_SOTI$EQUIPMENT_STATUS=="AC",]
profile <- profile[profile$EQUIPMENT_STATUS=="AC",]

#Do not use this profile_SOTI age
profile_SOTI$AGE <- 0 

#Format install date
profile_SOTI$INSTALLED_DATE <- as.Date(profile_SOTI$DATE_INSTALLED, "%d/%m/%Y")
profile$INSTALLED_DATE <- as.Date(profile$DATE_INSTALLED, "%d/%m/%Y")

#Use financial year subsets
profile1314 <- profile_SOTI[profile_SOTI$FY=="13/14",] #END OF FINANCIAL YEAR 13/14
profile1415 <- profile_SOTI[profile_SOTI$FY=="14/15",] #END OF FINANCIAL YEAR 14/15

profile1314$AGE <- round(digits=0,as.numeric(difftime(Start1415, profile1314$INSTALLED_DATE,units = "days"))/365.25) + 1
profile1415$AGE <- round(digits=0,as.numeric(difftime(Start1516, profile1415$INSTALLED_DATE,units = "days"))/365.25) + 1
profile$AGE <- round(digits=0,as.numeric(difftime(Start1617, profile$INSTALLED_DATE,units = "days"))/365.25) + 1

#Sanity check: plot age profile
plot(table(profile1314$AGE))
plot(table(profile1415$AGE))
plot(table(profile$AGE))

#Format inspection dates
inspections$RInspectionDate <- as.Date(as.character(inspections$INSPECTION_DATE), "%Y-%m-%d") 

#Remove bad inspection dates
inspections <- inspections[inspections$RInspectionDate!="1900-01-01",]

#Calculate most recent inspection prior to the end of the relevant financial year
inspections1314 <- aggregate( RInspectionDate ~ EQUIPMENT_NUMBER + PLANT_NUMBER, data = inspections[inspections$RInspectionDate < Start1415,], FUN = max)
inspections1415 <- aggregate( RInspectionDate ~ EQUIPMENT_NUMBER + PLANT_NUMBER, data = inspections[inspections$RInspectionDate < Start1516,], FUN = max)
inspectionsMostRecent <- aggregate( RInspectionDate ~ EQUIPMENT_NUMBER + PLANT_NUMBER, data = inspections, FUN = max)

#Augment profile with most recent inspection date
#Left join on to the inspections table
profile1314 <- merge(profile1314, inspections1314[,c("EQUIPMENT_NUMBER", "RInspectionDate")], by = "EQUIPMENT_NUMBER", all.x = TRUE )
profile1415 <- merge(profile1415, inspections1415[,c("EQUIPMENT_NUMBER", "RInspectionDate")], by = "EQUIPMENT_NUMBER", all.x = TRUE )
profile <- merge(profile, inspectionsMostRecent[,c("EQUIPMENT_NUMBER", "RInspectionDate")], by = "EQUIPMENT_NUMBER", all.x = TRUE )

#If inspection date < install date something is wrong.
#Set inspection date to NA
# profile1314$BadInspectionDate <- ifelse( profile1314$INSTALLED_DATE > profile1314$RInspectionDate,1,0)
# profile1415$BadInspectionDate <- ifelse( profile1415$INSTALLED_DATE > profile1415$RInspectionDate,1,0)

#drop big tables. I only have 4GB of RAM :/
remove(inspections)
remove(profile_SOTI)





#######------------------- 4.0 Calculation of When Inspections are Due-------------------############

#Time since most recent inspection
profile1314$Years_Since_Inspection <- as.numeric(difftime(Start1415,profile1314$RInspectionDate, units="days")/365.25)
profile1415$Years_Since_Inspection <- as.numeric(difftime(Start1516,profile1415$RInspectionDate, units="days")/365.25)
profile$Years_Since_Inspection1617 <- as.numeric(difftime(Start1617,profile$RInspectionDate, units="days")/365.25)
profile$Years_Since_Inspection1718 <- as.numeric(difftime(Start1718,profile$RInspectionDate, units="days")/365.25)

#Print rows where age < years since inspection
#THIS SHOULD RETURN NOTHING
# broken_rows <- profile1314[profile1314$AGE<profile1314$Years_Since_Inspection,c("AGE","Years_Since_Inspection")]
# table(round(digits = 0,broken_rows$Years_Since_Inspection),broken_rows$AGE,dnn = c("Yrs Since Inspection","Age"))

#Pole is due for inspection if it has not been inspected in the past 4 years,
#or if it is between 4 and 8 years old and in a corrosive zone.
profile1314$DueIn1415 <- ifelse( (is.na(profile1314$Years_Since_Inspection) | profile1314$Years_Since_Inspection>=4) & profile1314$AGE>=8,1,0)
NumInspectionsNewRules1415 <- sum(profile1314$DueIn1415, na.rm = TRUE)
cat("Strategy Inspections in 14/15 under new rules: ", sum(profile1314$DueIn1415, na.rm = TRUE))
profile1314$DueIn1415 <- ifelse( (is.na(profile1314$Years_Since_Inspection) | profile1314$Years_Since_Inspection>=4) & profile1314$AGE>=4 & profile1314$CorrosionZone==1,1,profile1314$DueIn1415)
NumInspectionsOldRules1415 <- sum(profile1314$DueIn1415, na.rm = TRUE)
cat("Strategy Inspections in 14/15 under old rules: ", sum(profile1314$DueIn1415, na.rm = TRUE))

profile1415$DueIn1516 <- ifelse( (is.na(profile1415$Years_Since_Inspection) | profile1415$Years_Since_Inspection>=4) & profile1415$AGE>=8,1,0)
NumInspectionsNewRules1516 <- sum(profile1415$DueIn1516, na.rm = TRUE)
cat("Strategy Inspections in 15/16 under new rules: ", sum(profile1415$DueIn1516, na.rm = TRUE))
profile1415$DueIn1516 <- ifelse( (is.na(profile1415$Years_Since_Inspection) | profile1415$Years_Since_Inspection>=4) & profile1415$AGE>=4 & profile1415$CorrosionZone==1,1,profile1415$DueIn1516)
NumInspectionsOldRules1516 <- sum(profile1415$DueIn1516, na.rm = TRUE)
cat("Strategy Inspections in 15/16 under old rules: ", sum(profile1415$DueIn1516, na.rm = TRUE))

profile$DueIn1617newrules <- ifelse( (is.na(profile$Years_Since_Inspection1617) | profile$Years_Since_Inspection1617>=4) & profile$AGE>=8,1,0)
NumInspectionsNewRules1617 <- sum(profile$DueIn1617newrules, na.rm = TRUE)
cat("Strategy Inspections in 16/17 under new rules: ", NumInspectionsNewRules1617)
profile$DueIn1617oldrules <- ifelse( (is.na(profile$Years_Since_Inspection1617) | profile$Years_Since_Inspection1617>=4) & profile$AGE>=4 & profile$CorrosionZone==1,1,profile$DueIn1617newrules)
NumInspectionsOldRules1617 <- sum(profile$DueIn1617oldrules, na.rm = TRUE)
cat("Strategy Inspections in 16/17 under old rules: ", NumInspectionsOldRules1617)

profile$DueIn1718newrules <- ifelse( (is.na(profile$Years_Since_Inspection1718) | profile$Years_Since_Inspection1718>=4) & profile$AGE>=8,1,0)
NumInspectionsNewRules1718 <- sum(profile$DueIn1718newrules, na.rm = TRUE)
cat("Strategy Inspections in 17/18 under new rules: ", NumInspectionsNewRules1718)
profile$DueIn1718oldrules <- ifelse( (is.na(profile$Years_Since_Inspection1718) | profile$Years_Since_Inspection1718>=4) & profile$AGE>=4 & profile$CorrosionZone==1,1,profile$DueIn1718newrules)
NumInspectionsOldRules1718 <- sum(profile$DueIn1718oldrules, na.rm = TRUE)
cat("Strategy Inspections in 17/18 under old rules: ", NumInspectionsOldRules1718)






##########--------------------- 5.0 Summary -------------------------###########

#Strategy Requirements
sum(profile1314$DueIn1415, na.rm = TRUE)
sum(profile1415$DueIn1516, na.rm = TRUE)
sum(profile$DueIn1617, na.rm = TRUE)
sum(profile$DueIn1718, na.rm = TRUE)

#Summary tables
table(round(digits = 0,profile1314$Years_Since_Inspection),profile1314$DueIn1415,dnn = c("Yrs Since Inspection","Due[Y/N?]"))
# table(round(digits = 0,profile1314$Years_Since_Inspection),profile1314$AGE,dnn = c("Yrs Since Inspection","Age"))

#rows where years since inspection > 8 but not due
profile1314[profile1314$DueIn1415==0 & profile1314$Years_Since_Inspection>= 8 & !is.na(profile1314$Years_Since_Inspection),c("AGE","Years_Since_Inspection","DueIn1415")]

#Find rows where age < years since inspection
broken_rows <- profile1314[profile1314$AGE<profile1314$Years_Since_Inspection & !is.na(profile1314$Years_Since_Inspection),c("AGE","Years_Since_Inspection")]
table(round(digits = 0,broken_rows$Years_Since_Inspection),broken_rows$AGE,dnn = c("Yrs Since Inspection","Age"))

#Inspection actuals
actual1415 <- length(inspections1415[inspections1415$RInspectionDate>"2014-07-01",][[1]] )                    
actual1516 <- length(inspectionsMostRecent[inspectionsMostRecent$RInspectionDate>"2015-07-01",][[1]])
cat( "The number of distinct poles inspected in 14/15: ", actual1415)
cat( "The number of distinct poles inspected in 15/16: ", actual1516)
actuals <- as.data.frame(c(actual1415,actual1516),c("actual1415","actual1516"))
colnames(actuals)=c("Inspections")

#Export required inspections
Strategy_Inspections <- as.data.frame(c(NumInspectionsOldRules1718,NumInspectionsOldRules1617,NumInspectionsOldRules1516,NumInspectionsOldRules1415,NumInspectionsNewRules1718,NumInspectionsNewRules1617,NumInspectionsNewRules1516,NumInspectionsNewRules1415)
                                      ,c("NumInspectionsOldRules1718","NumInspectionsOldRules1617","NumInspectionsOldRules1516","NumInspectionsOldRules1415","NumInspectionsNewRules1718","NumInspectionsNewRules1617","NumInspectionsNewRules1516","NumInspectionsNewRules1415"))
colnames(Strategy_Inspections)=c("Required Inspections")
write.csv(x=Strategy_Inspections, file = "Strategy_Inspection.csv")



#########---------------------- 6.0 Export Data ---------------------##########
# db<-file.path("Lamp Standard Poles  Inspection Data.accdb")
# Channel<-odbcConnectAccess2007(db)
# sqlSave(channel = Channel, dat = profile1314 ,tablename = "Profile1314", nastring = "NA")

# write.csv(x = profile1314, file = "")

