#Using RODBC library to query an accdb file 
library(RODBC)
setwd("C:\\Users\\n043341\\Documents\\Documents\\Projects\\AP_Info Packs\\Dx Streetlights")
db<-file.path("Condition_State_Analysis.accdb")
channel<-odbcConnectAccess2007(db)
data<-sqlFetch(channel,"Inspections")
data
