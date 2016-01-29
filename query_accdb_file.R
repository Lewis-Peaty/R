#Using RODBC library to query an accdb file 
library(RODBC)
setwd("C:\\Path\\To\\Your\\Working\\Directory")
db<-file.path("Your.accdb")
channel<-odbcConnectAccess2007(db)
data<-sqlFetch(channel,"Table_Name")
data
