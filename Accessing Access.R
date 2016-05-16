library(RODBC)
setwd("C:/Your/File/Path")
db<-file.path("Your_File_Name.accdb")
channel<-odbcConnectAccess2007(db)
d<-sqlFetch(channel,"Your Table Or Query Name") #d is a data frame

#Stuff
#Happens
#Here

sqlDrop(channel,"Your Table Name")
sqlSave(channel,dat = d, "Your Table Name")
