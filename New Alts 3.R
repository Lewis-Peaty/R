#Define concatenate function
`%c%` <- function(a,b){paste(sep="",a,b)}

#Set wd to source file location
setwd("~/Documents/Projects/AP_Conductor_Sampling/New Samples For Final Scoping Round The 500 26-5-2016")
library(RODBC)
db<-file.path("DM13791951_The_500_2.accdb")
channel<-odbcConnectAccess2007(db)
d<-sqlFetch(channel,"Candidate List",stringsAsFactors = FALSE) #d is a data frame
deltas <- sqlFetch(channel,"Alans Second Scoping Round Deltas",stringsAsFactors = FALSE,na.string=c("#N/A",""))
deltas <- deltas[deltas$Experiment=="The 500" & is.na(deltas$`Danger Sample`),]
s <- strsplit(deltas$`No Sample Available`,split = "\\.")
deltas$Round <- sapply(X=s,FUN = function(X){X[1]})
deltas$`Experiment Id` <- sapply(X=s,FUN = function(X){X[2]})
deltas$`Sample Number` <- sapply(X=s,FUN = function(X){X[3]})
deltas <- deltas[,5:7]
deltas$`Experimental Unit Id` <- deltas$Round %c% "." %c% deltas$`Experiment Id`
#Now we have formatted deltas and data

#Backup d
d.backup <- d
d <- d.backup

#Filter suitable candidates by Experiment Id
d <- d[d$`Experiment ID` %in% deltas$`Experiment Id`,]
#Remove pre-allocated candidates
d <- d[is.na(d$`Sample Number`),]

#Keep track of how many alts we are missing
missingAltsSum <- 0
missingMNTZSum <- 0
deltas$AltsFound <- 0

#Main loop
for(EID in unique(d$`Experiment ID`)){
  
  d.EID <- d[d$`Experiment ID`==EID,]
  d.MNTZ <- (sort(table(d.EID$MAINT_ZONE_NAM),decreasing = TRUE))
  deltas.SN <- deltas$`Sample Number`[deltas$`Experiment Id` %in% EID]
  
  #Do we have enough spare MNTZ?
  if(length(d.MNTZ) < length(deltas.SN)){
    print("Not Enough MNTZ For " %c% EID)
    missingMNTZSum <- missingMNTZSum + length(deltas.SN) - length(d.MNTZ)
  }
  
  #Loop through the Sample Numbers, assigning up to 5 new alts to each Sample Number
  for(i in 1:length(deltas.SN)){
    SN <- deltas.SN[i]
    
    #Select candidates from largest available MNTZ
    d.EID.MNTZ <- d.EID[d.EID$MAINT_ZONE_NAM==names(d.MNTZ)[i],]
  
    #Select up to 5 alts
    numAlts <- min(c(5,nrow(d.EID.MNTZ)))
    deltas$AltsFound[deltas$`Experiment Id`== EID & deltas$`Sample Number`==SN] <- deltas$AltsFound[deltas$`Experiment Id`==EID & deltas$`Sample Number`==SN]+numAlts
    #Print an alert if numAlts<5
    if(numAlts<5){
      print(EID %c% "." %c% SN %c% " has only " %c% numAlts %c% " alts.")
      missingAltsSum <- missingAltsSum + 5 - numAlts
    }
    
    #Assign alts from candidates
    for(j in 1:numAlts){
      d.EID.MNTZ$`Sample Number`[j] <- SN
      d.EID.MNTZ$`Experimental Unit Alternative`[j] <- LETTERS[7 + j]
    }
    
    #Push changes to d.EID
    d.EID[d.EID$MAINT_ZONE_NAM==names(d.MNTZ)[i],] <- d.EID.MNTZ
  }
  
  #Push changes to d
  d[d$`Experiment ID`==EID,] <- d.EID
  
#   #Now clean up any stragglers- disregard MNTZ
#   d.EID <- d[d$`Experiment ID`==EID & is.na(d$`Sample Number`),]
#   for(i in 1:length(deltas.SN)){
#     SN <- deltas.SN[i]
#     altsFound <- deltas$AltsFound[deltas$`Experiment Id`== EID & deltas$`Sample Number`==SN]
#     j <- 1
#     while(altsFound<5){
#       d.EID$`Sample Number`[j] <- SN
#       d.EID$`Experimental Unit Alternative`[j] <- LETTERS[7 + altsFound + j]
#       j=j+1
#     }
#   }
  
  
}

#The new alternatives- give these to Jeremy once the tests pass
d.alternatives.500 <- d[!is.na(d$`Sample Number`),]
d.alternatives.500 <- d.alternatives.500[,c("Round","Experiment ID","Sample Number","Conductor ID","Experimental Unit Alternative","MAINT_ZONE_NAM")]

#Checksum
nrow(d.alternatives.500) - nrow(deltas)*5 - missingAltsSum == 0 #errr ignore this
sum(deltas$AltsFound) - nrow(d.alternatives.500) == 0

##################################################################################################################
# THE 300

#Deltas for the 300
db<-file.path("DM13791951_The_500_2.accdb")
channel<-odbcConnectAccess2007(db)
deltas <- sqlFetch(channel,"Alans Second Scoping Round Deltas",stringsAsFactors = FALSE,na.string=c("#N/A",""))
deltas <- deltas[deltas$Experiment=="The 300" & is.na(deltas$`Danger Sample`),]
s <- strsplit(deltas$`No Sample Available`,split = "\\.")
deltas$Round <- sapply(X=s,FUN = function(X){X[1]})
deltas$`Experiment Id` <- sapply(X=s,FUN = function(X){X[2]})
deltas$`Sample Number` <- sapply(X=s,FUN = function(X){X[3]})
deltas <- deltas[,5:7]
deltas$`Experimental Unit Id` <- deltas$Round %c% "." %c% deltas$`Experiment Id`
#Grab new candidate data
db<-file.path("The_300.accdb")
channel<-odbcConnectAccess2007(db)
d<-sqlFetch(channel,"Candidate List",stringsAsFactors = FALSE) #d is a data frame

colnames(d)[colnames(d)=="Alternate Letter"] <- "Experimental Unit Alternative"

#Replace NAs
d$MAINT_ZONE_NAM <- ifelse(is.na(d$MAINT_ZONE_NAM),"",d$MAINT_ZONE_NAM)
any(is.na(d$MAINT_ZONE_NAM))

#Backup d
d.backup <- d
d <- d.backup

#Filter suitable candidates by Experiment Id
d <- d[d$`Experiment ID` %in% deltas$`Experiment Id`,]
#Remove pre-allocated candidates
d <- d[is.na(d$`Sample Number`),]

#Keep track of how many alts we are missing
missingAltsSum <- 0
missingMNTZSum <- 0
deltas$AltsFound <- 0

#Main loop
for(EID in unique(d$`Experiment ID`)){
  
  d.EID <- d[d$`Experiment ID`==EID,]
  d.MNTZ <- (sort(table(d.EID$MAINT_ZONE_NAM),decreasing = TRUE))
  deltas.SN <- deltas$`Sample Number`[deltas$`Experiment Id` %in% EID]
  
  #Do we have enough spare MNTZ?
  if(length(d.MNTZ) < length(deltas.SN)){
    print("Not Enough MNTZ For " %c% EID)
    missingMNTZSum <- missingMNTZSum + length(deltas.SN) - length(d.MNTZ)
  }
  
  #Loop through the Sample Numbers, assigning up to 5 new alts to each Sample Number
  for(i in 1:length(deltas.SN)){
    SN <- deltas.SN[i]
    
    #Select candidates from largest available MNTZ
    d.EID.MNTZ <- d.EID[d.EID$MAINT_ZONE_NAM==names(d.MNTZ)[i],]
    
    #Select up to 5 alts
    numAlts <- min(c(5,nrow(d.EID.MNTZ)))
    deltas$AltsFound[deltas$`Experiment Id`==EID & deltas$`Sample Number`==SN] <- deltas$AltsFound[deltas$`Experiment Id`==EID & deltas$`Sample Number`==SN]+numAlts
    #Print an alert if numAlts<5
    if(numAlts<5){
      print(EID %c% "." %c% SN %c% " has only " %c% numAlts %c% " alts.")
      missingAltsSum <- missingAltsSum + 5 - numAlts
    }
    
    
    #Assign alts from candidates
    for(j in 1:numAlts){
      d.EID.MNTZ$`Sample Number`[j] <- SN
      d.EID.MNTZ$`Experimental Unit Alternative`[j] <- LETTERS[7 + j]
    }
    
    #Push changes to d.EID
    d.EID[d.EID$MAINT_ZONE_NAM==names(d.MNTZ)[i],] <- d.EID.MNTZ
  }
  
  #Push changes to d
  d[d$`Experiment ID`==EID,] <- d.EID
  
}

#The new alternatives- give these to Jeremy once the tests pass
d.alternatives.300 <- d[!is.na(d$`Sample Number`),]
d.alternatives.300 <- d.alternatives.300[,c("Round","Experiment ID","Sample Number","Bay ID","Conductor ID","Experimental Unit Alternative","MAINT_ZONE_NAM")]

#Checksum
nrow(d.alternatives.300) - nrow(deltas)*5 - missingAltsSum == 0 #errr ignore this
sum(deltas$AltsFound) - nrow(d.alternatives.300) == 0
length(unique(d.alternatives.300$`Bay ID`)) - length(d.alternatives.300$`Bay ID`) == 0 #bays unique?

write.csv(file = "2nd Round Scoping Alts - Round 1 (the 500).csv",x = d.alternatives.500)
write.csv(file = "2nd Round Scoping Alts - Round 2 (the 300).csv",x = d.alternatives.300)

library(stargazer)

# sqlDrop(channel,"Your Table Name")

# sqlSave(channel,dat = d, "Your Table Name")
