library(RODBC)
library(reshape)
setwd("~/Documents/Projects/AP_Conductor_Sampling/Managing The 500")
db<-file.path("DM13791951_The_500_2.accdb")
channel<-odbcConnectAccess2007(db)
set.seed(342525) #Set the random seed
d<-sqlFetch(channel,"e1 Clear BCD Alternatives From Candidate List")
d2 <- d #copy
# used_conductors <- ""
for(c in colnames(d)){ if(is.factor(d[,c])){d[,c] <- as.character(d[,c])}} #factors -> char
`%c%` <- function(a,b){paste(sep="",a,b)}
d$INSTALLATION_DATE <- as.character(d$INSTALLATION_DATE) #Issue with re-importing date to access

# d <- d[is.na(d$`Sample Number`) & d$Rejected==0 & d$Expensive==0,] # remove unacceptable alts
t <- table(d$`Experiment ID`) ; t
Experiment_IDs_For_Manual_Selection <- t[t<30] ; Experiment_IDs_For_Manual_Selection
Experiment_IDs_For_Auto_Selection <- t[t>=30] ;Experiment_IDs_For_Auto_Selection
Experimental_Unit_IDs <- unique(d$`Experimental Unit ID`)
sort(Experimental_Unit_IDs)
unique(d$`Sample Number`)


#Main loop
bad_eids <- vector('character')
bad_euids <- vector('character')
more_than_5_warning <- vector('character')
for(e_id in sort(unique(d$`Experiment ID`))){
 temp <- d[d$`Experiment ID`==e_id,] 
 print("Experiment ID: " %c% as.character(e_id))
 mntz_tab <- table(temp$MAINT_ZONE_NAM[is.na(temp$`Sample Number`)& temp$Rejected==0 & temp$Expensive==0])
 mntz_tab <- sort(mntz_tab,decreasing = TRUE )
 
 #Check we have enough alternatives and maintenance zones to proceed
 if(length(mntz_tab)<5){
   print("Experiment ID " %c% as.character(e_id) %c% " does not have at least 5 different maintenance zones with >3 alts.")
   bad_eids <- append(bad_eids,e_id)
   next
 }
 if(any(mntz_tab[1:5]<3)){
   print("Experiment ID " %c% as.character(e_id) %c% " does not have at least 5 different maintenance zones with >3 alts.")
   bad_eids <- append(bad_eids,e_id)
   next
 }
 
 #For each unit id, select a maintenance zone and select 3 new alternatives from 
 #that maintenance zone
 unit_ids <- unique(temp$`Experimental Unit ID`[!is.na(temp$`Sample Number`)])
 unit_ids <- sort(unit_ids)
 if(length(unit_ids)!=5){
   print("Experiment ID " %c% as.character(e_id) %c% " does not have 5 distinct Sample Numbers.")
   bad_euids <- append(bad_euids,e_id)
   next
 }
 #Proceed regardless but handle length(unit_ids)!=5
 for(i in 1:length(unit_ids)){
   temp2 <- temp[is.na(temp$`Sample Number`) & temp$MAINT_ZONE_NAM==names(mntz_tab)[i] & temp$Rejected==0 & temp$Expensive==0,]
   r <- sample(1:nrow(temp2),size = 3,replace = F) ; r #A random number is good!
   sample_number <- substr(unit_ids[i],start=nchar(unit_ids[i]),nchar(unit_ids[i]))
   temp2[r[1],]$`Experimental Unit ID` <- unit_ids[i]
   temp2[r[1],]$`Sample Number` <- sample_number #careful...
   temp2[r[1],]$`Experimental Unit Alternative` <- "B"
   temp2[r[1],]
   
   temp2[r[2],]$`Experimental Unit ID` <- unit_ids[i]
   temp2[r[2],]$`Sample Number` <- sample_number #careful...
   temp2[r[2],]$`Experimental Unit Alternative` <- "C"
   temp2[r[2],]
   
   temp2[r[3],]$`Experimental Unit ID` <- unit_ids[i]
   temp2[r[3],]$`Sample Number` <- sample_number #careful...
   temp2[r[3],]$`Experimental Unit Alternative` <- "D"
   temp2[r[3],]  
   
   temp[is.na(temp$`Sample Number`) & temp$MAINT_ZONE_NAM==names(mntz_tab)[i] & temp$Rejected==0 & temp$Expensive==0,] <- temp2
 }
 d[d$`Experiment ID`==e_id,] <- temp #pop temp back into  d
 
 #Display results
 print(table(temp$`Experimental Unit ID`,temp$`Experimental Unit Alternative`))
 t2 <- melt(table(temp$`Experimental Unit ID`,temp$MAINT_ZONE_NAM)[2:6,])
 t2 <- t2[t2$value!=0,]
 print(t2)
 if(any(t2$value>3)){print("Warning! More than 3 in one maintenance zone...")}
 if(any(t2$value>5)){print("DANGER WILL ROBINSON: >5 in one maintenance zone?!...");more_than_5_warning <- append(more_than_5_warning,e_id)}
 cat("Experiment " %c% as.character(e_id) %c% " end.\n\n...........................................")
       
}
print("These experiments need manual love and care")
print(bad_eids)


  
#  sqlDrop(channel,"R Output Candidate List")
#  sqlSave(channel,dat = d, "R Output Candidate List")
