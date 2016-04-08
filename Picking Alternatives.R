library(RODBC)
library(reshape)
setwd("~/Documents/Projects/AP_Conductor_Sampling/Managing The 500")
db<-file.path("DM13791951_The_500_2.accdb")
channel<-odbcConnectAccess2007(db)
set.seed(342525) #Set the random seed
d<-sqlFetch(channel,"Candidate List")
dd <- sqlFetch(channel,"tst11 tst9 with Experiment ID added")
d2 <- d #copy
d <- d2
for(c in colnames(d)){ if(is.factor(d[,c])){d[,c] <- as.character(d[,c])}} #factors -> char
for(c in colnames(dd)){ if(is.factor(dd[,c])){dd[,c] <- as.character(dd[,c])}} #factors -> char
`%c%` <- function(a,b){paste(sep="",a,b)} #define concat operator
d$INSTALLATION_DATE <- as.character(d$INSTALLATION_DATE) #Handle issue with re-importing dates to access



bad_eids <- vector('character')
euids_without_mtzn <- vector('character')
euids_without_enough_alts <- vector('character')
more_than_5_warning <- vector('character')
for(e_id in sort(unique(dd$`Experiment ID`))){ #Main loop
  temp <- d[d$`Experiment ID`==e_id,] 
  print("Experiment ID: " %c% as.character(e_id))
  mntz_tab <- table(temp$MAINT_ZONE_NAM[is.na(temp$`Sample Number`)& temp$Reject==0])
  mntz_tab <- sort(mntz_tab,decreasing = TRUE )
  mntz_tab
  
  #For each unit id, select a maintenance zone and select 3 new alternatives from 
  #that maintenance zone
  unit_ids <- unique(dd$`Experimental Unit ID`[dd$`Experiment ID`==e_id]) ; unit_ids
  unit_ids <- sort(unit_ids)
  
  Limiting_Factor <- ifelse(length(mntz_tab)<length(unit_ids),length(mntz_tab),length(unit_ids))
  if(Limiting_Factor==0){
    print("Experiment ID " %c% as.character(e_id) %c% " does not have any spare maintenance zones.")
    bad_eids <- append(bad_eids,e_id)
    next
  }
  
  #Check we have enough alternatives and maintenance zones to proceed
  if(length(mntz_tab)<length(unit_ids)){
    print("Experiment ID " %c% as.character(e_id) %c% " does not have required number of distinct maintenance zones.")
    euids_without_mtzn <- append(euids_without_mtzn,unit_ids)
    # next
  }
  if(any(mntz_tab[1:length(unit_ids)]<2)){
    print("Experiment ID " %c% as.character(e_id) %c% " does not have at least 2 alt for each maintenance zone.")
    euids_without_enough_alts <- append(euids_without_enough_alts,unit_ids)
    # next
  }
  
  #Proceed regardless but handle length(unit_ids)!=5
  for(i in 1:Limiting_Factor){
    s <- ifelse(mntz_tab[i]<3,mntz_tab[i],3)
    temp2 <- temp[is.na(temp$`Sample Number`) & temp$MAINT_ZONE_NAM==names(mntz_tab)[i] & temp$Rejected==0 & temp$Expensive==0,]
    r <- sample(1:nrow(temp2),size = s,replace = F) ; r 
    sample_number <- substr(unit_ids[i],start=nchar(unit_ids[i]),nchar(unit_ids[i]))
    
    for(j in 1:s){
      temp2[r[j],]$`Experimental Unit ID` <- unit_ids[i]
      temp2[r[j],]$`Sample Number` <- sample_number #careful...
      temp2[r[j],]$`Experimental Unit Alternative` <- LETTERS[1+j] #"B"
      temp2[r[j],]
    }

    
    #pop temp2 back into temp
    temp[is.na(temp$`Sample Number`) & temp$MAINT_ZONE_NAM==names(mntz_tab)[i] & temp$Rejected==0 & temp$Expensive==0,] <- temp2
  }
  d[d$`Experiment ID`==e_id,] <- temp #pop temp back into  d
  
  #Display results
  print(table(temp$`Experimental Unit ID`,temp$`Experimental Unit Alternative`))
  t2 <- melt(table(temp$`Experimental Unit ID`,temp$MAINT_ZONE_NAM))
  t2 <- t2[t2$value!=0,]
  t2$Var.2 <- as.character(t2$Var.2)
  print(xtabs(value ~ Var.1 + Var.2,t2))
  if(any(t2$value>3)){print("Warning! More than 3 in one maintenance zone...")}
  if(any(t2$value>5)){print("DANGER WILL ROBINSON: >5 in one maintenance zone?!...");more_than_5_warning <- append(more_than_5_warning,e_id)}
  cat("Experiment " %c% as.character(e_id) %c% " end.\n\n...........................................")
  
}
print("These " %c% as.character(length(bad_eids)) %c% " experiments have no more alternatives")
print(bad_eids)
print("These " %c% as.character(length(euids_without_enough_alts)) %c% " euids do not have enough alts")
print(euids_without_enough_alts)
print("These " %c% as.character(length(euids_without_mtzn)) %c% " euids do not have enough mtzn")
print(euids_without_mtzn)



sqlDrop(channel,"R Output Candidate List")
sqlSave(channel,dat = d, "R Output Candidate List")
