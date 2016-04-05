library(RODBC)
setwd("~/Documents/Projects/AP_Conductor_Sampling/Managing The 500")
db<-file.path("DM13791951_The_500_2.accdb")
channel<-odbcConnectAccess2007(db)
set.seed(342525) #Set the random seed
d<-sqlFetch(channel,"b1 Alternative Candidates From Blanks")


`%c%` <- function(a,b){paste(sep="",a,b)}
#ffs
# used_conductors <- ""
for(c in colnames(d)){ if(is.factor(d[,c])){d[,c] <- as.character(d[,c])}}

exp_unit_ids <- unique(d$`Experimental Unit ID`) ;exp_unit_ids
for(id in exp_unit_ids){
 temp <- d[d$`Experimental Unit ID`== id,] #isolate exp unit id rows
 if(nrow(temp)<1){print("No alternatives left for: " %c% id) ; next}
 # print(temp)
 r <- round(runif(1,1,nrow(temp))) ; r #A random number is good!
 alt <- temp$`Primary Alternative Number`[r] #get the alt letter (mislabelled)
 new_alt <- grep(alt,LETTERS)+1 #increment alt letter
 if(new_alt==17){new_alt<-2} #P...
 temp$`Experimental Unit Alternative`[r] <- LETTERS[new_alt] #apply to new alt
 temp$`AlternativeSample Number`[r] <- temp$`Primary Sample Number`[r]
 # used_conductors <- append(used_conductors,temp$`Alternative Conductor ID`[r])
 d[d$`Experimental Unit ID`==id,] <- temp
 
}

t <- as.data.frame(table(d$`Alternative Conductor ID`,d$`Experimental Unit Alternative`))
nrow((table(d$`Experimental Unit ID`,d$`Experimental Unit Alternative`)))
# print(t)
# print(t[t$Freq>1,])
# print(t[t$Freq==0,])
#if(!nrow(t[t$Freq>1,])>0)

#Remove duplicates
ag <- aggregate(Freq ~ Var1,t, sum)
dupes <- ag[ag$Freq>1,]
for (dupe in as.character(dupes$Var1)){
  print(d[d$`Alternative Conductor ID` %in% dupe & !is.na(d$`Experimental Unit Alternative`) ,])
  temp <- d[d$`Alternative Conductor ID` %in% dupe & !is.na(d$`Experimental Unit Alternative`) ,]
  
  temp[2:nrow(temp),c("Experimental Unit Alternative")] <- NA
  temp[2:nrow(temp),c("AlternativeSample Number")] <- NA
  
  d[d$`Alternative Conductor ID` %in% dupe & !is.na(d$`Experimental Unit Alternative`) ,] <- temp
  print(d[d$`Alternative Conductor ID` %in% dupe & !is.na(d$`Experimental Unit Alternative`) ,])
}

print("Complete")
t <- as.data.frame(table(d$`Alternative Conductor ID`,d$`Experimental Unit Alternative`))
print(t[t$Freq>1,])
  
sqlDrop(channel,"tbl1 b1 R Output Firm Candidates")
sqlSave(channel,dat = d, "tbl1 b1 R Output Firm Candidates")
