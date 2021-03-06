library(sqldf)

setwd("C:\\Users\\n043341\\Documents\\Documents\\Projects\\AP_Conductor_Sampling\\Computing New Samples for the 500 15-3-2016\\")
d <- read.csv("C:\\Users\\n043341\\Documents\\Documents\\Projects\\AP_Conductor_Sampling\\Computing New Samples for the 500 15-3-2016\\candidate_list.csv",stringsAsFactors = FALSE)
d2 <- read.csv("C:\\Users\\n043341\\Documents\\Documents\\Projects\\AP_Conductor_Sampling\\Computing New Samples for the 500 15-3-2016\\candidate_list.csv",stringsAsFactors = FALSE)
failed_samples <- read.csv("C:\\Users\\n043341\\Documents\\Documents\\Projects\\AP_Conductor_Sampling\\Computing New Samples for the 500 15-3-2016\\failed_samples.csv",stringsAsFactors = FALSE)

#Fix datatypes
d$ID <- as.character(d$ID)
d$Primary.or.Alternate <- as.character(d$Primary.or.Alternate)
d$Output_Materials_KeyWord <- paste(sep="","'",as.character(d$Output_Materials_KeyWord))
d$Test.Matrix.1...Health.Index_KeyWord <- paste(sep="","'",as.character(d$Test.Matrix.1...Health.Index_KeyWord))
d$ReplaceList <- ifelse(is.na(d$ReplaceList),"",d$ReplaceList)
d$X15.16.Replacelist <- ifelse(is.na(d$X15.16.Replacelist),"",d$X15.16.Replacelist)

#Remove manually generated alternatives from earlier
table(d$Primary.or.Alternate)
pa <- d$Primary.or.Alternate
pa <- ifelse(substring(pa,first=1,last=1)=="A" | substring(pa,first=1,last=1)=="P",pa,"")
d$Primary.or.Alternate <- pa
table(d$Primary.or.Alternate)

#Show material combinations
table(d$Materials,d$EQUIP_CODE)
#Filter out bad materials
  d$BadMaterialType <- 0
  d$BadMaterialType <- ifelse((d$Materials=="CU" & (d$EQUIP_CODE=="HVCO" | d$EQUIP_CODE=="HVSP" )),1,d$BadMaterialType)
  model_code_exceptions <- c("7/1.75CU","7/1.75 CU","7/16 CU","7/16 CU","7/18CU","7/18 CU","7/20CU","7/20 CU")
  for(exception in model_code_exceptions){
    d[grep(exception,d$Material),c("BadMaterialType")] <- 1
  }
#Check we only hit copper and got all the HV overhead
table(d$BadMaterialType,d$Materials)
table(d$BadMaterialType,d$EQUIP_CODE)

#Keep a record of alternatives
alternatives <- data.frame(FailedCarrier=as.character(0),ID=as.character(0),SampleNum=as.character(0),B=as.character(0),C=as.character(0),D=as.character(0),stringsAsFactors = FALSE)
alternatives[1,1] <- "sdgfdfgdfgdfgdfg"
#Keep a record of ones that went bad
#Check these manually later
bads <- data.frame(FailedCarrier=as.character(0),stringsAsFactors = FALSE)


#Include the candidate carriers with bad material type
failed_carriers_list <- failed_samples$CONDUCTOR_PICK_ID..Carrier.Section.ID.
candidate_carriers_with_bad_materials <- d$NewPICK_ID[d$BadMaterialType==1 & d$Primary.or.Alternate!=""]
failed_carriers_list <- union(failed_carriers_list,candidate_carriers_with_bad_materials)

#Loop through failed samples for carrier codes
for (failed_carrier in failed_carriers_list) {
 #Find the corresponding experiment ID in the candidate list
 id <- d$ID[d$NewPICK_ID==failed_carrier]
 electorate <- d$ELECTRT_NAM[d$NewPICK_ID==failed_carrier]
 cat("\n")
 print(paste(sep="","Checking next carrier: ",failed_carrier," with id: ",id))
 #Catch the case where there is no corresponding experiment id
 if(length(id)==0){
   print("length id == 0")
   bads[nrow(bads)+1,1] <- failed_carrier #record the pick id which doesn't fall into an experiment id bucket
   next}
 alternatives[nrow(alternatives)+1,] <- c(failed_carrier,id,"","","","")
 P_or_A <- d$Primary.or.Alternate[d$NewPICK_ID==failed_carrier]
 sub_sample_num <- substring(P_or_A,first=nchar(P_or_A),last=nchar(P_or_A))
 #Sometimes d$Primary.or.Alternate doesn't have a number so check for that (e.g it's just "A")
 if(length(sub_sample_num)==0){next}
 if(!is.na(as.numeric(sub_sample_num))){
   #Check that alternatives haven't already been found
   if(length(grep(paste(sep="","B",sub_sample_num),rownames(table(d$Primary.or.Alternate[d$ID==id]))))>0){
     print("Alternatives previously found for this ID and sub sample number!")
     next}
   alternatives[nrow(alternatives),3] <- sub_sample_num
   #Try to find new candidates in same electorate
   unused_candidates <- d[d$ID==id & d$Primary.or.Alternate=="" & d$BadMaterialType==0 & d$ELECTRT_NAM==electorate ,]
   electorate_matched = TRUE
   if(nrow(unused_candidates)<3) {
     print(paste(sep="","Cannot find alternatives in same electorate for ",failed_carrier, " :("))
     unused_candidates <- d[d$ID==id & d$Primary.or.Alternate=="" & d$BadMaterialType==0 ,]
     electorate_matched = FALSE
   }
   alts <- nrow(unused_candidates)
   if(alts<1){
     print(paste(sep="","Cannot find any alternatives for ",failed_carrier))
     next}
   if(alts>3){alts=3} #Max 3 alternatives
   counter <- 1
   for (i in 1:alts){ #Loop through alternatives
     letter <- LETTERS[counter+1]
     unused_candidates[i,c("Primary.or.Alternate")] <- paste(sep="",letter,sub_sample_num)
     counter <- counter + 1
     alternatives[nrow(alternatives),i+3] <- unused_candidates[i,c("NewPICK_ID")]
   }
   print(paste(sep="","Found the following alternatives!"))
   print(alternatives[nrow(alternatives),])
   
   #Put the modified rows back into the candidate list
   if(electorate_matched){
   d[d$ID==id & d$Primary.or.Alternate=="" & d$BadMaterialType==0 & d$ELECTRT_NAM==electorate  ,] <- unused_candidates
   } else { d[d$ID==id & d$Primary.or.Alternate=="" & d$BadMaterialType==0 ,] <- unused_candidates }
 }
}

#Remove the boilerplate first row
#http://stackoverflow.com/questions/5231540/r-losing-column-names-when-adding-rows-to-an-empty-data-frame
alternatives <- alternatives[-1,]

#Summary stats
table(d$Primary.or.Alternate)
table(d$Primary.or.Alternate,d$Materials)
table(d$Materials,d$EQUIP_CODE)
table(d$Primary.or.Alternate,d$BadMaterialType)
write.csv2(x=d,file="C:\\Users\\n043341\\Documents\\Documents\\Projects\\AP_Conductor_Sampling\\Computing New Samples for the 500 15-3-2016\\candidate_list_with_alternatives.csv")
write.csv2(x=alternatives,file="C:\\Users\\n043341\\Documents\\Documents\\Projects\\Computing New Samples for the 500 15-3-2016\\AP_Conductor_Sampling\\alternatives.csv")


#Check each experiment
for( id in rownames(table(alternatives$ID))){
  tempdf <- d[d$ID==id & d$Primary.or.Alternate!="",]
  print(paste(sep="","Experiment ID= ",id))
  print(table(tempdf$ELECTRT_NAM))
  print(table(tempdf$Primary.or.Alternate))
  print(table(tempdf$Material))
  print ("Press [enter] to continue")
  number <- scan(n=1)
}

#Which columns have changed from the original dataset?
unmatched_columns = c()
for( i in 1:ncol(d)){
 unmatched_columns <- c(unmatched_columns, any(d[,i]!=d2[,i]))
}
print(colnames(d)[unmatched_columns[TRUE]]) #There are some NAs?

