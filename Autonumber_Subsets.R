
d <- read.csv("C:\\Users\\n043341\\Documents\\Documents\\Software\\R\\derper.csv",stringsAsFactors = FALSE)
d$Output_Materials_KeyWord <- as.character(d$Output_Materials_KeyWord)
d$Primary.or.Alternate <- as.character(d$Primary.or.Alternate)

keywords <- rownames(table(d$Output_Materials_KeyWord))

for( kw in keywords){

  print(nrow(d[d$Output_Materials_KeyWord==kw & d$Primary.or.Alternate=="" ,]))
  alts <- nrow(d[d$Output_Materials_KeyWord==kw & d$Primary.or.Alternate=="" ,])
  if (alts > 129) {alts=129}
  unused_candidates <- d[d$Output_Materials_KeyWord==kw & d$Primary.or.Alternate=="" ,]
  counter <- 1
  for (i in 1:alts){
    
    letter <- LETTERS[floor(counter/5)+1]
    if((letter != "A") & (letter != "P")) {
      unused_candidates[i,3] <- paste(sep="",letter,counter%%5+1)
    }
    counter <- counter + 1
    
  }
  
  d[d$Output_Materials_KeyWord==kw & d$Primary.or.Alternate=="" ,] <- unused_candidates
  
}
d[d$Output_Materials_KeyWord=="1010001001000",3][1:130]
rownames(table(d[d$Output_Materials_KeyWord=="1010001001000",3]))
table(d[d$Output_Materials_KeyWord==kw,3]) 

write.csv2(x=d,file="C:\\Users\\n043341\\Documents\\Documents\\Software\\R\\autonumbered_subsets.csv")


  