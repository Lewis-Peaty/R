
#From a defined folder, recursively access subfolders and move all files to top level
folder <- "C:\\Users\\n043341\\Documents\\Documents\\Software\\R"
setwd(folder)

#Renaming Version (moves files by renaming them- true story!)
for(item in list.files(folder, recursive = TRUE)){
  if(length(grep("/",item))==0){next} #Skip files in top level folder
  s <- unlist(strsplit(item,"/"))
  to <- paste(sep="",folder,"/",s[length(s)])
  if(file.rename(from = item, to = to)){
    print(paste("Successfully moved",item,"to",folder)) 
  }else{print(paste("Failed to move",item))}
}

#Copying Version
remove_after_copying <- FALSE
for(item in list.files(folder, recursive = TRUE)){
  if(length(grep("/",item))==0){next} #Skip files in top level folder
  if(file.copy(from = item,to = folder)){
    print(paste("Successfully copied",item,"to",folder)) 
    if(remove_after_copying){
      if(file.remove(item)){print(paste("Removed",item))
      }else{print(paste("Failed to remove",item))}
    }else{print(paste("Did not remove",item))}
  }else{print(paste("Failed to copy",item))}
}




