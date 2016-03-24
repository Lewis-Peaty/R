
#From a defined folder, recursively access subfolders and move all files to top level
folder <- "C:\\Users\\n043341\\Documents\\Documents\\Software\\R"
remove_after_copying <- TRUE
setwd(folder)

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
