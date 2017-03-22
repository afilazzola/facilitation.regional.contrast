##obtains occurences for a particular species or genus using gbif and produces a csv. 

species.occ <- function(genus,sp){
  if(missing(sp)){
    temp<- gbif(genus, geo=T)
    temp <- subset(temp, country=="United States")
    temp<-na.omit(temp[,c("lon","lat","species")])
    temp <- temp[!duplicated(temp), ] 
    write.csv(temp, paste("data\\",genus,".csv",sep=""))
  }  else{
    temp<- gbif(genus, sp, geo=T)
    temp <- subset(temp, country=="United States")
    temp<-na.omit(temp[,c("lon","lat","species")])
    temp <- temp[!duplicated(temp), ] 
    write.csv(temp, paste("data\\",genus,".",sp,".csv",sep=""))
  }
}