  library(httr)
  library(rjson)
  busrouteGenerator <- function(serviceNo, direction) {
    print(serviceNo)  
  url=paste('http://datamall2.mytransport.sg/ltaodataservice/BusRoutes?$skip=8536',sep="")
  response<-GET(url, 
                accept_json(), 
                add_headers(c('AccountKey' = 'RAGHPk3qTNC685iHir8V8w==',
                              'accept'='application/json')))
  json_data=fromJSON(as.character(response))
  check = FALSE
  #json_data$BusStopCode<-as.numeric(BusStopCode)
  #json_data$Services$long<-as.numeric(long)
  #json_data$Services$BusStopCode<-route10[row,]$BusStopCode
  #print(json_data$value)
  busRoute <- json_data$value
  #print(length(busRoute))
  #print(busRoute)
  
  busStops <- NULL
  filename = paste("Route",serviceNo,"_",direction,".csv",sep="")
  for(i in  seq(1,length(busRoute))) {
    
    unlisted<-unlist(busRoute[i])
    
    
    if(!is.na(charmatch(as.character(unlisted[1]),as.character(serviceNo))) && identical(as.numeric(unlisted[3]),as.numeric(direction))){
      
         if(is.null(busStops)){
        #check = TRUE
        #print("null condi")
        busStops <- data.frame(unlisted[4],unlisted[5],unlisted[6])
        names(busStops) <- c("SequenceNo","BusStopCode","Distance")
      }
      else{
        #print("normal scenario")
        bus<- data.frame(unlisted[4],unlisted[5],unlisted[6])
        names(bus) <- c("SequenceNo","BusStopCode","Distance")
        busStops <- rbind(busStops,bus)
      }
      }
      
    #print(as.numeric(unlisted[1]))
    
   
    
  }
#  print(busStops)
  write.csv(busStops, file=filename) 
  return(busStops)
    
  }
  
  busStops <- busrouteGenerator(196, 2)
 # print(busStops)