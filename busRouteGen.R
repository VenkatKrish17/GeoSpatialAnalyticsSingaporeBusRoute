  library(httr)
  library(rjson)
  busrouteGenerator <- function(serviceNo) {
    print(serviceNo)  
  url=paste('http://datamall2.mytransport.sg/ltaodataservice/BusRoutes/ServiceNo=',serviceNo,sep="")
  response<-GET(url, 
                accept_json(), 
                add_headers(c('AccountKey' = 'RAGHPk3qTNC685iHir8V8w==',
                              'accept'='application/json')))
  json_data=fromJSON(as.character(response))
  
  #json_data$BusStopCode<-as.numeric(BusStopCode)
  #json_data$Services$long<-as.numeric(long)
  #json_data$Services$BusStopCode<-route10[row,]$BusStopCode
  #print(json_data$value)
  busRoute <- json_data$value
  print(length(busRoute))
  
  busStops <- NULL
  filename = paste("Route",serviceNo,".csv",sep="")
  for(i in  seq(1,length(busRoute))) {
    
    unlisted<-unlist(busRoute[i])
    
    if(!identical(as.numeric(unlisted[1]),as.numeric(serviceNo))){
      break()
    }
    print(as.numeric(unlisted[1]))
    
    if(is.null(busStops)){
      print("null condi")
      busStops <- data.frame(unlisted[4],unlisted[5],unlisted[6])
      names(busStops) <- c("Stop No","Bus Stop Code","Distance")
    }
    else{
      print("normal scenario")
      bus<- data.frame(unlisted[4],unlisted[5],unlisted[6])
      names(bus) <- c("Stop No","Bus Stop Code","Distance")
      busStops <- rbind(busStops,bus)
    }
    
  }
  write.csv(busStops, file=filename) 
  return(busStops)
    
  }
  
  busStops <- busrouteGenerator(10)
  print(busStops)
