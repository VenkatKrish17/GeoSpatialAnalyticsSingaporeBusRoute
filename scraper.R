library(httr)
library(rjson)

busstops<-read.csv("BUSSTOPS.csv")

skipcount<-NULL
skipcount['30']<-"11347"
skipcount['196']<-"8536"
skipcount['51']<-"14372"
skipcount['166']<-"5646"
skipcount['284']<-"10971"
skipcount['133']<-"2581"
skipcount['502']<-"14176"
skipcount['518']<-"14564"
skipcount['97']<-"23431"

plotTheData<-function(serviceNo,direction){
  
  route10<-read.csv(paste("scraped/Route",serviceNo,"_",direction,".csv",sep=""))
  route10$lat<-0
  route10$long<-0
  
  buspos<<-NULL
  routeInfo<<-NULL;
  
  for (row in 1:nrow(route10)){
    if(!is.na(route10[row,]$BusStopCode)){
      #print(route10[row,]$BusStopCode)
      lat=busstops[busstops$BusStopCode==route10[row,]$BusStopCode,]$Latitude
      long=busstops[busstops$BusStopCode==route10[row,]$BusStopCode,]$Longitude
      route10[row,]$lat<-lat
      route10[row,]$long<-long
      #print(route10[row,])
     
      url=paste('http://datamall2.mytransport.sg/ltaodataservice/BusArrivalv2?BusStopCode=',route10[row,]$BusStopCode,'&ServiceNo=',serviceNo,sep="")
      response<-GET(url, 
                    accept_json(), 
                    add_headers(c('AccountKey' = 'RAGHPk3qTNC685iHir8V8w==',
                                  'accept'='application/json')))
      
      json_data=fromJSON(as.character(response))
      
      json_data <- lapply(json_data, function(x) {
         if(typeof(sapply(x,is.null))=="list"){
           print("sapply list")
           x[unlist(sapply(x,is.null))]<-NA
         }
        else{
          x[sapply(x,is.null)]<-NA
        }
        
        
        
        unlist(x) 
            
      })
      #print(json_data$Services)
      #print(lat)
      #print(long)
      json_data$Services$lat<-as.numeric(lat)
      json_data$Services$long<-as.numeric(long)
      #print(json_data)
      json_data$Services$BusStopCode<-route10[row,]$BusStopCode
      json_data$Services$timestamp<-as.character(Sys.time())
      if(is.null(routeInfo)){
        routeInfo<<-json_data$Services
      }
    else{
      rbind(routeInfo,json_data$Services)->>routeInfo
    }
    }
  }
}


busrouteGenerator <- function(serviceNo, direction) {
  #print(serviceNo)  
  #route 10 -- http://datamall2.mytransport.sg/ltaodataservice/BusRoutes
  #route 196- http://datamall2.mytransport.sg/ltaodataservice/BusRoutes?$skip=8536
  #route 30 - http://datamall2.mytransport.sg/ltaodataservice/BusRoutes?$skip=11347
  #print(as.character(skipcount[as.character(serviceNo)]))
  url=paste('http://datamall2.mytransport.sg/ltaodataservice/BusRoutes?$skip=',as.character(skipcount[as.character(serviceNo)]),sep="")
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
  filename = paste("scraped/Route",serviceNo,"_",direction,".csv",sep="")
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
  count<-8
  while(count>=0){
    plotTheData(serviceNo,direction)
    #time<-gsub(" ","_",Sys.time())
    #time<-gsub(":","_",time)
    #time<-gsub("-","_",time)
    #routeInfo$time<<-Sys.time()
    filename<-paste("scraped/route",serviceNo,"_",direction,"_","snapshot",".csv",sep="")
    print(filename)
    write.table(routeInfo,filename, sep = ",", col.names = T, append = T)
    Sys.sleep(480)
    count<<-count+1
  }
  
  #  return(busStops)
  
}



#fill name <- route_direction_year_month_date_hh_mm_ss.csv
#busrouteGenerator(133,1)
busrouteGenerator(518,1) 


#print(busStops)




