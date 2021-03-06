library(httr)
library(rjson)

library(leaflet)
busstops<-read.csv("BUSSTOPS.csv")
#print(Sys.time())
m <- leaflet()  %>% addProviderTiles(providers$CartoDB.Positron)
skipcount<-NULL
skipcount['30']<-"11347"
skipcount['196']<-"8536"
skipcount['51']<-"14372"
skipcount['166']<-"5646"
skipcount['284']<-"10971"
skipcount['133']<-"2581"
skipcount['502']<-"14176"
skipcount['518']<-"14564"


legend<-NULL
legend['30']<-"#196F3D"
legend['196']<-"#6C3483"
legend['51']<-"#212F3C"
legend['166']<-"#2E86C1"
legend['284']<-"#ffff33"

weightdata$SEA<-10
weightdata$SDA<-15
weightdata$LSD<-20

#print(skipcount['196'])
busstopIcon <- makeIcon(
  iconUrl = "doticon.png",
  iconWidth = 10, iconHeight = 10,
  iconAnchorX = 0, iconAnchorY = 0
)
busIcon <- makeIcon(
  iconUrl = "busicon.png",
  iconWidth = 10, iconHeight = 10,
  iconAnchorX = 10, iconAnchorY = 10
)
singleBusIcon <- makeIcon(
  iconUrl = "single.png",
  iconWidth = 10, iconHeight = 10,
  iconAnchorX = 10, iconAnchorY = 10
)
doubleBusIcon <- makeIcon(
  iconUrl = "double.png",
  iconWidth = 10, iconHeight = 10,
  iconAnchorX = 10, iconAnchorY = 10
)
bendyBusIcon <- makeIcon(
  iconUrl = "bendy.png",
  iconWidth = 10, iconHeight = 10,
  iconAnchorX = 10, iconAnchorY = 10
)
plotTheData<-function(serviceNo,direction){
  
  route10<-read.csv(paste("Route",serviceNo,"_",direction,".csv",sep=""))
  route10$lat<-0
  route10$long<-0
  
  buspos<<-NULL
  routeInfo<-NULL;
  
  for (row in 1:nrow(route10)){
    if(!is.na(route10[row,]$BusStopCode)){
      #print(route10[row,]$BusStopCode)
      lat=busstops[busstops$BusStopCode==route10[row,]$BusStopCode,]$Latitude
      long=busstops[busstops$BusStopCode==route10[row,]$BusStopCode,]$Longitude
      route10[row,]$lat<-lat
      route10[row,]$long<-long
      #print(route10[row,])
      m<<-addMarkers(m,lng=long, lat=lat,icon=busstopIcon)
      url=paste('http://datamall2.mytransport.sg/ltaodataservice/BusArrivalv2?BusStopCode=',route10[row,]$BusStopCode,'&ServiceNo=',serviceNo,sep="")
      response<-GET(url, 
                    accept_json(), 
                    add_headers(c('AccountKey' = 'RAGHPk3qTNC685iHir8V8w==',
                                  'accept'='application/json')))
      
      json_data=fromJSON(as.character(response))
      json_data <- lapply(json_data, function(x) {
        x[sapply(x, is.null)] <- NA
        unlist(x)
      })
      #print(json_data$Services)
      #print(lat)
      #print(long)
      json_data$Services$lat<-as.numeric(lat)
      json_data$Services$long<-as.numeric(long)
      #print(json_data)
      json_data$Services$BusStopCode<-route10[row,]$BusStopCode
      #print(json_data$Services)
      #print(json_data$Services)
      
      if(is.null(routeInfo)){
        routeInfo<-json_data$Services
        
        #print("if")
      }
      
      else{
        
        rbind(routeInfo,json_data$Services)->routeInfo
        
      }
      #print(routeInfo)
      
      #asFrame <- do.call("rbind", lapply(json_data, as.data.frame))
      #print(asFrame)
    }
  }
  
  getIcon<-function(type){
    if(type=="SD"){
      singleBusIcon <- makeIcon(
        iconUrl = "single.png",
        iconWidth = 30, iconHeight = 30,
        iconAnchorX = 0, iconAnchorY = 0
      )
      return (singleBusIcon)
    }
    if(type=="DD"){
      doubleBusIcon <- makeIcon(
        iconUrl = "double.png",
        iconWidth = 30, iconHeight = 30,
        iconAnchorX = 0, iconAnchorY = 0
      )
      return (doubleBusIcon)
    }
    if(type=="BD"){
      bendyBusIcon <- makeIcon(
        iconUrl = "bendy.png",
        iconWidth = 30, iconHeight = 30,
        iconAnchorX = 0, iconAnchorY = 0
      )
      return (bendyBusIcon)
    }
    
    
   
  }
  
  
  outdf<-as.data.frame(routeInfo,row.names = routeInfo[1],check.names=FALSE)
  
  
  
  originlat=routeInfo[1,]$lat
  originlong=routeInfo[1,]$long
  buslat=routeInfo[nrow(routeInfo)-2,]$NextBus.Latitude
  buslong=routeInfo[nrow(routeInfo)-2,]$NextBus.Longitude
  bustype=routeInfo[nrow(routeInfo)-2,]$NextBus.Type
  
  busroute=as.character(routeInfo[nrow(routeInfo)-2,]$ServiceNo)
  globalcol<<-NULL
  globalcol<<-as.character(legend[as.character(serviceNo)])
  for(path in 2:nrow(routeInfo)){
    #Sys.sleep(0.1)
    if(as.numeric(weightdata[routeInfo[path,]$NextBus.Load])==15){
      col<-"yellow"
    }
    else if(as.numeric(weightdata[routeInfo[path,]$NextBus.Load])==20){
      col<-"red"
    }
    else{
      col<-globalcol
    }
    m<<-addPolylines(m,lat=c(originlat,routeInfo[path,]$lat),lng=c(originlong,routeInfo[path,]$long),weight=as.numeric(weightdata[routeInfo[path,]$NextBus.Load]),color=col)
    originlat=routeInfo[path,]$lat
    originlong=routeInfo[path,]$long
    
    
  }
  print(buslat)
  print(buslong)
  print(bustype)
  m<<-addMarkers(m,lat=as.numeric(buslat),lng=as.numeric(buslong),icon=getIcon(as.character(bustype)),popup = paste("route ",busroute))

  #m<-addPolylines(m,lat=as.numeric(outdf$lat),lng=as.numeric(outdf$long),weight=10,color =  colordata[outdf$NextBus.Load])
  #m<-addPolylines(m,lat=outdf[!is.na(outdf$lat),]$lat, lng=outdf[!is.na(outdf$long),]$long,col="red",weight=10)
 
  
  
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
  plotTheData(serviceNo,direction)
#  return(busStops)
  
}

busrouteGenerator(30, 2)
busrouteGenerator(196, 1)
#busrouteGenerator(284,1)
busrouteGenerator(166,1)
print(m)
#print(busStops)




