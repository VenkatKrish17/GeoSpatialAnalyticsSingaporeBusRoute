busstops<-read.csv("BUSSTOPS.csv")
library(leaflet)
library(httr)
library(rjson)
m <- leaflet() %>%
  addTiles() 

busstopIcon <- makeIcon(
  iconUrl = "busicon.png",
  iconWidth = 20, iconHeight = 20,
  iconAnchorX = 10, iconAnchorY = 10,
)
route10<-read.csv("result (2).csv")
route10$lat<-0
route10$long<-0
names(route10)[1]<-paste("ServiceNo")

routeInfo<-NULL;

for (row in 1:nrow(route10)){
  if(!is.na(route10[row,]$BusStopCode)){
    print(route10[row,]$BusStopCode)
    lat=busstops[busstops$BusStopCode==route10[row,]$BusStopCode,]$Latitude
    long=busstops[busstops$BusStopCode==route10[row,]$BusStopCode,]$Longitude
    route10[row,]$lat<-lat
    route10[row,]$long<-long
    m<-addMarkers(m,lng=long, lat=lat,icon=busstopIcon)
    url=paste('http://datamall2.mytransport.sg/ltaodataservice/BusArrivalv2?BusStopCode=',route10[row,]$BusStopCode,'&ServiceNo=',route10[row,]$ServiceNo,sep="")
    response<-GET(url, 
              accept_json(), 
              add_headers(c('AccountKey' = 'RAGHPk3qTNC685iHir8V8w==',
                            'accept'='application/json')))
  
    json_data=fromJSON(as.character(response))
    json_data <- lapply(json_data, function(x) {
      x[sapply(x, is.null)] <- NA
      unlist(x)
    })
    json_data$Services$lat<-as.numeric(lat)
    json_data$Services$long<-as.numeric(long)
    json_data$Services$BusStopCode<-route10[row,]$BusStopCode
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
outdf<-as.data.frame(routeInfo,row.names = routeInfo[1],check.names=FALSE)
colordata$SEA<-"Green"
colordata$SDA<-"yellow"
colordata$LSD<-"Red"

originlat=routeInfo[1,]$lat
originlong=routeInfo[1,]$long
for(path in 2:nrow(routeInfo)){
  
  
  m<-addPolylines(m,lat=c(originlat,routeInfo[path,]$lat),lng=c(originlong,routeInfo[path,]$long),weight=10,color = as.character(colordata[routeInfo[path,]$NextBus.Load]))
  originlat=routeInfo[path,]$lat
  originlong=routeInfo[path,]$long
  
}

#m<-addPolylines(m,lat=as.numeric(outdf$lat),lng=as.numeric(outdf$long),weight=10,color =  colordata[outdf$NextBus.Load])
#m<-addPolylines(m,lat=outdf[!is.na(outdf$lat),]$lat, lng=outdf[!is.na(outdf$long),]$long,col="red",weight=10)
print(m)
