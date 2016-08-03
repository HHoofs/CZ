Data <- read.table("CVC.txt",header = TRUE,sep="\t")

require(foreign)
require(ggmap)
require(stringr)
require(ggvis)
require(maptools)
require(rgdal)

Data <- read.spss("CZ.sav",to.data.frame = TRUE,trim.factor.names = TRUE)

Data <- Data[-1,]
names(Data) <- c("Inst","Place","Dec","Prod","Desc","Price")

Data$Place <- str_trim(as.character(Data$Place))

Data$Prod <- str_trim(as.character(Data$Prod))

head(Data)

str(Data)

?geocode

DataGeo <- data.frame(Place=unique(Data$Place),Long=NA,Lat=NA,stringsAsFactors = FALSE)

for(i in 1:nrow(DataGeo)){
  geoLonLat <- geocode(DataGeo[i,"Place"])
  DataGeo[i,c("Long","Lat")] <- geoLonLat
}

geocode(unique(Data$Place)[1])

DataGeo %>% ggvis(~Lat,~Long)

read.shp("columbus.shp", format="polygon")

myShapeInR<-readOGR("Netherlands_shapefile","myShapeFile")
gor=readShapeSpatial('Netherlands_shapefile/nl_10km.shp')

map <- get_googlemap('netherlands',zoom = 7)

plot(map)

DataFin <- merge(Data,DataGeo,by="Place",all.x=TRUE)

DataFinSel <- subset(DataFin,Prod=="972804034")


HoustonMap <- ggmap(map, base_layer = ggplot(aes(x = Long, y = Lat),
                                                 data = DataFinSel)) +
  geom_point(aes(x = Long, y = Lat, fill=Price),
             data = DataFinSel,pch=21,colour="black",size=4)


HoustonMap +
  stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
                 bins = 5, geom = "polygon",
                 data = violent_crimes) +
  scale_fill_gradient(low = "black", high = "red") +
  facet_wrap(~ day)




library(plotGoogleMaps)

polygons.plot <- polygons[,c("CO2","GDP.capita","NAME")]
polygons.plot <- polygons.plot[polygons.plot$NAME!="Antarctica",]
names(polygons.plot) <- c("CO2 emissions (metric tons per capita)","GDP per capita (current US$)","Country Name")

#Full Page Map
map <- plotGoogleMaps(polygons.plot,zoom=4,fitBounds=F,filename="Map_GoogleMaps.html",layerName="Economic Data")


#To add this to an existing HTML page
map <- plotGoogleMaps(polygons.plot,zoom=2,fitBounds=F,filename="Map_GoogleMaps_small.html",layerName="Economic Data",map="GoogleMap",mapCanvas="Map",map.width="800px",map.height="600px",control.width="200px",control.height="600px")
