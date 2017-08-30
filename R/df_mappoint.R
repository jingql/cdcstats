
#登革热病例标点地图

df_mappoint <- function(mdf=mdf){
  library(leaflet)
  names(mdf)[names(mdf) %in% c("经度","纬度")] <- c("lng","lat")
  mdf %>% leaflet() %>% addTiles() %>%
    addCircleMarkers(lng=~lng,lat=~lat,radius=20,popup=~as.character(format(as.Date(发病日期),"%m-%d"))) %>%
    addCircleMarkers(lng=~lng,lat=~lat,radius=40,popup=~as.character(街道))

}


