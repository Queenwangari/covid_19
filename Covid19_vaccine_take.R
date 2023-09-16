setwd("C:/Users/PC/Desktop/R")

library(rgdal)
library(sp)
library(GISTools)
library(sf)



#importing the data
counties<-st_read("kenya_counties.GeoJSON")
print(is.data.frame(counties))
CovidVaccineFully<-read.csv("fully vaccinated.csv")
print(is.data.frame(CovidVaccineFully))

CovidVaccinepart<-read.csv("Partially vaccinated.csv")

print(is.data.frame(CovidVaccinepart))

Healthfacilities<-read.csv("kenya_hf.csv")
print(is.data.frame(Healthfacilities))

#combining counties with covidVaccineFully
Data1<-merge(counties,CovidVaccineFully,by='NAME_1')

#combining counties with CovidVaccinepart
Data2<-merge(counties,CovidVaccinepart,by="NAME_1")

#converting the data frame to numeric values
vec<-gsub("%","",Data1$X..fully.vaccinated)
vec
full_vaccinated<-as.numeric(vec)#changes the percentages from character to numeric
full_vaccinated
vec2<-gsub("%","",Data2$X.pop.received.dose1)
partially_vaccinated<-as.numeric(vec2)
partially_vaccinated

#creating color functions for the datasets data1
range <- c(0,2,4,6,8,10,Inf)
pal1 <- colorBin("YlOrRd",domain =Data1$X..fully.vaccinated,bins = range)

#color function for data2
scale <- c(0,5,10,15,20,25,Inf)
pal2 <- colorBin("magma", domain = Data2$X.pop.received.dose1, bins = scale)

#color function for Health facilities
colorpal=colorFactor(topo.colors(5),Healthfacilities$Type)


#introducing leaflets
library(leaflet)
basemap<-leaflet()%>%
  addTiles(group="OSM(Default)")%>%
  addProviderTiles(providers$Esri.WorldImagery,group = "Esri.WorldImagery")%>%
  addProviderTiles(providers$CartoDB.Positron,group = "CartoDB.Positron")
basemap%>%
  addPolygons(data=Data1,
              fillColor =pal1(full_vaccinated),
              group ="No.of people fully vaccinated" ,
              weight= 0.2,
              opacity = 1,
              color = "black",
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label =paste(Data1$NAME_1,Data1$X..fully.vaccinated),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))%>%
  addPolygons(data = Data2,
              fillColor = pal2(partially_vaccinated),
              group = "No.of people partially vaccinated",
              weight = 0.5,
              opacity = 1,
              color = "black",
              fillOpacity = 0.5,
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label =paste(Data2$NAME_1,Data2$X.pop.received.dose1),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))%>%
  addCircleMarkers(data = Healthfacilities,
                   color = colorpal(Healthfacilities$Type),
                   radius = 0.1,
                   group = "health facilities",
                   clusterOptions = markerClusterOptions(),
                   popup = paste("<h3>Health facility</h3>",
                          "<b>Name:</b>",Healthfacilities$Name,"<br>",
                           "<b>County:</b>",Healthfacilities$County,"<br>",
                           "<b>Type:</b>",Healthfacilities$Type,"<br>",
                           "<b>Ownership:</b>",Healthfacilities$Ownership))%>%
  addLegend(pal=colorpal,
            position="topleft",
            title=" HEALTH FACILITY TYPE",
            values = Healthfacilities$Type)%>%
  addLegend(pal = pal2,
            values = Data2$No.of.pop.partially,
            title = "No.of people partially vaccinated per %",
            opacity = 2,
            position = "bottomleft")%>%
  addLegend(pal = pal1,
            values = Data1$fully_vaccinated,
            title = "No.of people fully vaccinated per %",
            opacity = 2,
            position = "bottomright")%>%
  # Layers control
  addLayersControl(
    baseGroups = c("OSM (default)", "Esri.WorldImagery", "CartoDB.Positron"),
    overlayGroups = c("No.of people fully vaccinated",
                      "No.of people partially vaccinated",
                      "health facilities" ),
    options = layersControlOptions(collapsed = FALSE)
  )


