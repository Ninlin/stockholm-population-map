##########
# MAP
#########
# data from http://hub.arcgis.com/datasets/a65ed98be782459289d0dea41436dd60_0?geometry=-28.404%2C53.634%2C73.021%2C68.263
# tutorial https://medium.com/@traffordDataLab/lets-make-a-map-in-r-7bd1d9366098

library(sf) 
library(tidyverse) 
library(classInt) 
library(viridis)
library(ggplot2)
library(ggmap)

sweden <- st_read("https://opendata.arcgis.com/datasets/a65ed98be782459289d0dea41436dd60_0.geojson") 

alla_kommuner <- toupper(c("Stockholm", "Bromma", "Enskede", "Farsta", "Liljeholmen", "Hägersten", "Hässelby", "Johanneshov", "Kista", "Älvsjö", "Årsta", "Vällingby", "Spånga", "Skarpnäck", "Sköndal", "Skärholmen", "Enskededalen", "Enskede Gård", "Bandhagen", "Bagarmossen"))

#city <- toupper(c("stockholm"))

#plot(stockhom$POSTNR) - to find the outlier

#filter stockholm data
stockholm <- sweden %>%
        filter(POSTORT %in% alla_kommuner) %>%
        select(POSTORT, POSTNR, Net_Income_Median, 
               Motherland_Parents_Born_Abroad, Motherland_Born_Abroad,
               Age_0_6, Age_6_19, Age_20_24, Age_25_44, Age_45_64, Age_65_W,
               BEF111231,geometry) %>%
       # filter(POSTNR < 13000) %>%
        mutate(pbapercap = Motherland_Parents_Born_Abroad/BEF111231,
               bapercap = (Motherland_Born_Abroad)/BEF111231,
               all = Age_0_6 + Age_6_19 + Age_20_24 + Age_25_44 + Age_45_64 + Age_65_W,
               bapercap2 = round((Motherland_Born_Abroad/all)*100, 0)) %>%
        filter(pbapercap > 0)
#plot(st_geometry(stockholm))                                  

#create classes of the percentage variable
classes <- classIntervals(stockholm$bapercap2, n = 5, style = "jenks")
stockholm <- stockholm %>%
        mutate(percent_class = cut(bapercap2, classes$brks, include.lowest = T))

#categorized map 1
map1 <- ggplot(stockholm) + 
        geom_sf(aes(fill = cut_number(bapercap2, 5)),
                alpha = 0.8, colour = 'white', size = 0.1) +
        scale_fill_brewer(palette = "GnBu",                   
                          name = "Born Abroad (%)") +    
        labs(x = NULL, y = NULL, 
             title = "Residents born aboad in the Stockholm county, 2016(?)",
             caption = "Caption here") +  
        theme(panel.background = element_blank(),                     
              line = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text = element_blank(),                      
              axis.title = element_blank()) +
        coord_sf(datum = NA)


#categorized map 2
map2 <- ggplot(stockholm) + 
        geom_sf(aes(fill = percent_class),
                alpha = 0.8, colour = 'white', size = 0.1) +
        scale_fill_brewer(palette = "GnBu",                   
                          name = "Born Abroad (%)"
                          # ,
                          # guide_colourbar(direction = "horizontal", 
                          #                 barheight = unit(2, units = "mm"),
                          #                 barwidth = unit(50, units = "mm"),
                          #                 draw.ulim = F,
                          #                 title.position = 'top',
                          #                 title.hjust = 0.5,
                          #                 label.hjust = 0.5)
                          ) +    
        labs(x = NULL, y = NULL, 
             title = "Residents born aboad in the Stockholm county, 2016(?)",
             caption = "Caption here") +  
        theme(panel.background = element_blank(),        
              line = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank()) +
        coord_sf(datum = NA)

#continuous scale map
map3 <- ggplot(stockholm) + 
        geom_sf(aes(fill = bapercap2),
                alpha = 0.8, colour = 'white', size = 0.1) +
        scale_fill_distiller(palette = "GnBu",
                             direction = 1,
                             name = "Born Abroad (%)") +    
        labs(x = NULL, y = NULL, 
             title = "Residents born aboad in the Stockholm county, 2016(?)",
             caption = "Caption here") +  
        theme(panel.background = element_blank(),        
              line = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank()) +
        coord_sf(datum = NA)

map2
