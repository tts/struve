library(sf)
library(tidyverse)
library(rvest)
library(leaflet)
library(htmltools)

#-------------------------------------------------- 
# Struve Geodesic Arc points in Finland (PDF)
#--------------------------------------------------

mml <- "https://www.maanmittauslaitos.fi/sites/maanmittauslaitos.fi/files/attachments/2021/06/Struvenpisteet.pdf"
download.file(mml, 'points.pdf', mode="wb")

# Firefox dev tools > Inspector. Copy all innerHTML, add root and save
raw <- read_html("page.html")

links <- raw %>% 
  html_nodes(xpath = "//a[starts-with(@id, 'pdfjs_internal_id_')]/@href") %>% 
  html_text()

#------------------------------------------------------
# Museovirasto's WFS API does not include all points, 
# so harvesting name and coords from the linked pages
#------------------------------------------------------
get_info <- function(url){
  title <- url %>% 
    read_html() %>%
    html_nodes(xpath = "//td[@title='kohteen nimi']") %>% 
    html_text()
  
  coords <- url %>% 
    read_html() %>%
    html_nodes(xpath = "//span[@id='koordinaatit']//td[@class='norm']") %>% 
    html_text()
  
  wgs84 <- stringr::str_extract(string = coords[1], pattern = "ETRS89/WGS84[^\r]+")
  
  df <- data.frame(title, wgs84)
  
  df2 <- df %>% 
    mutate(latraw = str_extract(wgs84, "Lat:\\s([0-9.]+)°"),
           lat = str_extract(latraw, "([0-9.]+)"),
           lonraw = str_extract(wgs84, "Lon:\\s([0-9.]+)°"),
           lon = str_extract(lonraw, "([0-9.]+)")) %>% 
    select(title, lat, lon)
  
  return(df2)
}

res <- map_df(links, get_info)

saveRDS(res, "struve_kohde_pisteet.RDS")

res_l <- data.frame(res, links)

res_sf <- st_as_sf(res_l, coords = c("lon", "lat"), crs = 4326) %>% 
  rename(Kohde = title,
         Linkki = links)

#-------------------------------------------------
# Arc points with the status of World Heritage
#-------------------------------------------------
baseurl <- "https://geoserver.museovirasto.fi/geoserver/rajapinta_suojellut/wfs?version=1.0.0&request=GetFeature"
type <- "rajapinta_suojellut:maailmanperinto_piste"
request <- paste0(baseurl, "&typeName=", type)
str <- st_read(request, stringsAsFactors = FALSE) %>% 
  st_as_sf(str, coords = c("lon", "lat"))

st_crs(str) <- 3067
points_wh <- str %>%
  st_transform(4326) 

#- Make the radius of the circle on the map reflect the WH status
map_data <- res_sf %>% 
  mutate(Suojeltu = ifelse(Kohde == "Porlom (Porlammi)", 7,
                     ifelse(Kohde == "Stuorrahanoaivi (Stuor-oivi)", 7, 
                            ifelse(Kohde == "Tornea (Alatornio)", 7,
                                   ifelse(Kohde == "Aavasaksa (Avasaksa)", 7,
                                          ifelse(Kohde == "Puolakka (Oravivuori)", 7,
                                                 ifelse(Kohde == "Svartvira (Mustaviiri)", 6, 3)))))))

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-70%,20%);
    position: fixed !important;
    left: 30%;
    padding-left: 10px; 
    padding-right: 10px; 
    font-style: 'Tahoma', sans-serif;
    font-size: 18px;
  }
  "))
  
title <- tags$div(
  tag.map.title, HTML("Struven ketju Suomessa</br>Isommat ympyrät maailmanperintökohteita")
) 

leaflet(map_data) %>% 
  addTiles(attribution = 'MML | Museovirasto | Tuija Sonkkila') %>%
  addCircleMarkers(
    radius = ~Suojeltu,
    stroke = TRUE, weight = 2, color = "black", fillOpacity = 0.5, fillColor = "orange",
    popup =~paste(Kohde,"<br>","<a href =\"",Linkki, "\", target=\"_blank\">Museovirasto</a>"),
    popupOptions = popupOptions(textsize = "12px",
                                direction = "auto")
  ) %>% 
  addControl(title, position = "topleft", className="map-title") 
  


