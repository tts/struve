library(sf)
library(tidyverse)
library(rvest)

# Struve geodesic arc points in Finland
mml <- "https://www.maanmittauslaitos.fi/sites/maanmittauslaitos.fi/files/attachments/2021/06/Struvenpisteet.pdf"
download.file(mml, 'points.pdf', mode="wb")

# Firefox dev tools > Inspector. Copy all innerHTML, add root and save
raw <- read_html("page.html")

links <- raw %>% 
  html_nodes(xpath = "//a[starts-with(@id, 'pdfjs_internal_id_')]/@href") %>% 
  html_text()

# Museovirasto's WFS API does not include these, so harvesting name and coords from the linked pages instead
get_info <- function(url){
  title <- url %>% 
    read_html() %>%
    html_nodes(xpath = "//td[@title='kohteen sijaintikunta']") %>% 
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

saveRDS(res, "struve_pisteet.RDS")

res_sf <- st_as_sf(res, coords = c("lon", "lat"), crs = 4326)
