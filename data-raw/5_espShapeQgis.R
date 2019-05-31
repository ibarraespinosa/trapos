# eporting shapefile to qgis
setwd("/home/sergio/INVENTARIOS/SP_veh")
library(sf)
library(data.table)

lista <- as.list(list.files(path = "dados/speed", pattern = ".csv", 
                            full.names = T))
#f1 <- do.call("rbind",  lapply(lista[1:5], fread))
#f1 <- do.call("rbind",  lapply(lista[6:10], fread))
#f1 <- do.call("rbind",  lapply(lista[11:15], fread))
#f1 <- do.call("rbind",  lapply(lista[16:20], fread))
#f1 <- do.call("rbind",  lapply(lista[21:25], fread))
#f1 <- do.call("rbind",  lapply(lista[26:30], fread))
#f1 <- do.call("rbind",  lapply(lista[31:35], fread))
#f1 <- do.call("rbind",  lapply(lista[36:40], fread))
#f1 <- do.call("rbind",  lapply(lista[41:45], fread))
#f1 <- do.call("rbind",  lapply(lista[46:50], fread))

# observaciones
2084671 + 2081844 + 2081896 + 2083208 + 2082139 + 
2081309 + 2081577 + 2080248 + 2081086 + 2079088 + 
2080360 + 2079834 + 2078178 + 2083120 + 2078828 + 
2081707 + 2080193 + 2083690 + 2080659 + 2084288 + 
2080996 + 2081471 + 2080818 + 2080404 + 2081821 + 
2080322 + 2082334 + 2079395 + 2079687 + 2081224 + 
2082208 + 2081839 + 2082775 + 2083164 + 2080388 + 
2083018 + 2079363 + 2083897 + 2081733 + 2078849 + 
2082156 + 2083799 + 2083520 + 2082169 + 2080888 + 
2084414 + 2077547 + 2079382 + 2083024 + 2078920

#Solo shapefile
f1 <- fread(lista[[1]])
f1car <- f1[f1$tipo == "Car" , ]
f1truck <- f1[f1$tipo == "Truck" , ]

f1car <- st_as_sf(f1car, coords = c("lon","lat"))
st_crs(f1car) <- 4326


f1truck <- st_as_sf(f1truck, coords = c("lon","lat"))
st_crs(f1truck) <- 4326

st_write(obj = f1car, layer = "gpsCAR", dsn = "shapefiles", 
	driver = "ESRI Shapefile", update = T)

st_write(obj = f1truck, layer = "gpstruck", dsn = "shapefiles", 
	driver = "ESRI Shapefile", update = T)
