setwd("data-raw")
# a) Calcular Velocidad
# b) buffer 10m en puntos
# c) intersectar buffer lineas
# NO d) Calcular LKM a red
# NO e) Calcular diferencia de tiempo por grupal ID
# f) agregar flujo length id
# g) agregar velocidad mean
library(sf)
library(data.table)
net <- st_transform(st_read("shapefiles/roads.gpkg"), crs = 31983) # open street map
# podria calcular aqui esto pero puede ser tambien en el siguiente script
# net <- net[net$highway != "road" & net$highway != "residential", ]
# regiones
# regiones <- st_read("/home/sergio/INVENTARIOS/PHD/shapefiles/regiones.shp")
# regiones <- st_transform(regiones, 31983)

# net <- st_intersection(net, regiones)
# net$lengthm <- st_length(
#   st_cast(
#     net[st_dimension(net) == 1,]
#   )
# )
# net$LKM <- set_units(net$lengthm, km)

lista <- as.list(list.files(path = "dados/speed", pattern = ".csv",
                            full.names = T))
for (i in 1:length(lista) ) {
#for (i in 1 ) {
ve1 <- fread(lista[[i]], h = T)

ve1 <- as.data.frame(ve1[!is.na(ve1$lon) & ve1$delta_time < 60*30, ])

ve1 <- st_as_sf(ve1, coords = c("lon","lat"))
st_crs(ve1) <- 4326
ve1 <- st_transform(ve1, st_crs(31983))
ve1$date_time_utm <- as.POSIXct(as.character(ve1$date_time_utm),
                                format = "%Y-%m-%d %H:%M:%S",
                                tz = "America/Sao_Paulo" )
ve1$hora <- strftime(ve1$date_time_utm,
                       format = "%Y-%m-%d_%H", tz = "UTC")

for (j in 1:length(unique(ve1$hora)) ) {
vej <-  ve1[ ve1$hora == unique(ve1$hora)[j], ]
#rm(ve1)
vej_b10m <- st_buffer(x = vej, dist = 10 )
vej <- st_intersection(x = net, y = vej_b10m)
vej <- data.table(vej)
fluxo <- vej[   , .(length(veiculo),
                    mean(speed, na.rm = T),
                    median(speed, na.rm = T),
                    quantile(speed, .75, na.rm = T),
                    quantile(speed, .85, na.rm = T),
                    quantile(speed, .95, na.rm = T),
                    max(speed, na.rm = T)),
                by = .(id, tipo, hora)]

names(fluxo) <- c("id", "tipo", "hora", "vei", "VelTipoMean", "VelTipoMedian",
                  "VTq75", "VTq85", "VTq95", "VMax" )
saveRDS(fluxo, paste0("dados/flow/fluxo_",i,"_",
			unique(ve1$hora)[j],".rds"))
# AQUI
speed <- vej[ , .(mean(speed, na.rm = T),
                  median(speed, na.rm = T),
                  quantile(speed, .75, na.rm = T),
                  quantile(speed, .85, na.rm = T),
                  quantile(speed, .95, na.rm = T),
                  max(speed, na.rm = T)),
              by = .(id, hora)]
names(speed) <- c("id", "hora","VelAllMean", "VelAllMedian",
                  "VTAllq75", "VTAllq85", "VTAllq95", "VAllMax" )

saveRDS(speed, paste0("dados/flow/speed_",i,"_",
			unique(ve1$hora)[j],".rds"))

  }

}



