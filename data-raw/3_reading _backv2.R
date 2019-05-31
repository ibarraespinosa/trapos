setwd("/home/sergio/INVENTARIOS/SP_veh")
library(sf)
library(data.table)
library(units)
# llamando datos para corregir sesgo espacial

hora <- as.list(readRDS("dados/flow/hora.rds")) #UTC 2014-10-05_00 2014-10-11_00
hora2 <- as.POSIXct(readRDS("dados/flow/hora.rds"),
		    tz = "UTC",
	            format = "%Y-%m-%d_%H")
attr(hora2, "tzone") <- "America/Sao_Paulo"
hora3 <- strftime(hora2, format = "%Y-%m-%d_%H")
# Corregir datos
pts <- readRDS("~/Dropbox/count/df.rds")

# red
net <- st_transform(st_read("shapefiles/roads.shp"), crs = 31983)
net <- net[net$highway != "road" & net$highway != "residential", ]
regiones
regiones <- st_read("/home/sergio/INVENTARIOS/PHD/shapefiles/regiones.shp")
regiones <- st_transform(regiones, 31983)
 net <- st_intersection(net, regiones)
net$lengthm <- st_length(
  st_cast(
    net[st_dimension(net) == 1,]
  )
)
net$LKM <- set_units(net$lengthm, km)

for (i in 1:144) {
vel <- as.list(
  list.files(path = "dados/flow", 
             pattern = glob2rx(paste0("fluxo*",hora[[i]],"*")),
             full.names = T)
)


vel1 <- data.table(do.call("rbind",  lapply(vel, readRDS)))

#Velocida por todos los vehiculos
speed <- vel1[ , .(mean(VelTipoMean, na.rm = T),   
		   median(VelTipoMedian, na.rm = T), 
		   quantile(VTq75, .75, na.rm = T), 
                   max(VMax, na.rm = T)),
              by = .(id)]

names(speed) <- c("id", "VALLmean", "VALLMedian", "VALLQ75", "VALLMax")

#Esta velocidad es mas interesante por que es por tipo de vehiculo
flow <- vel1[ , .(sum(vei, na.rm = T), 
		  mean(VelTipoMean, na.rm = T),
		  median(VelTipoMedian, na.rm = T), 
		  quantile(VTq75, .75, na.rm = T), 
                  max(VMax, na.rm = T)),,
               by = .(id, tipo)]
names(flow) <- c("id", "tipo", "vei" ,"VMedia", "VMedian", "VTQ75", "VMax")

setkey(speed, id)
setkey(flow, id)
# Agregando flow["car"] a speed
Result <- merge(speed, flow[flow$tipo == "Car", ], all.x=TRUE)
# Agregando flow["Taxi"] a speed
Result <- merge(Result, flow[flow$tipo == "Taxi", ], all.x=TRUE)
# Agregando flow["Truck"] a speed
Result <- merge(Result, flow[flow$tipo == "Truck", ], all.x=TRUE)
names(Result) <- c("id", "VALLmean", "VALLmedian","VALLQ75", "VALLMax",
"tipoCar", "Car", "VMediaCar","VMedianCar", "VQ75Car", "VMaxCar",
"tipoTaxi", "Taxi", "VMediaTaxi","VMedianTaxi", "VQ75Taxi", "VMaxTaxi",
"tipoTruck", "Truck", "VMediaTruck","VMedianTruck", "VQ75Truck", "VMaxTruck")

# filling gaps in lanes per street
aggregate(as.numeric(as.character(net$lanes)),
          by = list(net$highway), mean, na.rm = T)
net$lanes <- ifelse(
  is.na(net$lanes) & 
	net$highway == "motorway",round(2.690727),
  ifelse(
    is.na(net$lanes) & 
	net$highway == "motorway_link", round(1.299677),
    ifelse(
      is.na(net$lanes) & 
	net$highway == "trunk", round(2.995162),
      ifelse(
        is.na(net$lanes) & 
	net$highway == "trunk_link", round(1.849498),
        ifelse(
          is.na(net$lanes) & 
	net$highway == "primary", round(2.690727),
          ifelse(
            is.na(net$lanes) & 
	net$highway == "primary_link",round(1.484009),
            ifelse(
              is.na(net$lanes) & 
	net$highway == "secondary", round(2.159727),
              ifelse(
                is.na(net$lanes) & 
	net$highway == "secondary_link", round(1.415282),
                ifelse(
                  is.na(net$lanes) & 
	net$highway == "tertiary", round(1.985048),
                  ifelse(
                    is.na(net$lanes) & 
	net$highway == "tertiary_link", round(1.379562),
                    ifelse(
                      is.na(net$lanes) & 
	net$highway == "residential", round(1.792322),
                      net$lanes
                    )))))))))))

neto <- merge(net, Result, by = "id", all.x = T)
head(neto)
aggregate(neto$VALLmean , by = list(neto$highway), mean, na.rm = T)

df <- aggregate(cbind(pts$X12A89,
                      (pts$X12C782x+pts$X12C783x+pts$X12C784x),
                      pts$X12V89), 
          by = list(pts$lanes.1, pts$highway), mean)
names(df) <- c( "lanes", "ts", "PC", "Trucks","V")
df$ra <- df$PC/df$V
df$ta <- df$Trucks/df$V
print(hora[[i]])
# print(df)
# hist(neto$lanes)
table(neto$lanes)

df1 <- aggregate(cbind(pts$X12A89,
                (pts$X12C782x+pts$X12C783x+pts$X12C784x),
                pts$X12V89), 
          by = list(pts$highway), mean)
df1$tv <- df1$V2/df1$V3
df1$pcv <- df1$V1/df1$V3
print(df1)
neto$TruckCor <- ifelse(
  neto$highway == "motorway_link" | neto$highway == "motorway" |
    neto$highway == "trunk_link" | neto$highway == "trunk",
  	(123.60417/55.93750)*neto$VMediaTruck, 
		ifelse(
 		 neto$highway == "secondary" | neto$highway == "secondary_link",
 		 (2036.857/40.28571)*neto$VMediaTruck,
			ifelse(
  			neto$highway == "tertiary" | neto$highway == "tertiary_link",
 			 (1202.000/20.00000)*neto$VMediaTruck,
			neto$Truck)))

### Velocidad Corregida #### 
# Tengo muchos tipos de velocidad
# corregire por los variso tipos y despues comparare para ver
# que correcion queda mejor dependiendo de la velocidad
# inicialmente, correcion de Cars usando velocidad de Taxi deberia funcionar
# Sin embargo, cual de ellas es la pregunta

#  CarCorVMediaTaxi ####
neto$CarCorVMediaTaxi <- ifelse(
  # Motorway  and Trunk ####
  # No tengo datos de motoray, entonces, Trunk se aplica a motorway tambien
  (neto$highway == "trunk" |
     neto$highway == "trunk_link" |
     neto$highway == "motorway" |
     neto$highway == "motorway_link" ) &
    neto$lanes >=10,
  (9615.500/69.15000)*neto$VMediaTaxi,
  ifelse(
    (neto$highway == "trunk" |
       neto$highway == "trunk_link" |
       neto$highway == "motorway" |
       neto$highway == "motorway_link" ) &
      neto$lanes < 10 & neto$lanes >=8,
    (4709.071/60.62143)*neto$VMediaTaxi,
    ifelse(
      (neto$highway == "trunk" |
         neto$highway == "trunk_link" |
         neto$highway == "motorway" |
         neto$highway == "motorway_link" ) &
        neto$lanes < 8 & neto$lanes >=6,
      (4416.850/54.01000)*neto$VMediaTaxi,
      ifelse(
        (neto$highway == "trunk" |
           neto$highway == "trunk_link" |
           neto$highway == "motorway" |
           neto$highway == "motorway_link" ) &
          neto$lanes < 6 & neto$lanes >=4,
        (3282.500/51.53333)*neto$VMediaTaxi,
        ifelse(
          (neto$highway == "trunk" |
             neto$highway == "trunk_link" |
             neto$highway == "motorway" |
             neto$highway == "motorway_link" ) &
            neto$lanes < 4,
          (786.500/16.00000)*neto$VMediaTaxi,
          # Tertiary ####
          # los valores de CET con terciary estan para 6 y 3 pistas
          # Es mejor tomar un valor medio, siendo
          ifelse(
            neto$highway == "tertiary" |
              neto$highway == "tertiary_link"  &
              neto$lanes >= 4,
            (1600.000/22.00000)*neto$VMediaTaxi,
            ifelse(
              neto$highway == "tertiary" |
                neto$highway == "tertiary_link"  &
                neto$lanes < 4,
              (804.000/15.90000)*neto$VMediaTaxi,
              # secondary ####
              ifelse(
                neto$highway == "secondary" |
                  neto$highway == "secondary_link" &
                  neto$lanes >= 8,
                (1671.000/26.00000)*neto$VMediaTaxi,
                ifelse(
                  neto$highway == "secondary" |
                    neto$highway == "secondary_link" &
                    neto$lanes >= 6 & neto$lanes < 8 ,
                  (2388.000/36.64286 )*neto$VMediaTaxi,
                  ifelse(
                    neto$highway == "secondary" |
                      neto$highway == "secondary_link" &
                      neto$lanes == 5,
                    (1295.000/28.90000)*neto$VMediaTaxi,
                    ifelse(
                      neto$highway == "secondary" |
                        neto$highway == "secondary_link" &
                        neto$lanes == 4,
                      (2527.000/26.43333)*neto$VMediaTaxi,
                      ifelse(
                        neto$highway == "secondary" |
                          neto$highway == "secondary_link" &
                          neto$lanes < 4,
                        (626.500/27.50000)*neto$VMediaTaxi,
                        # primary ####
                        ifelse(
                          neto$highway == "primary" |
                            neto$highway == "primary_link" &
                            neto$lanes >= 8,
                          (4486.500/61.68750)*neto$VMediaTaxi,
                          ifelse(
                            neto$highway == "primary" |
                              neto$highway == "primary_link" &
                              neto$lanes == 7,
                            (2664.000/30.80000)*neto$VMediaTaxi,
                            ifelse(
                              neto$highway == "primary" |
                                neto$highway == "primary_link" &
                                neto$lanes == 6,
                              (3233.250/37.30357)*neto$VMediaTaxi,
                              ifelse(
                                neto$highway == "primary" |
                                  neto$highway == "primary_link" &
                                  neto$lanes < 6 & neto$lanes >= 4,
                                (3311.200/35.88000)*neto$VMediaTaxi,
                                ifelse(
                                  neto$highway == "primary" |
                                    neto$highway == "primary_link" &
                                    neto$lanes < 4,
                                  (2697.000/29.50000)*neto$VMediaTaxi,
                                  0
                                ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ))


neto$CarCorVMediaTaxi <- ifelse(
  neto$highway == "motorway_link" |
    neto$highway == "trunk_link" |
    neto$highway == "primary_link" |
    neto$highway == "secondary_link" |
    neto$highway == "tertiary_link", neto$CarCorVMediaTaxi/2,
  neto$CarCorVMediaTaxi
)
print(sum(neto$CarCorVMediaTaxi, na.rm = T)/1000000)

#  VMedianTaxi ####
neto$CarCorVMedianTaxi <- ifelse(
  # Motorway  and Trunk ####
  # No tengo datos de motoray, entonces, Trunk se aplica a motorway tambien
  (neto$highway == "trunk" |
     neto$highway == "trunk_link" |
     neto$highway == "motorway" |
     neto$highway == "motorway_link" ) &
    neto$lanes >=10,
  (9615.500/69.15000)*neto$VMedianTaxi,
  ifelse(
    (neto$highway == "trunk" |
       neto$highway == "trunk_link" |
       neto$highway == "motorway" |
       neto$highway == "motorway_link" ) &
      neto$lanes < 10 & neto$lanes >=8,
    (4709.071/60.62143)*neto$VMedianTaxi,
    ifelse(
      (neto$highway == "trunk" |
         neto$highway == "trunk_link" |
         neto$highway == "motorway" |
         neto$highway == "motorway_link" ) &
        neto$lanes < 8 & neto$lanes >=6,
      (4416.850/54.01000)*neto$VMedianTaxi,
      ifelse(
        (neto$highway == "trunk" |
           neto$highway == "trunk_link" |
           neto$highway == "motorway" |
           neto$highway == "motorway_link" ) &
          neto$lanes < 6 & neto$lanes >=4,
        (3282.500/51.53333)*neto$VMedianTaxi,
        ifelse(
          (neto$highway == "trunk" |
             neto$highway == "trunk_link" |
             neto$highway == "motorway" |
             neto$highway == "motorway_link" ) &
            neto$lanes < 4,
          (786.500/16.00000)*neto$VMedianTaxi,
          # Tertiary ####
          # los valores de CET con terciary estan para 6 y 3 pistas
          # Es mejor tomar un valor medio, siendo
          ifelse(
            neto$highway == "tertiary" |
              neto$highway == "tertiary_link"  &
              neto$lanes >= 4,
            (1600.000/22.00000)*neto$VMedianTaxi,
            ifelse(
              neto$highway == "tertiary" |
                neto$highway == "tertiary_link"  &
                neto$lanes < 4,
              (804.000/15.90000)*neto$VMedianTaxi,
              # secondary ####
              ifelse(
                neto$highway == "secondary" |
                  neto$highway == "secondary_link" &
                  neto$lanes >= 8,
                (1671.000/26.00000)*neto$VMedianTaxi,
                ifelse(
                  neto$highway == "secondary" |
                    neto$highway == "secondary_link" &
                    neto$lanes >= 6 & neto$lanes < 8 ,
                  (2388.000/36.64286 )*neto$VMedianTaxi,
                  ifelse(
                    neto$highway == "secondary" |
                      neto$highway == "secondary_link" &
                      neto$lanes == 5,
                    (1295.000/28.90000)*neto$VMedianTaxi,
                    ifelse(
                      neto$highway == "secondary" |
                        neto$highway == "secondary_link" &
                        neto$lanes == 4,
                      (2527.000/26.43333)*neto$VMedianTaxi,
                      ifelse(
                        neto$highway == "secondary" |
                          neto$highway == "secondary_link" &
                          neto$lanes < 4,
                        (626.500/27.50000)*neto$VMedianTaxi,
                        # primary ####
                        ifelse(
                          neto$highway == "primary" |
                            neto$highway == "primary_link" &
                            neto$lanes >= 8,
                          (4486.500/61.68750)*neto$VMedianTaxi,
                          ifelse(
                            neto$highway == "primary" |
                              neto$highway == "primary_link" &
                              neto$lanes == 7,
                            (2664.000/30.80000)*neto$VMedianTaxi,
                            ifelse(
                              neto$highway == "primary" |
                                neto$highway == "primary_link" &
                                neto$lanes == 6,
                              (3233.250/37.30357)*neto$VMedianTaxi,
                              ifelse(
                                neto$highway == "primary" |
                                  neto$highway == "primary_link" &
                                  neto$lanes < 6 & neto$lanes >= 4,
                                (3311.200/35.88000)*neto$VMedianTaxi,
                                ifelse(
                                  neto$highway == "primary" |
                                    neto$highway == "primary_link" &
                                    neto$lanes < 4,
                                  (2697.000/29.50000)*neto$VMedianTaxi,
                                  0
                                ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ))


neto$CarCorVMedianTaxi <- ifelse(
  neto$highway == "motorway_link" |
    neto$highway == "trunk_link" |
    neto$highway == "primary_link" |
    neto$highway == "secondary_link" |
    neto$highway == "tertiary_link", neto$CarCorVMedianTaxi/2,
  neto$CarCorVMedianTaxi
)
print(sum(neto$CarCorVMedianTaxi, na.rm = T)/1000000)

#  VQ75Taxi ####
neto$CarCorVQ75Taxi <- ifelse(
  # Motorway  and Trunk ####
  # No tengo datos de motoray, entonces, Trunk se aplica a motorway tambien
  (neto$highway == "trunk" |
     neto$highway == "trunk_link" |
     neto$highway == "motorway" |
     neto$highway == "motorway_link" ) &
    neto$lanes >=10,
  (9615.500/69.15000)*neto$VQ75Taxi,
  ifelse(
    (neto$highway == "trunk" |
       neto$highway == "trunk_link" |
       neto$highway == "motorway" |
       neto$highway == "motorway_link" ) &
      neto$lanes < 10 & neto$lanes >=8,
    (4709.071/60.62143)*neto$VQ75Taxi,
    ifelse(
      (neto$highway == "trunk" |
         neto$highway == "trunk_link" |
         neto$highway == "motorway" |
         neto$highway == "motorway_link" ) &
        neto$lanes < 8 & neto$lanes >=6,
      (4416.850/54.01000)*neto$VQ75Taxi,
      ifelse(
        (neto$highway == "trunk" |
           neto$highway == "trunk_link" |
           neto$highway == "motorway" |
           neto$highway == "motorway_link" ) &
          neto$lanes < 6 & neto$lanes >=4,
        (3282.500/51.53333)*neto$VQ75Taxi,
        ifelse(
          (neto$highway == "trunk" |
             neto$highway == "trunk_link" |
             neto$highway == "motorway" |
             neto$highway == "motorway_link" ) &
            neto$lanes < 4,
          (786.500/16.00000)*neto$VQ75Taxi,
          # Tertiary ####
          # los valores de CET con terciary estan para 6 y 3 pistas
          # Es mejor tomar un valor medio, siendo
          ifelse(
            neto$highway == "tertiary" |
              neto$highway == "tertiary_link"  &
              neto$lanes >= 4,
            (1600.000/22.00000)*neto$VQ75Taxi,
            ifelse(
              neto$highway == "tertiary" |
                neto$highway == "tertiary_link"  &
                neto$lanes < 4,
              (804.000/15.90000)*neto$VQ75Taxi,
              # secondary ####
              ifelse(
                neto$highway == "secondary" |
                  neto$highway == "secondary_link" &
                  neto$lanes >= 8,
                (1671.000/26.00000)*neto$VQ75Taxi,
                ifelse(
                  neto$highway == "secondary" |
                    neto$highway == "secondary_link" &
                    neto$lanes >= 6 & neto$lanes < 8 ,
                  (2388.000/36.64286 )*neto$VQ75Taxi,
                  ifelse(
                    neto$highway == "secondary" |
                      neto$highway == "secondary_link" &
                      neto$lanes == 5,
                    (1295.000/28.90000)*neto$VQ75Taxi,
                    ifelse(
                      neto$highway == "secondary" |
                        neto$highway == "secondary_link" &
                        neto$lanes == 4,
                      (2527.000/26.43333)*neto$VQ75Taxi,
                      ifelse(
                        neto$highway == "secondary" |
                          neto$highway == "secondary_link" &
                          neto$lanes < 4,
                        (626.500/27.50000)*neto$VQ75Taxi,
                        # primary ####
                        ifelse(
                          neto$highway == "primary" |
                            neto$highway == "primary_link" &
                            neto$lanes >= 8,
                          (4486.500/61.68750)*neto$VQ75Taxi,
                          ifelse(
                            neto$highway == "primary" |
                              neto$highway == "primary_link" &
                              neto$lanes == 7,
                            (2664.000/30.80000)*neto$VQ75Taxi,
                            ifelse(
                              neto$highway == "primary" |
                                neto$highway == "primary_link" &
                                neto$lanes == 6,
                              (3233.250/37.30357)*neto$VQ75Taxi,
                              ifelse(
                                neto$highway == "primary" |
                                  neto$highway == "primary_link" &
                                  neto$lanes < 6 & neto$lanes >= 4,
                                (3311.200/35.88000)*neto$VQ75Taxi,
                                ifelse(
                                  neto$highway == "primary" |
                                    neto$highway == "primary_link" &
                                    neto$lanes < 4,
                                  (2697.000/29.50000)*neto$VQ75Taxi,
                                  0
                                ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ))


neto$CarCorVQ75Taxi <- ifelse(
  neto$highway == "motorway_link" |
    neto$highway == "trunk_link" |
    neto$highway == "primary_link" |
    neto$highway == "secondary_link" |
    neto$highway == "tertiary_link", neto$CarCorVQ75Taxi/2,
  neto$CarCorVQ75Taxi
)
print(sum(neto$CarCorVQ75Taxi, na.rm = T)/1000000)

#  CarCorVMaxTaxi ####
neto$CarCorVMaxTaxi <- ifelse(
  # Motorway  and Trunk ####
  # No tengo datos de motoray, entonces, Trunk se aplica a motorway tambien
  (neto$highway == "trunk" |
     neto$highway == "trunk_link" |
     neto$highway == "motorway" |
     neto$highway == "motorway_link" ) &
    neto$lanes >=10,
  (9615.500/69.15000)*neto$VMaxTaxi,
  ifelse(
    (neto$highway == "trunk" |
       neto$highway == "trunk_link" |
       neto$highway == "motorway" |
       neto$highway == "motorway_link" ) &
      neto$lanes < 10 & neto$lanes >=8,
    (4709.071/60.62143)*neto$VMaxTaxi,
    ifelse(
      (neto$highway == "trunk" |
         neto$highway == "trunk_link" |
         neto$highway == "motorway" |
         neto$highway == "motorway_link" ) &
        neto$lanes < 8 & neto$lanes >=6,
      (4416.850/54.01000)*neto$VMaxTaxi,
      ifelse(
        (neto$highway == "trunk" |
           neto$highway == "trunk_link" |
           neto$highway == "motorway" |
           neto$highway == "motorway_link" ) &
          neto$lanes < 6 & neto$lanes >=4,
        (3282.500/51.53333)*neto$VMaxTaxi,
        ifelse(
          (neto$highway == "trunk" |
             neto$highway == "trunk_link" |
             neto$highway == "motorway" |
             neto$highway == "motorway_link" ) &
            neto$lanes < 4,
          (786.500/16.00000)*neto$VMaxTaxi,
          # Tertiary ####
          # los valores de CET con terciary estan para 6 y 3 pistas
          # Es mejor tomar un valor medio, siendo
          ifelse(
            neto$highway == "tertiary" |
              neto$highway == "tertiary_link"  &
              neto$lanes >= 4,
            (1600.000/22.00000)*neto$VMaxTaxi,
            ifelse(
              neto$highway == "tertiary" |
                neto$highway == "tertiary_link"  &
                neto$lanes < 4,
              (804.000/15.90000)*neto$VMaxTaxi,
              # secondary ####
              ifelse(
                neto$highway == "secondary" |
                  neto$highway == "secondary_link" &
                  neto$lanes >= 8,
                (1671.000/26.00000)*neto$VMaxTaxi,
                ifelse(
                  neto$highway == "secondary" |
                    neto$highway == "secondary_link" &
                    neto$lanes >= 6 & neto$lanes < 8 ,
                  (2388.000/36.64286 )*neto$VMaxTaxi,
                  ifelse(
                    neto$highway == "secondary" |
                      neto$highway == "secondary_link" &
                      neto$lanes == 5,
                    (1295.000/28.90000)*neto$VMaxTaxi,
                    ifelse(
                      neto$highway == "secondary" |
                        neto$highway == "secondary_link" &
                        neto$lanes == 4,
                      (2527.000/26.43333)*neto$VMaxTaxi,
                      ifelse(
                        neto$highway == "secondary" |
                          neto$highway == "secondary_link" &
                          neto$lanes < 4,
                        (626.500/27.50000)*neto$VMaxTaxi,
                        # primary ####
                        ifelse(
                          neto$highway == "primary" |
                            neto$highway == "primary_link" &
                            neto$lanes >= 8,
                          (4486.500/61.68750)*neto$VMaxTaxi,
                          ifelse(
                            neto$highway == "primary" |
                              neto$highway == "primary_link" &
                              neto$lanes == 7,
                            (2664.000/30.80000)*neto$VMaxTaxi,
                            ifelse(
                              neto$highway == "primary" |
                                neto$highway == "primary_link" &
                                neto$lanes == 6,
                              (3233.250/37.30357)*neto$VMaxTaxi,
                              ifelse(
                                neto$highway == "primary" |
                                  neto$highway == "primary_link" &
                                  neto$lanes < 6 & neto$lanes >= 4,
                                (3311.200/35.88000)*neto$VMaxTaxi,
                                ifelse(
                                  neto$highway == "primary" |
                                    neto$highway == "primary_link" &
                                    neto$lanes < 4,
                                  (2697.000/29.50000)*neto$VMaxTaxi,
                                  0
                                ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ))



neto$CarCorVMaxTaxi <- ifelse(
  neto$highway == "motorway_link" |
    neto$highway == "trunk_link" |
    neto$highway == "primary_link" |
    neto$highway == "secondary_link" |
    neto$highway == "tertiary_link", neto$CarCorVMaxTaxi/2,
  neto$CarCorVMaxTaxi
)
print(sum(neto$CarCorVMaxTaxi, na.rm = T)/1000000)

# Guardar RDS ####
neto$hora <- hora3[i]
saveRDS(neto,  #original
        paste0("RDS/VCOR/","flow_", hora[i], ".rds") 
               )
#rm(neto)
print(hora[i])
print(hora3[i])
}


