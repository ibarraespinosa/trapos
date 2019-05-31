setwd("/home/sergio/INVENTARIOS/SP_veh")
library(sf)
library(data.table)
# llamando datos para corregir sesgo espacial
hora <- as.list(readRDS("dados/flow/hora.rds"))
# Corregir datos
pts <- readRDS("~/Dropbox/count/df.rds")

# red
net <- st_transform(st_read("shapefiles/roads.shp"), crs = 31983)
net <- net[net$highway != "road" & net$highway != "residential", ]

for (i in 1:145) {
vel <- as.list(
  list.files(path = "dados/flow", 
             pattern = glob2rx(paste0("fluxo*",hora[[i]],"*")),
             full.names = T)
)


vel1 <- data.table(do.call("rbind", 
                           lapply(vel, readRDS)))

speed <- vel1[ , .(mean(VelTipoMean, na.rm = T), 
                  mean(VelTipoMedian, na.rm = T)),
              by = .(id)]

names(speed) <- c("id", "Vmean", "Vmedian")
flow <- vel1[ , .(sum(vei, na.rm = T), mean(VelTipoMean, na.rm = T)),
               by = .(id, tipo)]
names(flow) <- c("id", "tipo", "vei" ,"Vmtipo")

setkey(speed, id)
setkey(flow, id)
Result <- merge(speed, flow[flow$tipo == "Car", ], all.x=TRUE)
Result <- merge(Result, flow[flow$tipo == "Taxi", c(1,3,4)], all.x=TRUE)
Result <- merge(Result, flow[flow$tipo == "Truck", c(1,3,4)], all.x=TRUE)
names(Result) <- c("id", "Vmean", "Vmedian", "tipo", "Car", "VmCar", 
               "Taxi", "VmTaxi", "Truck", "VmTruck")
# fixing lanes per street
aggregate(as.numeric(as.character(net$lanes)),
          by = list(net$highway), mean, na.rm = T)
net$lanes <- ifelse(
  is.na(net$lanes) & net$highway == "motorway",3, # 2.690727,
  ifelse(
    is.na(net$lanes) & net$highway == "motorway_link",1, # 1.299677,
    ifelse(
      is.na(net$lanes) & net$highway == "trunk", 3, #2.995162,
      ifelse(
        is.na(net$lanes) & net$highway == "trunk_link", 2,#1.849498,
        ifelse(
          is.na(net$lanes) & net$highway == "primary", 3, #2.690727,
          ifelse(
            is.na(net$lanes) & net$highway == "primary_link", 2 ,#1.484009
            ifelse(
              is.na(net$lanes) & net$highway == "secondary", 2, #2.159727
              ifelse(
                is.na(net$lanes) & net$highway == "secondary_link", 1, #1.415282
                ifelse(
                  is.na(net$lanes) & net$highway == "tertiary", 2, #1.985048,
                  ifelse(
                    is.na(net$lanes) & net$highway == "tertiary_link", 1, # 1.379562,
                    ifelse(
                      is.na(net$lanes) & net$highway == "residential", 2, # 1.792322,
                      net$lanes
                    )))))))))))
neto <- merge(net, Result, by = "id", all.x = T)
head(neto)
aggregate(neto$Vmean , by = list(neto$highgway), mean, rm = T)
neto$Vmean3 <- neto$Vmean*3
neto$Vmean3 <- ifelse(neto$Vmean3 > 120, 120, neto$Vmean3)
# las velocidades estan un poco bajas. Seria bueno repetir los calculos
#  de la valocidad permitiendo una velocidad mayor
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
# Velocidad calculada####
neto$CarCor <- ifelse(
  (neto$highway == "trunk" | neto$highway == "trunk_link" | 
     neto$highway == "motorway" | neto$highway == "motorway_link" ) & 
    neto$lanes >=10,
  139.05278*neto$Vmean,
  ifelse(
    (neto$highway == "trunk" | neto$highway == "trunk_link" | 
       neto$highway == "motorway" | neto$highway == "motorway_link" ) & 
      neto$lanes < 10 & neto$lanes >=8,
    77.67998*neto$Vmean,
    ifelse(
      (neto$highway == "trunk" | neto$highway == "trunk_link" | 
         neto$highway == "motorway" | neto$highway == "motorway_link" ) & 
        neto$lanes < 8 & neto$lanes >=6,
      81.77837*neto$Vmean,
      ifelse(
        (neto$highway == "trunk" | neto$highway == "trunk_link" | 
           neto$highway == "motorway" | neto$highway == "motorway_link" ) & 
          neto$lanes < 6 & neto$lanes >=4,
        63.69664*neto$Vmean,
        ifelse(
          (neto$highway == "trunk" | neto$highway == "trunk_link" | 
             neto$highway == "motorway" | neto$highway == "motorway_link" ) & 
            neto$lanes < 4,
          49.15625*neto$Vmean,

###################
          ifelse(
            neto$highway == "tertiary" |neto$highway == "tertiary_link"  & 
              neto$lanes >=6,
            72.72727*neto$Vmean,
            ifelse(
              neto$highway == "tertiary" |neto$highway == "tertiary_link"  & 
		neto$lanes <6,
              50.56604*neto$Vmean,
              ifelse(
                neto$highway == "tertiary" |neto$highway == "tertiary_link"  &  
		neto$lanes >= 8 & neto$lanes < 6,
                64.26923*neto$Vmean,
#   WTF
#   Lo que pao al final es que:
#   Mayor  que 8 es 64.26923, de 8 a 6 es 72.72727 y menor que 6  64.26923
###################
                ifelse(
                  neto$highway == "secondary" | neto$highway == "secondary_link" &
                    neto$lanes >= 6 & neto$lanes < 4,
#                    neto$lanes >= 6,
                  65.16959*neto$Vmean,
			ifelse(
			neto$highway == "secondary" | neto$highway == "secondary_link" &
                       neto$lanes >= 4 & neto$lanes < 2,
#                       neto$lanes < 6 & neto$lanes > 4,
                    95.59899*neto$Vmean,
                    ifelse(
                      neto$highway == "secondary" | neto$highway == "secondary_link" & 
                        neto$lanes < 4,
#                        neto$lanes < 4,
                      22.78182*neto$Vmean,
                      ifelse(
###################
                        neto$highway == "primary" |neto$highway == "primary_link" & 
                          neto$lanes >= 8,
                        72.72948*neto$Vmean,
                        ifelse(
                          neto$highway == "primary" |neto$highway == "primary_link" & 
                            neto$lanes == 7,
                          86.49351*neto$Vmean,
                          ifelse(
                            neto$highway == "primary" |neto$highway == "primary_link" & 
                              neto$lanes <= 6 & neto$lanes > 4,
                            86.67401*neto$Vmean,
                            ifelse(
                              neto$highway == "primary" |neto$highway == "primary_link" & 
                                neto$lanes < 6 & neto$lanes >= 4,
                              92.28540*neto$Vmean,
                              ifelse(
                                neto$highway == "primary" |neto$highway == "primary_link" & 
                                  neto$lanes < 4,
                                91.42373*neto$Vmean,
                              0  
                              ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )
neto$CarCor <- ifelse(
  neto$highway == "motorway_link" | 
    neto$highway == "trunk_link" | 
    neto$highway == "primary_link" | 
    neto$highway == "secondary_link" | 
    neto$highway == "tertiary_link", neto$CarCor/2,
  neto$CarCor    
  )
sum(neto$CarCor, na.rm = T)/1000000

df1 <- aggregate(cbind(pts$X12A89,
                (pts$X12C782x+pts$X12C783x+pts$X12C784x),
                pts$X12V89), 
          by = list(pts$highway), mean)
df1$tv <- df1$V2/df1$V3
df1$pcv <- df1$V1/df1$V3
neto$TruckCor <- ifelse(
  neto$highway == "motorway_link" | neto$highway == "motorway" |
    neto$highway == "trunk_link" | neto$highway == "trunk",
  2.2096834*neto$Vmean, neto$Truck)

### Velocidad Corregida #### NO
neto$CarCor3 <- ifelse(
  (neto$highway == "trunk" | neto$highway == "trunk_link" |
     neto$highway == "motorway" | neto$highway == "motorway_link" ) &
    neto$lanes >=10,
  139.05278*neto$Vmean3,
  ifelse(
    (neto$highway == "trunk" | neto$highway == "trunk_link" |
       neto$highway == "motorway" | neto$highway == "motorway_link" ) &
      neto$lanes < 10 & neto$lanes >=8,
    77.67998*neto$Vmean3,
    ifelse(
      (neto$highway == "trunk" | neto$highway == "trunk_link" |
         neto$highway == "motorway" | neto$highway == "motorway_link" ) &
        neto$lanes < 8 & neto$lanes >=6,
      81.77837*neto$Vmean3,
      ifelse(
        (neto$highway == "trunk" | neto$highway == "trunk_link" |
           neto$highway == "motorway" | neto$highway == "motorway_link" ) &
          neto$lanes < 6 & neto$lanes >=4,
        63.69664*neto$Vmean3,
        ifelse(
          (neto$highway == "trunk" | neto$highway == "trunk_link" |
             neto$highway == "motorway" | neto$highway == "motorway_link" ) &
            neto$lanes < 4,
          49.15625*neto$Vmean3,
          ifelse(
            neto$highway == "tertiary" |neto$highway == "tertiary_link"  &
              neto$lanes >=6,
            72.72727*neto$Vmean3,
            ifelse(
              neto$highway == "tertiary" |neto$highway == "tertiary_link"  &                 neto$lanes <6,
              50.56604*neto$Vmean3,
              ifelse(
                neto$highway == "tertiary" |neto$highway == "tertiary_link"  &                   neto$lanes >= 8 & neto$lanes < 6,
                64.26923*neto$Vmean3,
                ifelse(
                  neto$highway == "secondary" | neto$highway == "secondary_link" &
                    neto$lanes >= 6 & neto$lanes < 4,
                  65.16959*neto$Vmean3,
                  ifelse(
                    neto$highway == "secondary" | neto$highway == "secondary_link" &                       neto$lanes >= 4 & neto$lanes < 2,
                    95.59899*neto$Vmean3,
                    ifelse(
                      neto$highway == "secondary" | neto$highway == "secondary_link" &                         neto$lanes < 4,
                      22.78182*neto$Vmean3,
                      ifelse(
                        neto$highway == "primary" |neto$highway == "primary_link" &
                          neto$lanes >= 8,
                        72.72948*neto$Vmean3,
                        ifelse(
                          neto$highway == "primary" |neto$highway == "primary_link" &                             neto$lanes == 7,
                          86.49351*neto$Vmean3,
                          ifelse(
                            neto$highway == "primary" |neto$highway == "primary_link" &                               neto$lanes <= 6 & neto$lanes > 4,
                            86.67401*neto$Vmean3,
                            ifelse(
                              neto$highway == "primary" |neto$highway == "primary_link" &                                 neto$lanes < 6 & neto$lanes >= 4,
                              92.28540*neto$Vmean3,
                              ifelse(
                                neto$highway == "primary" |neto$highway == "primary_link" &                                   neto$lanes < 4,
                                91.42373*neto$Vmean3,
                                0
                              ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )
neto$CarCor3 <- ifelse(
  neto$highway == "motorway_link" |
    neto$highway == "trunk_link" |
    neto$highway == "primary_link" |
    neto$highway == "secondary_link" |
    neto$highway == "tertiary_link", neto$CarCor3/2,
  neto$CarCor3
)
sum(neto$CarCor, na.rm = T)/1000000

df1 <- aggregate(cbind(pts$X12A89,
                       (pts$X12C782x+pts$X12C783x+pts$X12C784x),
                       pts$X12V89),
                 by = list(pts$highway), mean)
df1$tv <- df1$V2/df1$V3
df1$pcv <- df1$V1/df1$V3
neto$TruckCor3 <- ifelse(
  neto$highway == "motorway_link" | neto$highway == "motorway" |
    neto$highway == "trunk_link" | neto$highway == "trunk",
  2.2096834*neto$Vmean3, neto$Truck)


# Guardar RDS ####
neto$hora <- hora[i]
#saveRDS(neto,  #original
#        paste0("RDS/VCOR/","flow_", hora[i], ".rds") 
#               )
saveRDS(neto, 
        paste0("flow_", hora[i], ".rds") 
               )
rm(neto)
print(hora[i])
}


