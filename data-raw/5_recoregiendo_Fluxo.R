setwd("/home/sergio/INVENTARIOS/SP_veh")
library(sf)
library(data.table)
library(units)
# llamando datos para corregir sesgo espacial

lista <- list.files(path = "RDS/VCORhora", 
	            pattern = "flow", full.names = T)
for (i in 1:168) {
neto <- readRDS(lista[i])

neto$TruckCor2 <- ifelse(
		  neto$highway == "motorway_link" |
		  neto$highway == "motorway" |
		  neto$highway == "trunk_link" |
		  neto$highway == "trunk",
		  	(123.60417/55.93750)*ifelse(is.na(neto$VMaxTruck),
				             neto$VALLMax, neto$VMaxTruck),
					neto$Truck)

neto$Car2 <- ifelse(
  # Motorway  and Trunk ####
  # No tengo datos de motoray, entonces, Trunk se aplica a motorway tambien
  (neto$highway == "trunk" |
     neto$highway == "trunk_link" |
     neto$highway == "motorway" |
     neto$highway == "motorway_link" ) &
    neto$lanes >=10,
  (9615.500/69.15000)*ifelse(is.na(neto$VMaxTaxi), 
			     neto$VALLMax, 
                             neto$VMaxTaxi),
  ifelse(
    (neto$highway == "trunk" |
       neto$highway == "trunk_link" |
       neto$highway == "motorway" |
       neto$highway == "motorway_link" ) &
      neto$lanes < 10 & neto$lanes >=8,
    (4709.071/60.62143)*ifelse(is.na(neto$VMaxTaxi), 
			       neto$VALLMax, 
                               neto$VMaxTaxi),
    ifelse(
      (neto$highway == "trunk" |
         neto$highway == "trunk_link" |
         neto$highway == "motorway" |
         neto$highway == "motorway_link" ) &
        neto$lanes < 8 & neto$lanes >=6,
      (4416.850/54.01000)*ifelse(is.na(neto$VMaxTaxi), 
				 neto$VALLMax, 
		                 neto$VMaxTaxi),
      ifelse(
        (neto$highway == "trunk" |
           neto$highway == "trunk_link" |
           neto$highway == "motorway" |
           neto$highway == "motorway_link" ) &
          neto$lanes < 6 & neto$lanes >=4,
        (3282.500/51.53333)*ifelse(is.na(neto$VMaxTaxi), 
				   neto$VALLMax, 
		                   neto$VMaxTaxi),
        ifelse(
          (neto$highway == "trunk" |
             neto$highway == "trunk_link" |
             neto$highway == "motorway" |
             neto$highway == "motorway_link" ) &
            neto$lanes < 4,
          (786.500/16.00000)*ifelse(is.na(neto$VMaxTaxi), 
				    neto$VALLMax, 
		                    neto$VMaxTaxi),
          # Tertiary ####
          # los valores de CET con terciary estan para 6 y 3 pistas
          # Es mejor tomar un valor medio, siendo
          ifelse(
            neto$highway == "tertiary" |
              neto$highway == "tertiary_link"  &
              neto$lanes >= 4,
            (1600.000/22.00000)*ifelse(is.na(neto$VMaxTaxi), 
				       neto$VALLMax, 
		                       neto$VMaxTaxi),
            ifelse(
              neto$highway == "tertiary" |
                neto$highway == "tertiary_link"  &
                neto$lanes < 4,
              (804.000/15.90000)*ifelse(is.na(neto$VMaxTaxi), 
					neto$VALLMax, 
				        neto$VMaxTaxi),
              # secondary ####
              ifelse(
                neto$highway == "secondary" |
                  neto$highway == "secondary_link" &
                  neto$lanes >= 8,
                (1671.000/26.00000)*ifelse(is.na(neto$VMaxTaxi), 
					   neto$VALLMax, 
				           neto$VMaxTaxi),
                ifelse(
                  neto$highway == "secondary" |
                    neto$highway == "secondary_link" &
                    neto$lanes >= 6 & neto$lanes < 8 ,
                  (2388.000/36.64286 )*ifelse(is.na(neto$VMaxTaxi), 
					      neto$VALLMax, 
				              neto$VMaxTaxi),
                  ifelse(
                    neto$highway == "secondary" |
                      neto$highway == "secondary_link" &
                      neto$lanes == 5,
                    (1295.000/28.90000)*ifelse(is.na(neto$VMaxTaxi), 
					       neto$VALLMax, 
				               neto$VMaxTaxi),
                    ifelse(
                      neto$highway == "secondary" |
                        neto$highway == "secondary_link" &
                        neto$lanes == 4,
                      (2527.000/26.43333)*ifelse(is.na(neto$VMaxTaxi), 
						 neto$VALLMax, 
						 neto$VMaxTaxi),
                      ifelse(
                        neto$highway == "secondary" |
                          neto$highway == "secondary_link" &
                          neto$lanes < 4,
                        (626.500/27.50000)*ifelse(is.na(neto$VMaxTaxi), 
						  neto$VALLMax, 
						  neto$VMaxTaxi),
                        # primary ####
                        ifelse(
                          neto$highway == "primary" |
                            neto$highway == "primary_link" &
                            neto$lanes >= 8,
                          (4486.500/61.68750)*ifelse(is.na(neto$VMaxTaxi), 
						      neto$VALLMax, 
						      neto$VMaxTaxi),
                          ifelse(
                            neto$highway == "primary" |
                              neto$highway == "primary_link" &
                              neto$lanes == 7,
                            (2664.000/30.80000)*ifelse(is.na(neto$VMaxTaxi), 
						       neto$VALLMax, 
						       neto$VMaxTaxi),
                            ifelse(  
                              neto$highway == "primary" |
                                neto$highway == "primary_link" &
                                neto$lanes == 6,
                              (3233.250/37.30357)*ifelse(is.na(neto$VMaxTaxi), 
						     	 neto$VALLMax, 
						   	 neto$VMaxTaxi),
                              ifelse(
                                neto$highway == "primary" |
                                  neto$highway == "primary_link" &
                                  neto$lanes < 6 & neto$lanes >= 4,
                                (3311.200/35.88000)*ifelse(is.na(neto$VMaxTaxi), 
							   neto$VALLMax, 
							   neto$VMaxTaxi),
                                ifelse(
                                  neto$highway == "primary" |
                                    neto$highway == "primary_link" &
                                    neto$lanes < 4,
                                  (2697.000/29.50000)*ifelse(is.na(neto$VMaxTaxi), 
							     neto$VALLMax, 
                     					     neto$VMaxTaxi),
                                  0
                                ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ))



neto$Car2 <- ifelse(
  neto$highway == "motorway_link" |
    neto$highway == "trunk_link" |
    neto$highway == "primary_link" |
    neto$highway == "secondary_link" |
    neto$highway == "tertiary_link", neto$CarCorVMaxTaxi/2,
  neto$Car2
)
saveRDS(neto, lista[i])
print(i)
}


lista <- list.files(path = "../SP_veh/RDS/VCORhora", 
	            pattern = "flow", full.names = T)

speedall <- list()
speedatruck <- list()

for (i in 1:168) {
neto <- readRDS(lista[i])
speedall[[i]] <- ifelse(is.na(neto$VMaxTaxi), neto$VALLMax, neto$VMaxTaxi)
speedatruck[[i]] <-  ifelse(is.na(neto$VMaxTruck), neto$VALLMax, neto$VMaxTruck)
print(i)
}

dfspeed <- as.data.frame(do.call("cbind", speedall))
dfspeedatruck <- as.data.frame(do.call("cbind", speedatruck))

carpetas <- c("campinas", "masp", "santos", "sjdc", "sorocaba")

f1 <- readRDS(lista[1])

dfspeed$region <- f1$region
dfspeedatruck$region <- f1$region

for (i in 1:length(carpetas)) {
f1x <- f1[f1$region == carpetas[i], ]
speed1 <- dfspeed[dfspeed$region == carpetas[i], ]
speedt <- dfspeed[dfspeedatruck$region == carpetas[i], ]

saveRDS(f1x,
	paste0("~/INVENTARIOS/PHD/A1/regiones/", carpetas[i], 
		"/network/flow_001_1_00.rds"))
saveRDS(speed1[,-169],
	paste0("~/INVENTARIOS/PHD/A1/regiones/", carpetas[i],
		"/network/speedall.rds"))
saveRDS(speedt[,-169],
	paste0("~/INVENTARIOS/PHD/A1/regiones/", carpetas[i],
		"/network/speedtruck.rds"))
print(carpetas[i])
}

#perfiles ####
library(ggplot2)
library(ggthemes)

car <- list()
truck <- list()

for (i in 1:168) {
neto <- readRDS(lista[i])
car[[i]] <- neto$Car2
truck[[i]] <-  neto$TruckCor2
print(i)
}

dfcar <- as.data.frame(do.call("cbind", truck))
dftruck <- as.data.frame(do.call("cbind", truck))

dfcar$region <- neto$region
dftruck$region <- neto$region
df <- rbind(dfcar, dftruck)
df1 <- data.frame(profile =  
+ c(colSums(df[df$region == "campinas", 1:168], na.rm = T)/46387449,
+ colSums(df[df$region == "masp", 1:168], na.rm = T)/114499195,
+ colSums(df[df$region == "sjdc", 1:168], na.rm = T)/10071927,
+ colSums(df[df$region == "santos", 1:168], na.rm = T)/9434083,
+ colSums(df[df$region == "sorocaba", 1:168], na.rm = T)/7736638))

head(df1)

df1$hour <- rep(1:168, each = nrow(df))
df1$region <- df$region

aggregate(df1$Profile, by = list(df1$region), sum, na.rm = T)

# campinas  46387449
#     masp 114499195
#   santos   9434083
#     sjdc  10071927
# sorocaba   7736638


ggplot(df1, aes(x = factor(hour), y = Profile, colour = region)) + geom_boxplot() + 
theme_bw()


# plot mehotds X
library(ggplot2)
f1 <- readRDS("RDS/VCORhora2/000mon0")
head(f1)
f1 <- f1[f1$region == "masp", ]
fbias <- f1[f1$Car > 0 & !is.na(f1$Car), "Car"] 
plot(fbias["Car"], axes = T)

fcor <- f1[f1$CarCor3 > 0 & !is.na(f1$CarCor3), "CarCor3"] 
plot(fcor["CarCor3"])

fbias$type <- "Biased"
fcor$type <- "Corrected"
names(fbias) <- c("Car", "geometry", "type")
names(fcor) <- c("Car", "geometry", "type")
df <- rbind(fbias, fcor)
box <- st_bbox(df)
box[[1]] <- 320000
box[[2]] <- 7385000
box[[3]] <- 340000
box[[4]] <- 7400000
box <- st_as_sfc(box)

df2 <- st_intersection(df, box)
plot(df2["Car"])

ggplot(df2) +
  geom_sf(aes(colour = Car))+
  facet_wrap(~type, ncol = 2) +
  scale_color_gradientn(colours = rev(cptcity::cpt("mpl_viridis"))) +
  theme_bw()

