setwd("data-raw")
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


lista <- list.files(path = "RDS/VCORhora",
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

