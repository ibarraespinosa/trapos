setwd("data-raw")
library(sf)
library(units)

# La primera hor es  "2014-10-05 00:00:00" UTC
# Yo necesito una semana completa desde el 6 de Octubre 00:00 Hora local
PrimerahoraUTC <-  as.POSIXct("2014-10-05 00:00:00",
			      format = "%Y-%m-%d %H:%M:%S",
			      tz = "UTC" )
PrimerahoraUTCenBR <- PrimerahoraUTC
attr(PrimerahoraUTCenBR, "tzone") <- "America/Sao_Paulo"


PrimerahoraBR <-  as.POSIXct("2014-10-06 00:00:00",
			      format = "%Y-%m-%d %H:%M:%S",
			      tz = "America/Sao_Paulo" )

PrimerahoraBRenUTC <- PrimerahoraBR
attr(PrimerahoraBRenUTC, "tzone") <- "UTC"
# "2014-10-06 03:00:00 UTC"

# Tengo que seleccionar archivos desde flow_2014-10-06_03
# y continuar 168 horas

# la ultima hora es # 2014-10-10 23:00 UTC

ultimahoraUTC <- as.POSIXct("2014-10-10 23:00:00",
			      format = "%Y-%m-%d %H:%M:%S",
			      tz = "UTC" )

ultimahoraUTCBR <- ultimahoraUTC
attr(ultimahoraUTCBR, "tzone") <- "America/Sao_Paulo"

# Que equivale a 2014-10-10 20:00:00 -03 hora BR Viernes 8 de la noche
# Entonces, necesito completar desde esta hora en adelante para hacer una semana

# La primera hora es
PrimerahoraUTCBR <- PrimerahoraUTC
attr(PrimerahoraUTCBR, "tzone") <- "America/Sao_Paulo"

# Tengo informacion de Saturday, October 04, 2014 21:00:00
# hasta Friday, October 10, 2014 20:00:00
# complteare la información
# desde Friday, October 10, 2014 21:00:00 a Sunday, October 12, 2014 23:00:00
# con Saturday, October 04, 2014 21:00:00 a Monday, October 06, 2014 23:00:00
# 51 horas



# con file.copy copiare los archivos que ncesito para compeltar la semana
# y copiare con los nombres que necesito en UTC

# Luego copiare la semana de flujo y copaire en hora BR en VCORhora
# por tanto ten que copiar 51 arquivos desde la segunra hora UTC

lista <- list.files(path = "RDS/VCOR",
	            pattern = "flow", full.names = T)

horaUTC <- seq.POSIXt(from = PrimerahoraUTC, by = "hour", length.out = 144+51)
horaUTChora <- strftime(horaUTC,
  			format = "%Y-%m-%d_%H",
			tz = "UTC")
file.copy(from = c(lista[1:27], lista[4:27]),
	  to = paste0("../../SP_veh/RDS/VCOR/flow_",
		      horaUTChora[145:195], ".rds"), overwrite = T)

# ya tengo los archivos que necesito, pero ahora tengo que convertirlos a un
# formato mas amigable, desde 000mon00 hasta 168sun23

horaUTCBR <- horaUTC
attr(horaUTCBR, "tzone") <- "America/Sao_Paulo"


hora023 <- strftime(horaUTCBR, format("%H"), tz = "America/Sao_Paulo")
horadia <- strftime(horaUTCBR, format("%u"), tz = "America/Sao_Paulo")

#Tiene que ser

nombres <- c(paste0("00",1:9, "_",
		    horadia[28:195][1:9], "_",
		    hora023[28:195][1:9]),
	     paste0("0",  10:99, "_",
		    horadia[28:195][10:99], "_",
		    hora023[28:195][10:99]),
	     paste0(100:168, "_",
		    horadia[28:195][100:168], "_",
		    hora023[28:195][100:168])
             )
# Actualizando lista
lista <- list.files(path = "RDS/VCOR",
	            pattern = "flow", full.names = T)
# copiando con nombres
#  flow _ horaconsecutiva_numerodia_hora124
file.copy(from = lista[28:195],
	  to = paste0("RDS/VCORhora/flow_",
		      nombres, ".rds"), overwrite = T)



#### FECHAR AQUI
###########################3

# ahora tengo el flujo en 168 horas en la carpeta VCORhora

# Ahora tengo varias velocidades y varios flujos corregidos
# Tengo que ver cual es la mejor velocida dpara luego elegir
# el flujo asociado

# Velocidades:
# "VMediaTaxi" "VMedianTaxi"  "VQ75Taxi"  "VMaxTaxi" "VMediaTruck"
# Solo fataria exportar las velocidades
VMediaTaxi <- list()
VMedianTaxi <- list()
VQ75Taxi <- list()
VMaxTaxi <- list()

lista <- list.files(path = "RDS/VCORhora",
	            pattern = "flow", full.names = T)
for (i in 1:168) {
net <- readRDS(lista[i])
VMediaTaxi[[i]] <- net$VMediaTaxi
VMedianTaxi[[i]] <- net$VMedianTaxi
VQ75Taxi[[i]] <- net$VQ75Taxi
VMaxTaxi[[i]] <- net$VMaxTaxi
print(i)
}

cVMediaTaxi <- do.call("cbind", VMediaTaxi)
cVMedianTaxii <- do.call("cbind", VMedianTaxi)
cVQ75Taxi <- do.call("cbind", VQ75Taxi)
cVMaxTaxi <- do.call("cbind", VMaxTaxi)

f1 <- readRDS("RDS/VCORhora/flow_001_1_00.rds")

VMediaTaxi <- st_sf(cVMediaTaxi, geometry = f1$geometry)
VMedianTaxi <- st_sf(cVMedianTaxii, geometry = f1$geometry)
VQ75Taxi <- st_sf(cVQ75Taxi, geometry = f1$geometry)
VMaxTaxi <- st_sf(cVMaxTaxi, geometry = f1$geometry)
