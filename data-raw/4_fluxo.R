setwd("/home/sergio/INVENTARIOS/PHD/A1")
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

lista <- list.files(path = "../../SP_veh/RDS/VCOR", 
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
lista <- list.files(path = "../../SP_veh/RDS/VCOR", 
	            pattern = "flow", full.names = T)
# copiando con nombres 
#  flow _ horaconsecutiva_numerodia_hora124
file.copy(from = lista[28:195], 
	  to = paste0("../../SP_veh/RDS/VCORhora/flow_", 
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

lista <- list.files(path = "../SP_veh/RDS/VCORhora", 
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

f1 <- readRDS("../SP_veh/RDS/VCORhora/flow_001_1_00.rds")

VMediaTaxi <- st_sf(cVMediaTaxi, geometry = f1$geometry)
VMedianTaxi <- st_sf(cVMedianTaxii, geometry = f1$geometry)
VQ75Taxi <- st_sf(cVQ75Taxi, geometry = f1$geometry)
VMaxTaxi <- st_sf(cVMaxTaxi, geometry = f1$geometry)

library(ggplot2)
library(ggthemes)
# p1 ####
df <- data.frame(Speed = as.numeric(cVMediaTaxi))
df$Highway <-  f1$highway
df <- df[!is.na(df$Highway), ]
df$Highway <- ifelse(
  df$Highway == "motorway_link" |
    df$Highway == "motorway", "motorway",
  ifelse(
    df$Highway == "trunk_link" |
      df$Highway == "trunk", "trunk",
    ifelse(
      df$Highway == "primary_link" |
        df$Highway == "primary", "primary",
      ifelse(
        df$Highway == "secondary_link" |
          df$Highway == "secondary", "secondary",
        ifelse(
          df$Highway == "tertiary_link" |
            df$Highway == "tertiary", "tertiary",
            df$Highway
        )
        )
      )
    )
  )
df$Highway <- factor(x = df$Highway, 
                     levels = c("motorway", "trunk", "primary", "secondary", "tertiary"))


df$region <-  f1$region
df$id <- (1:168)
df1 <- df[df$id <=24,]

p1 <- ggplot(df1, aes(x = factor(id), y = Speed, fill = Highway)) +
  geom_boxplot() + facet_wrap(~Highway, scales = "free", nrow = 1) +
   labs(y = expression(km~h^-1), x = "Local Time")+
  scale_x_discrete(breaks = seq(0,23,5))+
  theme_tufte() + 
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

# p2 ####
df <- data.frame(Speed = as.numeric(cVMedianTaxi))
df$Highway <-  f1$highway
df <- df[!is.na(df$Highway), ]
df$Highway <- ifelse(
  df$Highway == "motorway_link" |
    df$Highway == "motorway", "motorway",
  ifelse(
    df$Highway == "trunk_link" |
      df$Highway == "trunk", "trunk",
    ifelse(
      df$Highway == "primary_link" |
        df$Highway == "primary", "primary",
      ifelse(
        df$Highway == "secondary_link" |
          df$Highway == "secondary", "secondary",
        ifelse(
          df$Highway == "tertiary_link" |
            df$Highway == "tertiary", "tertiary",
            df$Highway
        )
        )
      )
    )
  )
df$Highway <- factor(x = df$Highway, 
                     levels = c("motorway", "trunk", "primary", "secondary", "tertiary"))


df$region <-  f1$region
df$id <- (1:168)
df1 <- df[df$id <=24,]

p2 <- ggplot(df1, aes(x = factor(id), y = Speed, fill = Highway)) +
  geom_boxplot() + facet_wrap(~Highway, scales = "free", nrow = 1) +
   labs(y = expression(km~h^-1), x = "Local Time")+
  scale_x_discrete(breaks = seq(0,23,5))+
  theme_tufte() + 
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


# p3 ####
df <- data.frame(Speed = as.numeric(cVQ75Taxi))
df$Highway <-  f1$highway
df <- df[!is.na(df$Highway), ]
df$Highway <- ifelse(
  df$Highway == "motorway_link" |
    df$Highway == "motorway", "motorway",
  ifelse(
    df$Highway == "trunk_link" |
      df$Highway == "trunk", "trunk",
    ifelse(
      df$Highway == "primary_link" |
        df$Highway == "primary", "primary",
      ifelse(
        df$Highway == "secondary_link" |
          df$Highway == "secondary", "secondary",
        ifelse(
          df$Highway == "tertiary_link" |
            df$Highway == "tertiary", "tertiary",
            df$Highway
        )
        )
      )
    )
  )
df$Highway <- factor(x = df$Highway, 
                     levels = c("motorway", "trunk", "primary", "secondary", "tertiary"))


df$region <-  f1$region
df$id <- (1:168)
df1 <- df[df$id <=24,]

p3 <- ggplot(df1, aes(x = factor(id), y = Speed, fill = Highway)) +
  geom_boxplot() + facet_wrap(~Highway, scales = "free", nrow = 1) +
   labs(y = expression(km~h^-1), x = "Local Time")+
  scale_x_discrete(breaks = seq(0,23,5))+
  theme_tufte() + 
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

# p4 ####
df <- data.frame(Speed = as.numeric(cVMaxTaxi))
df$Highway <-  f1$highway
df <- df[!is.na(df$Highway), ]
df$Highway <- ifelse(
  df$Highway == "motorway_link" |
    df$Highway == "motorway", "motorway",
  ifelse(
    df$Highway == "trunk_link" |
      df$Highway == "trunk", "trunk",
    ifelse(
      df$Highway == "primary_link" |
        df$Highway == "primary", "primary",
      ifelse(
        df$Highway == "secondary_link" |
          df$Highway == "secondary", "secondary",
        ifelse(
          df$Highway == "tertiary_link" |
            df$Highway == "tertiary", "tertiary",
            df$Highway
        )
        )
      )
    )
  )
df$Highway <- factor(x = df$Highway, 
                     levels = c("motorway", "trunk", "primary", "secondary", "tertiary"))


df$region <-  f1$region
df$id <- (1:168)
df1 <- df[df$id <=24,]

p4 <- ggplot(df1, aes(x = factor(id), y = Speed, fill = Highway)) +
  geom_boxplot() + facet_wrap(~Highway, scales = "free", nrow = 1) +
   labs(y = expression(km~h^-1), x = "Local Time")+
  scale_x_discrete(breaks = seq(0,23,5))+
  theme_tufte() + 
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


png("VMEAN.png",
    width = 3000, height = 800, units = "px", pointsize = 5,
    bg = "white",  res = 300)
p1 + ggtitle("Mean speed")
dev.off()


png("VEDIAN.png",
    width = 3000, height = 800, units = "px", pointsize = 5,
    bg = "white",  res = 300)
p2 + ggtitle("Median speed")
dev.off()


png("VPERCENTIL75.png",
    width = 3000, height = 800, units = "px", pointsize = 5,
    bg = "white",  res = 300)
p3 + ggtitle("P75 speed")
dev.off()


png("VMAX.png",
    width = 3000, height = 800, units = "px", pointsize = 5,
    bg = "white",  res = 300)
p4 + ggtitle("Max speed")
dev.off()

png("street_speed.png",
    width = 3000, height = 800, units = "px", pointsize = 5,
    bg = "white",  res = 300)
p4
dev.off()

st_write(obj = st_transform(f1, 3857), driver = "ESRI Shapefile", dsn = "../shapefiles", update = T, layer = "f1hora_3857")

