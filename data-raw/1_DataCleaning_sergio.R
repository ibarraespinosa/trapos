setwd("/home/sergio/INVENTARIOS/SP_veh")
clean_raw_data <- function(input_file, output_file, timezone = "Etc/UTC", 
                           speed_limit = 110.0, acceleration_limit = 10.0) {
  library(data.table)
  library(magrittr)
  library(dplyr)
  library(geosphere)
  
  # Data Input File Format: object_id, time, latitude, longitude.
  
  # Leitura das colunas veiculo, hora_coleta, lat, lon do arquivo.
  # Se header pula a primeira linha, não pula caso contrário.
  dt_in_gps_data <- fread (
    input = input_file,
    sep = ',',
    header = TRUE,
    na.strings = "NA",
    select = c("veiculo", "tipo","hora_coleta", "lat", "lon"),
    # select = c("veiculo","hora_coleta", "lat", "lon"),
    stringsAsFactors = FALSE
  )
  
  # Prepara a coluna com a data e hora, originalmente String, da coleta corretamente no UTM -3 e no tipo de dado PosixCT.  
  date_time_utm <- as.POSIXct(dt_in_gps_data$hora_coleta, 
    format = "%Y-%m-%d %H:%M:%S", tz = "Etc/UTC" )
  # Timezone ajustado para UTM -3.
  attr(date_time_utm, "tzone") <- timezone
  
  # Anexa a coluna de tempo de coleta no formato de dado PosixCT e no fuso desejado.
  dt_in_gps_data <- data.table(dt_in_gps_data, date_time_utm)
  dt_in_gps_data <- dt_in_gps_data %>% select(veiculo, tipo, lat, lon, date_time_utm)
  # dt_in_gps_data <- dt_in_gps_data %>% select(veiculo, lat, lon, date_time_utm)
  rm(date_time_utm)
  
  #dt_in_gps_data <- dt_in_gps_data %>% mutate (
    #hora = format(date_time_utm, "%H"),
    #date = format(date_time_utm, "%d/%m/%y"))
  
  # o arrange e necessario para posterior calculo dos deltas tempo e espaço
  dt_distinct_gps_data <- dt_in_gps_data %>% 
    arrange(veiculo, date_time_utm) %>% 
    distinct(veiculo, tipo, date_time_utm, lat, lon, .keep_all = TRUE) %>%
    select(veiculo,tipo, date_time_utm, lat, lon)
  # distinct(veiculo,date_time_utm, lat, lon, .keep_all = TRUE) %>% 
  #   select(veiculo, date_time_utm, lat, lon)
  
  rm(dt_in_gps_data)

  # A partir daqui somente se parametrizado para fazer o cleaning pela velocidade
  # Calcula o delta tempo entre as coletas de cada veículo, no caso deve-se escolher nas repetições 
  # de lat-lon os que
  dt_lag_gps_data <- dt_distinct_gps_data %>% 
    group_by(veiculo) %>% 
    mutate(previous_time=lag(date_time_utm), 
           previous_lat=lag(lat), 
           previous_lon=lag(lon))
  
  rm(dt_distinct_gps_data)
  
  dt_gps_data <- dt_lag_gps_data %>% 
    mutate(delta_time = as.numeric(difftime(date_time_utm, 
                                            previous_time, 
                                            units="secs")))
  
  rm(dt_lag_gps_data)
  
  # Cria dataframe vazio para receber as distâncias percorridas entre pontos.
  dt_delta_space <- data.frame(delta_space = numeric())
  
  x <- 1
  f1 <- function(lat, lon, lag_lat, lag_lon) {
    function() {
      print(x)
      x <<- x + 1
      if (is.na(lag_lat)) {
        NA
      }
      else {
        distHaversine (c(as.numeric(lag_lat), as.numeric(lag_lon)), c(as.numeric(lat),as.numeric(lon)))
      }
    }
  }
  
  # Calcula a distância percorrida
  # Utiliza o apply que é mais rápido que loop no R.
  delta_space <- apply( dt_gps_data,1, 
                       function(dt_gps_data) f1(dt_gps_data['lat'],
                                                dt_gps_data['lon'],
                                                dt_gps_data['previous_lat'],
                                                dt_gps_data['previous_lon'])())
  dt_delta_space <- data.frame(delta_space)
  rm(delta_space)
  rm(x)
  
  # Ajusta o nome do dataframe com o espaço percorrido.
  colnames(dt_delta_space) <- c("delta_space")
  
  dt_gps_data <- data.table(dt_gps_data, dt_delta_space)
  
  rm(dt_delta_space)
  
  # Calcula a velocidade média que o objeto percorreu entre dois pontos em km/h.
  dt_gps_data <- dt_gps_data %>%   
    mutate(speed = 3.6*(delta_space/delta_time))
  
  # Se aceleração ativa
  # Variação de velocidade entre dois pontos.
  dt_gps_data <- dt_gps_data %>%   
    mutate(delta_speed = speed - lag(speed))
  
  # Calcula a aceleração média estimada entre dois pontos.
  dt_gps_data<- dt_gps_data %>%   
    mutate(acceleration = delta_speed/delta_time)
  
  # Seleciona apenas veiculos com ao menos 5 pontos coletados
  dt_filtered_output <- dt_gps_data %>% 
    group_by(veiculo)%>%
    filter(n() >= 5)
  
  # Finalmente elimina dados de acordo com os parâmetros de aceleração e velocidade desejados.
  # Se nao definida
  max_accelaration = 129600
  # Se nao definido
  max_speed=110
  dt_filtered_output <- dt_filtered_output %>% 
    filter(((speed <= max_speed | is.na(speed)) & 
              (abs(acceleration) <= max_accelaration | is.na(acceleration))) |
             ((is.na(acceleration) & speed <= max_speed))) %>% 
    select(veiculo, tipo, lat, lon, date_time_utm, delta_time, delta_space, speed)
    #select(veiculo, lat, lon, date_time_utm, delta_time, delta_space, speed)
  
  rm(dt_gps_data)
  
  write.table(
    dt_filtered_output, 
    file = output_file, 
    quote = FALSE, 
    sep = ',',
    na = '', 
    col.names = TRUE,
    row.names = FALSE
  )
  # Fim DataCleaning
  ######################################################
}

# testando a funcao

# veiculos <- c('Car', 'Taxi', 'Truck')
# desc_veiculos <- c('Carros', 'Táxis', 'Caminhões')
# df_desc_veiculos <- data.frame(row.names = veiculos, name_cap = desc_veiculos)
timezone <- "America/Sao_Paulo"
speed_limit <- 110.0
acceleration_limit <- 10.0


lista <- as.list(list.files(path = "dados", pattern = ".csv", full.names = T))
nombres <- 0:49
for (i in 1:50) {
  in_file <- lista[[i]]
  out_file <- paste0('dados/speed/d',i, '.csv')
    clean_raw_data(in_file, out_file, timezone)
}
f1 <- fread(lista[[i]], h = T)
hora <- as.POSICf1$hora_coleta

