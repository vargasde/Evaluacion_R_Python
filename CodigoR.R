library(data.table)
library(dplyr)
library(ggmap)
library(ggplot2)
library(tidyverse)
library(ggmap)
library(devtools)
library(caRtociudad)

multas <- read.csv('MultasMAD.csv', sep = ',', header = T)

summary(multas)
colnames(multas)

# CAMBIO DE NOMBRE DE MES A NUMERO
multas$MES = as.character(multas$MES)
multas$MES <- gsub('ENERO', '1', multas$MES)
multas$MES <- gsub('FEBRERO', '2', multas$MES)
multas$MES <- gsub('SEPTIEMBRE', '9', multas$MES)
multas$MES <- gsub('NOVIEMBRE', '11', multas$MES)
multas$MES <- gsub('DICIEMBRE', '12', multas$MES)
multas$MES <- gsub('OCTUBRE', '10', multas$MES)
multas$MES <- gsub('SEPTIEMBRE', '9', multas$MES)
multas$MES = as.integer(multas$MES)

# MODIFICAR HORA
multas$HORA = as.character.numeric_version(multas$HORA)
multas$HORA <- chartr(".", ":", multas$HORA)
head(multas$HORA)

# UNIR FECHA Y HORA COMO CHARACTER
multas$FECHA <- paste(multas$ANIO, multas$MES, 01, sep = '-') 
multas$FECHA <- paste(multas$FECHA, multas$HORA, sep = ' ')
head(multas$FECHA)

# Convierte a DATETIME
multas$FECHA <- as.POSIXct(multas$FECHA) #TARDA APROX. 6MIN
class(multas$FECHA)
multas$HORA <- NULL

#Arreglar la CALIFICACION PARA QUE SOLO SEAN 3 CATEGORIAS: LEVE, GRAVE Y MUY GRAVE
levels(multas$CALIFICACION)
levels(multas$CALIFICACION)[levels(multas$CALIFICACION)=="GRAVE     "] <- "GRAVE"
levels(multas$CALIFICACION)[levels(multas$CALIFICACION)=="LEVE      "] <- "LEVE"
levels(multas$CALIFICACION)[levels(multas$CALIFICACION)=="MUY GRAVE "] <- "MUY GRAVE"
multas$CALIFICACION = as.character(multas$CALIFICACION)
class(multas$CALIFICACION)


#Cambio a character a LUGAR
class(multas$LUGAR)
multas$LUGAR = as.character(multas$LUGAR)

# Convertir DESCUENTO a caracter
class(multas$DESCUENTO)
multas$DESCUENTO = as.character(multas$DESCUENTO)

# ARREGLAR NOMBRES DE DENUNCIANTES
class(multas$DENUNCIANTE)
levels(multas$DENUNCIANTE)
levels(multas$DENUNCIANTE)[levels(multas$DENUNCIANTE)=="SER                 "] <- 'SER'
levels(multas$DENUNCIANTE)[levels(multas$DENUNCIANTE)=="POLICIA MUNICIPAL   "] <- 'POLICIA MUNICIPAL'
levels(multas$DENUNCIANTE)[levels(multas$DENUNCIANTE)=="SACE                "] <- 'SACE'
multas$DENUNCIANTE = as.character(multas$DENUNCIANTE)

# ARREGLAR NOMBRES DE HECHOS
class(multas$HECHO.BOL)
multas$HECHO.BOL = as.character(multas$HECHO.BOL)

#--------------------------------------------------------------------------------------------------------------------------------

# CLASIFICACION DE MULTAS
# Sacamos una tabla con un conteo de las razones de las multas, verificamos cuales son las de mayor
# repeticion y hacemos un loop especifico para cada razon de importancia

reasons <- count(multas, multas$HECHO.BOL)
colnames(reasons) <- c("Reason", 'count')

# Loop que busca palabras en cada fila y clasifica cada una segun lo que encuentra
c = 1
for (i in 1:nrow(reasons)) {
  if(print(grepl("*ALCOHOL*|*ALCOHOLEMIA*|*EMBRIAGUEZ*|*ALCOHOL*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'ALCOHOLEMIA'
  } else if(print(grepl("*CONTAMINACIÓN*|*GASES*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'VIOLAR RESTRICCIONES POR CONTAMINACION'
  } else if(print(grepl("*ESTACIONAR EN LUGAR PROHIBIDO*|*ESTACIONAR EN ZONA PROHIBIDA*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'ESTACIONAMIENTO PROHIBIDO'
  } else if(print(grepl("*ESTACIONAR*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'ESTACIONAMIENTO NEGLIGENTE'
  } else if(print(grepl("*MEDIDA*|*ABANDONA PUESTO*|*ABANDONO EL PUESTO*|*ABANDONO PUESTO*|
                        *ABANDONA EL PUESTO*|*ABANDONA EL VHO*|*ABANDONA VHO*|*ABANDONAR EL VHO*|*ABANDONAR VHO*|
                        *ABANDONAR EL PUESTO*|*ABANDONAR PUESTO*|*ABANDONA COCHE*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'ESTACIONAMIENTO NEGLIGENTE'
  } else if(print(grepl("*SENTIDO CONTRARIO*|*CONTRARIA*|*DIRECCION PROHIBIDA*|*SENTIDO PROHIBIDO*|*DIRECCIÓN PROHIBIDO*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'CIRCULACION EN SENTIDO CONTRARIO O PROHIBIDO'
  } else if(print(grepl("*VELOCIDAD*|*ACELER*|\\bMAS DE\\b|\\bCIRCULA A\\b", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'VELOCIDAD O ACELERACION INDEBIDA'
  } else if(print(grepl("*PRIORIDAD DE PASO*|*PREFERENCIA DE PASO*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'PRIORIDAD DE PASO' 
  } else if(print(grepl("*SEMAFORO ROJO*|*SEMAFORO EN ROJO*|*FASE ROJA*|*REBASAR*SEMAFOROS*|*SALTA*SEMAFORO*|*SEMAFORO*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'ILEGALIDAD EN SEMAFOROS'
  } else if(print(grepl("*REBASAR*|*ADELANTAR*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'REBASAR INDEBIDAMENTE'
  } else if(print(grepl("*SEÑAL*|*VIALES*|*CIRCULACIÓN RESERVADA*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'OMISION DE SENALIZACION VIAL O DE CONDUCCION'
  } else if(print(grepl("*OCUPANTE*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'EXCESO DE OCUPANTES'
  } else if(print(grepl("*PANTALLA*|*TELEFONO*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'USO DE DISPOSITIVO ELECTRONICO'
  } else if(print(grepl("\\bMOVIL\\b|*OTROS DISPOSITIVOS*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'USO DE DISPOSITIVO ELECTRONICO'
  } else if(print(grepl("*NIÑ@*|*MENOR*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'PELIGRO A MENORES'
  } else if(print(grepl("\\bKM\\b|\\bKMH\\b|\\bKMS\\b", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'VELOCIDAD O ACELERACION INDEBIDA'
  } else if(print(grepl("*CARGA*|*DESCARGA*|*ANIMALES*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'TRANSPORTE DE PASAJEROS, OBJETOS O ANIMALES'
  } else if(print(grepl("*CINTUR*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'CINTURON DE SEGURIDAD'
  } else if(print(grepl("*DROGA*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'DROGAS'
  } else if(print(grepl("\\bCASCO\\b", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'EQUIPO PROTECTOR'
  } else if(print(grepl("*MARCHA ATR*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'MARCHA ATRAS'
  } else if(print(grepl("*PEATONES*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'OBSTACULIZACION A PEATONES'
  } else if(print(grepl("*ZIG ZAG*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'CONDUCCION NEGLIGENTE ZIG ZAG'
  } else if(print(grepl("\\bPARAR\\b|\\bPARARSE\\b|\\bALTO\\b", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'OMISION DE SENALIZACION VIAL O DE CONDUCCION' #Indicar que esto puede ser porque para donde no debe o porque no para cuando le indican.
  } else if(print(grepl("*CAMI?N*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'EXCESO DE PESO CAMION'
  } else if(print(grepl("\\bPERSONAS\\b", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'TRANSPORTE DE PASAJEROS, OBJETOS O ANIMALES'
  } else if(print(grepl("*CARRIL*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'USO INDEBIDO DE CARRILES'
  } else if(print(grepl("*BICICLETA*|*MONOPATINES*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'OTROS BICICLETAS'
  } else if(print(grepl("*MONOPATINES*", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'OTROS MONOPATINES'
  } else if(print(grepl("*MOTOCICLETA*|*CICLOMOTOR", reasons$Reason[c]) == TRUE)) {
    reasons$Motivo[c] <- 'OTROS MOTOCICLETA Y CICLOMOTOR'
  } else {
    reasons$Motivo[c] <- 'OTROS'
  }
  c = c + 1
}

# TIPO DE VEHICULO AL QUE SE LE APLICA LA MULTA
a = 1
for (i in 1:nrow(reasons)) {
  if(print(grepl("*MONOPATIN*|\\bPATIN\\b", reasons$Reason[a]) == TRUE)) {
    reasons$Vehiculo_implicado[a] <- 'MONOPATIN, PATIN O SIMILAR'
  } else if(print(grepl("\\bDOS RUEDA\\b|\\bDOS RUEDAS\\b|\\bMOTO\\b|\\bCICLOMOTOR\\b|\\bMOTOCICLETA\\b", reasons$Reason[a]) == TRUE)) {
    reasons$Vehiculo_implicado[a] <- 'MOTOCICLETA'
  } else if(print(grepl("\\bBICI*|*CICLISTA*|*BICICLETA*", reasons$Reason[a]) == TRUE)) {
    reasons$Vehiculo_implicado[a] <- 'BICICLETA'
  } else {
    reasons$Vehiculo_implicado[a] <- 'AUTOMOTOR'
  }
  a = a + 1
}

# HACER MERGE CON EL DETALLE
colnames(reasons)[1] <- 'HECHO.BOL'
reasons$count <- NULL
multas <- merge(multas,reasons, by.x = "HECHO.BOL", by.y = "HECHO.BOL") # Tarda aprox 11min
colnames(multas)[c(17,18)] <- c('categoria', 'vehiculo')

#----------------------------------------------------------------------------------------------------------------

# MAPAS
# Para buscar las coordenadas primero sacamos una tabla con las direcciones unicas de toda la tabla original.
# Debido al tiempo y limite de consultas a la API, para el proyecto del TFM hemos segmentado las multas 'Muy Graves'. 

address <- multas %>%
  filter(CALIFICACION == 'MUY GRAVE') %>%
  select(LUGAR)

address <- distinct(address, LUGAR)
address$direccion <- paste(address$LUGAR, ', Madrid', sep = '')

# Geocoding: hacemos un loop para que ggmap haga la geolocalizacion. No ha funcionado en todas las direcciones
# ya que Google no logro encontrar todas, sin embargo, la mayoria fue geolocalizada. Debido al limite de consultas,
# para esta evaluacion el loop se ha limitado para que revise hasta la fila 50.

register_google('AIzaSyDJT1IiUfMgeHV1nDnZHo2fNVBtcdl3qqY')

for(i in 1:50) {
  result <- geocode(address$direccion[i], output = "latlona", source = "google")
  address$lon[i] <- as.numeric(result[1])
  address$lat[i] <- as.numeric(result[2])
}

rm(result)

mapcodes <- address[c(1,3,4)]
mapcodes <- distinct(mapcodes, LUGAR, .keep_all = TRUE)

# Creamos un archivo .csv para guardar la informacion, por seguridad.

# mapcodes <- read.csv('geocodegrave.csv', sep = ',', header = T)

# Merge con multas: cuando tenemos ya los geocodes hacemos un merge con la tabla de multas para que a cada fila que
# tenga esa direccion, le aplique las coordenadas. Para esta evaluacion, unicamente indicamos el codigo, ya que el
# archivo importado ya contiene estas dos columnas. El codigo de merge que utilizamos en el proyecto del TFM es:

# multas <- dplyr::left_join(multas, mapcodes, by = "LUGAR")

# --------------------------------------------------------------------------------------------------------------- 

# Arreglar orden y nombres de columnas

colnames(multas)
multas <- multas[c(16,2,3,17,1,10,11,18,9,6,7,8,14,15,4,5)]
colnames(multas) <- c('fecha', 'calificacion','lugar', 'categoria', 'descripcion',
                      'vel_limite', 'vel_circula', 'vehiculo', 'denunciante', 'monto',
                      'descuento', 'puntos', 'lon', 'lat', 'mes', 'anio')

colnames(multas) <- c('hecho', 'calificacion', 'lugar', '')

# Generar el nuevo csv de Multas

#write.csv(multas, 'MultasMadrid.csv', row.names = TRUE)

#---------------------------------------------------------------------------

GRAFICOS

# Multas por anio

t_anio <- multas['anio']
t_anio$anio = as.character(t_anio$anio)
t_anio = count(t_anio, t_anio$anio)
colnames(t_anio) <- c('anio','freq')
ggplot(t_anio,
       aes(x = anio,
           y = freq,
           label = freq)) +
  geom_bar(stat = 'identity') +
  scale_y_discrete() +
  geom_text(size = 4, vjust = 2, color = 'white')

ggplot(multas,
       aes(x = calificacion,
           y = anio,
           fill = calificacion)) +
  geom_violin(scale = 'area')

tabla_denun <- multas[c(9,16)]
tabla_denun <- melt(table(tabla_denun))
ggplot(tabla_denun,
       aes(x = anio,
           y = value,
           fill = denunciante,
           label = value)) +
  geom_bar(stat = 'identity') +
  geom_text(size = 4, vjust = -0.3, color = 'black') +
  facet_wrap(~ denunciante)

#---------------------------------------------------------------------------------

MAPAS

multas_graves <- multas[multas$calificacion == 'MUY GRAVE',]
multas_graves <- multas_graves[multas_graves$lon > -4.492209,]
multas_graves <- multas_graves[multas_graves$lon < -3.1355329,]
multas_graves <- multas_graves[c(13,14)]
multas_graves$comb <- paste(multas_graves$lon, multas_graves$lat, sep = ' ')
multas_graves <- distinct(multas_graves, comb, .keep_all = TRUE)
multas_graves <- multas_graves[c(2,1)]


# Más o menos, Madrid dentro de la M-30
so.lat <- 40.3967656
so.lon <- -3.7300284
ne.lat <- 40.4798895
ne.lon <- -3.6705881

# Los pintamos
centro <- c(so.lat + (ne.lat - so.lat)/2,
            so.lon + (ne.lon - so.lon)/2)
mapa <- caRtociudad::cartociudad_get_map(centro, 5)
ggmap(mapa) + geom_point(aes(x = lon, y = lat), data = multas_graves)
