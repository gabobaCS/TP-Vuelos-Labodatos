require(tidyverse)
require(lubridate)
df1 <- read.csv("aterrizajes-y-despegues-registrados-por-eana-2014.csv", sep=";")


df1 = df1 %>%
        mutate_if(is.character, str_trim)


nacionales = df1 %>%  
  filter(Clasificación.Vuelo == "Cabotaje")

##Limpieza de Vuelos Nacionales


nacionales = 
  nacionales %>% 
    mutate(Datetime = dmy_h(paste(Fecha, Hora))) %>% 
    relocate(Datetime) 

nacionales = nacionales[order(nacionales$Datetime),] 

filtro1 = nacionales %>% 
            filter(Origen.OACI == "SAME",
                   Destino.OACI == "SACO",
                   Aerolinea.Nombre == "Austral Líneas Aéreas",
                   Aeronave == "EMBRAER E-190")


count = 0

filtro1$Tiempo.de.Vuelo = 0

for (i in 1:nrow(filtro1)) {
  if(filtro1$Tipo.de.Movimiento[i] == "Despegue"){
    for (j in i+1:nrow(filtro1)) {
      if(filtro1$Tipo.de.Movimiento[j] == "Aterrizaje"){
        filtro1$Tiempo.de.Vuelo[i] = filtro1[j,1] - filtro1[i,1]
        print(filtro1[j,1] - filtro1[i,1])
        break
      }
    }
  }
}

nacionales$Tiempo.de.Vuelo = NA
nacionales$Aerolinea.Nombre[is.na(nacionales$Aerolinea.Nombre)] = "Privado"
nacionales$Aeronave[is.na(nacionales$Aeronave)] = "Privado"

for (i in 1:nrow(nacionales)) {
  if(nacionales$Tipo.de.Movimiento[i] == "Despegue"){
    fila = nacionales[i,]
    origen = fila["Origen.OACI"]
    destino = fila["Destino.OACI"]
    aerolinea = fila["Aerolinea.Nombre"]
    aeronave = fila["Aeronave"]
    
    for (j in i+1:nrow(nacionales)){
      if (is.na(origen)){
        print(i)
      }
      if(nacionales$Tipo.de.Movimiento[j] == "Aterrizaje" &&
         nacionales$Origen.OACI[j] == origen &&
         nacionales$Destino.OACI[j] == destino &&
         (nacionales$Aerolinea.Nombre[j] == aerolinea | 
          is.na(nacionales$Aerolinea.Nombre[j]) && is.na(aerolinea) ) &&
         nacionales$Aeronave[j] == aeronave &&
         is.na(nacionales$Tiempo.de.Vuelo[j])){
        
          nacionales$Tiempo.de.Vuelo[i] = nacionales[j,1] - nacionales[i,1]
          nacionales$Tiempo.de.Vuelo[j] = nacionales[j,1] - nacionales[i,1]
          break
      }
    }
  }
}

nacionales %>% filter(Aerolinea.Nombre == "N/A")


