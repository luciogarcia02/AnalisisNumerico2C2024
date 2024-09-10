library(sqldf)
install.packages("sqldf")
library(ggplot2)
previaje <-read.csv(file.choose(), encoding="UTF-8")

cantxprovyedicion <- sqldf("SELECT PROVINCIA_DESTINO, 
EDICION, SUM(viajes) AS CANT_VIAJES, SUM(viajeros) AS 
CANT_VIAJEROS FROM previaje
GROUP BY PROVINCIA_DESTINO, EDICION 
ORDER BY SUM(viajes) DESC LIMIT 10;")

graf_por_prov<-ggplot(data=cantxprovyedicion, 
aes(x=CANT_VIAJES, y=provincia_destino)) + 
geom_bar(stat="identity",fill="#f70388") 

vuelos<-read.csv(file.choose(),sep = ";", encoding="UTF-8")

names(vuelos)[1] <- "Fecha" 
names(vuelos)[2] <- "Hora"
names(vuelos)[3] <- "Clase"
names(vuelos)[4] <- "Clasificacion"
names(vuelos)[5] <- "Tipo_movimiento"
names(vuelos)[7] <- "Origen_destino"
names(vuelos)[8] <- "Aerolinea"
names(vuelos)[12] <- "Calidad_dato"
head(vuelos)

cant_vuelos<-sqldf("SELECT Aerolinea, count(Aerolinea) as Cantidad from vuelos group by Aerolinea HAVING Cantidad>0 order by Cantidad DESC")

cant_vuelos_profe <- sqldf("SELECT count(*) as Cantidad, 
Aerolinea
FROM vuelos where calidad_dato = 'DEFINITIVO' and 
Aerolinea <> 0 
GROUP BY Aerolinea
ORDER BY count(*) DESC limit 10;")

graf_cant_vuelos<-ggplot(data=cant_vuelos, aes(x=reorder(Aerolinea, -Cantidad), y=Cantidad,fill=Aerolinea)) + geom_bar(stat="identity") + geom_text(aes(label=Cantidad), vjust=1.8, color="black", size=3.5)
graf_cant_vuelos
