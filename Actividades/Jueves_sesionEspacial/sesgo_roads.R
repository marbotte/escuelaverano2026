# script para calcular el sesgo de proximidad a las carreteras en el biobío
# Marius Bottin (mbottin@humboldt.org.co)
# 21/01/2026


# Carpetas ----
fol_Data <- "../../../data_documents/Data/"

# Paquetes ----
if(!require(sf)){install.packages(sf);library(sf)}
if(!require(osmdata)){install.packages(osmdata);library(osmdata)}

# datos de occurrencia -----
occ<-read.csv(paste(fol_Data,"registros_gbif_biobiomma.csv",sep="/"),na.strings = c("NA",""))
sf_occ <- st_as_sf(occ, coords=c("decimalLongitude", "decimalLatitude"))
st_crs(sf_occ)<-4326

# poligono del biobío ----
regiones_chile <- st_read(dsn=paste(fol_Data,"/spatial/",sep="/"),layer="REGIONES_v1")
regiones_chile<-st_transform(regiones_chile,st_crs(sf_occ))
pol_biobio<-regiones_chile[regiones_chile$REGION=="Biobío",]


# Mapa basico
plot(st_geometry(pol_biobio),col="grey")
plot(st_geometry(sf_occ),add=T,col="red",pch=3,cex=.1)

# Extraer las occurrencias en el biobío ----
inBiobio<-st_intersects(pol_biobio,sf_occ,sparse=F)[1,]
plot(st_geometry(pol_biobio),col="grey")
plot(st_geometry(sf_occ[inBiobio,]),add=T,col="red",pch=3,cex=.1)
plot(st_geometry(sf_occ[!inBiobio,]),add=T,col="blue",pch=3,cex=.1)

sf_occ_biobio<-sf_occ[inBiobio,]

# Descargar las carreteras ----

# CUIDADO ESTE CODIGO ES PESADO... 
# bb_biobio <- st_bbox(regiones_chile[regiones_chile$REGION=="Biobío",])
# OSM_biobio<-opq(bb_biobio) %>% add_osm_feature(key = "highway") %>% osmdata_sf()
# table(OSM_biobio$osm_lines$highway)
# filter<-
#   !is.na(OSM_biobio$osm_lines$highway)
# OSM_biobio$osm_lines$highway%in%c(
#   "motorway",
#   "motorway_link",
#   "primary","primary_link",
#   "secondary","secondary_link",
#   "tertiary","terciary_link")
# roads_biobio<- OSM_biobio$osm_lines[filter,c("osm_id","name","highway")]

# alternativa desde el shapefile:
roads_biobio<-st_read(dsn=paste(fol_Data,"roads",sep="/"),layer = "roads_biobio")


# mapa de las carreteras y puntos ----

#plot(st_geometry(pol_biobio),col="blue")
#plot(st_geometry(roads_biobio), col="grey", lwd=.5,add=T)
#plot(st_geometry(sf_occ_biobio), col="red",pch=3,cex=.2,add=T)

# tomar n puntos al azar en las occurrencias ----
n=20
sf_occ_biobio_npt<-sf_occ_biobio[sample(1:nrow(sf_occ_biobio),n),]

# Calcular la distancia de los n puntos a las carreteras ----
d_roads<-numeric(n)
for(i in 1:n)
{
  cat(".")
  d_roads[i]<-min(st_distance(st_geometry(sf_occ_biobio_npt[i,]),st_geometry(roads_biobio))[1,])
}
gc()
# Tomar n puntos al azar en el biobio ----
nAleatPt <- st_sample(st_geometry(pol_biobio),n)

# Calcular la distancia de los n puntos aleatorios a las carreteras ----
d_roads_aleat<-numeric(n)
for(i in 1:n)
{
  cat(".")
  d_roads_aleat[i]<-min(st_distance(st_geometry(nAleatPt[i,]),st_geometry(roads_biobio))[1,])
}
gc()



# Comparar ----
par(mfrow=c(1,2))
hist(d_roads,main="Ocurrencias reales",nclass=7,xlim=c(0,max(c(d_roads,d_roads_aleat))),ylim=c(0,12))
hist(d_roads_aleat, main="Puntos aleatorios",nclass=14,xlim=c(0,max(c(d_roads,d_roads_aleat))),ylim=c(0,12))
mean(d_roads)
mean(d_roads_aleat)
