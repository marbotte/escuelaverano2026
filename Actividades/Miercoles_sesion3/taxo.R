



# paquetes ----
if(!require(taxize)){install.packages("taxize");library(taxize)}
if(!require(openxlsx)){install.packages("openxlsx");library(openxlsx)}

# Ejemplo especie simple -----
res1<-get_gbifid("Copiapoa")
res2<-get_gbifid("Copiapoaa")

# Desde archivo ----
file <- "./corregir_taxa.xlsx"
tabTax <- read.xlsx(file)
tabTax$Taxon
# get_gbifid_ busca los nombres y envia los resultados en una lista de tablas (1 tabla por taxon)
busquedaGbif <- get_gbifid_(tabTax$Taxon)
# extraer la primera tabla de resultados
busquedaGbif[[1]]
#extraer el usagekey del primer elemento de la primera tabla de resultado (primer nombre buscado)
busquedaGbif[[1]]$usagekey[1]
busquedaGbif[[1]][1,"usagekey"]
# sacar la clasificación completa desde 1 usagekey
classification(as.gbifid(busquedaGbif[[1]]$usagekey[1]),"gbif")

# cuantas propuestas por nombre (numero de filas de las tablas)
sapply(busquedaGbif,nrow)

# cuantas son exactas
exact<-logical(length(busquedaGbif))
for(i in 1:length(busquedaGbif))
{
  if(nrow(busquedaGbif[[i]])==0)
    {exact[i]<-NA
    }else{
      exact[i]<-busquedaGbif[[i]]$matchtype[1]=="EXACT"}
}

# canonicalname de resultado
result1Gbif<-sapply(busquedaGbif,function(x)
{
 if(nrow(x)==0){return(NA)}
  return(x$canonicalname[1])
})
cbind(tabTax,result1Gbif)

#
busquedaGbif<-busquedaGbif[sapply(busquedaGbif,nrow)>0]
columns<-data.frame(
colname=unlist(lapply(busquedaGbif,colnames)),
elt=rep(1:length(busquedaGbif),sapply(busquedaGbif,ncol))
)

rs<-rowSums(as.matrix(table(columns)))
colOK<-names(rs)[rs==50]
all(sapply(busquedaGbif,function(x,n)all(n%in%colnames(x)),n=colOK))
busquedaGbif<-lapply(busquedaGbif,function(x,n)x[,n],n=colOK)
tablaCompleta<-
  data.frame(
  nombreEnviado=rep(names(busquedaGbif),sapply(busquedaGbif,nrow)),
  Reduce(rbind,busquedaGbif)
)

tablaCompleta[!duplicated(tablaCompleta$nombreEnviado),]

tablaSinSino<-tablaCompleta[tablaCompleta$status=="ACCEPTED",]
final<-tablaSinSino[!duplicated(tablaSinSino$nombreEnviado),]
tabTax[!tabTax$Taxon%in%final$nombreEnviado,]


table(exact,tabTax$Correcto,useNA="ifany")


# classification ----

classif <- classification(final$usagekey,"gbif")

ranks<-unique(unlist(lapply(classif,function(x)x$rank)))
acceptedNames<-final$canonicalname
taxoMat<-matrix(NA,nrow=length(acceptedNames),ncol=length(ranks),dimnames=list(acceptedNames,ranks))
for(i in 1:length(classif)){
  ROW<-classif[[i]][nrow(classif[[i]]),"name"]
  for(j in 1:nrow(classif[[i]])){
    taxoMat[ROW,classif[[i]][j,"rank"]]<-classif[[i]][j,"name"]
}}
names(classif)
