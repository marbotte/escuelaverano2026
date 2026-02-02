## ----echo=F------------------------------------------------------------------------------------------------------------
fol_Datos <- "../../../data_documents/Data/"
fol_Results <- "../../../data_documents/Results/"


## ----------------------------------------------------------------------------------------------------------------------
# Aqui empiezan los codigos del ejercicio 1 sobre los filtros simples----
(load(paste0(fol_Datos,"diatomeas.RData")))


## ----------------------------------------------------------------------------------------------------------------------
fil_her <- env_info$HER1Lyon == 2 | env_info$HER1Lyon == 6 | env_info$HER1Lyon == 7

## ----------------------------------------------------------------------------------------------------------------------
fil_comp <- fil_her & reference_sites


## ----------------------------------------------------------------------------------------------------------------------
sum(fil_comp)


## ----------------------------------------------------------------------------------------------------------------------
sp_abund_mat_filtered <- sp_abund_mat[fil_comp,]


## ----------------------------------------------------------------------------------------------------------------------
tot_abund_fil <- colSums(sp_abund_mat_filtered)
tot_abund_fil[1:10]


## ----------------------------------------------------------------------------------------------------------------------
as.logical(tot_abund_fil[1:10])


## ----------------------------------------------------------------------------------------------------------------------
dim(sp_abund_mat_filtered)
sp_abund_mat_filtered <- sp_abund_mat_filtered[, as.logical(colSums(sp_abund_mat_filtered))]
dim(sp_abund_mat_filtered)


## ----------------------------------------------------------------------------------------------------------------------
colnames(sp_abund_mat_filtered)[1:10]
sort(colnames(sp_abund_mat_filtered))[1:10]
sp_abund_mat_filtered <- sp_abund_mat_filtered[,sort(colnames(sp_abund_mat_filtered))]


## ----------------------------------------------------------------------------------------------------------------------
env_info_filtered <- env_info[fil_comp,]


## ----------------------------------------------------------------------------------------------------------------------
sp_01_filtered <- sp_abund_mat_filtered
mode(sp_01_filtered) <- "logical"


## ----------------------------------------------------------------------------------------------------------------------
(riqueza <- rowSums(sp_01_filtered))


## ----------------------------------------------------------------------------------------------------------------------
(acid5pc <- quantile(env_info_filtered$PH,0.05))
(alka5pc <- quantile(env_info_filtered$PH,0.95))


## ----------------------------------------------------------------------------------------------------------------------
(riq_acid <- riqueza[env_info_filtered$PH <= acid5pc])
(riq_alka <- riqueza[env_info_filtered$PH >= alka5pc])


## ----------------------------------------------------------------------------------------------------------------------
save(list=c("sp_abund_mat_filtered","riq_acid","riq_alka"), file = paste0(fol_Results,"filteredDiatoms.RData"))


## ----------------------------------------------------------------------------------------------------------------------
(load(paste0(fol_Datos,"diatomTaxonomy.RData")))


## ----------------------------------------------------------------------------------------------------------------------
m_sp_mat_taxo <- match(colnames(sp_abund_mat),taxonomy$cd)


## ----------------------------------------------------------------------------------------------------------------------
w_fragilariaceae <- which(taxonomy$family[m_sp_mat_taxo]=="Fragilariaceae")
w_naviculaceae <- which(taxonomy$family[m_sp_mat_taxo]=="Naviculaceae")


## ----------------------------------------------------------------------------------------------------------------------
indexFraNav <- rowSums(sp_abund_mat[,w_fragilariaceae])/rowSums(sp_abund_mat[,w_naviculaceae])


## ----------------------------------------------------------------------------------------------------------------------
boxplot(indexFraNav~reference_sites)


## ----------------------------------------------------------------------------------------------------------------------
# Buscamos, para cada taxón, la familia que corresponde
family <- taxonomy$family[m_sp_mat_taxo]
# Miramos la lista de familia (unica) y excluímos los taxones que no tienen una información de familia
un_family <- na.omit(unique(family))
# Creamos la matriz
mat_diat_fam <- matrix(NA, nrow=nrow(sp_abund_mat), ncol=length(un_family),dimnames=list(NULL,un_family))
# Hacemos un bucle que toma cada familia una por una
# Para cada familia filtramos la matriz de abundancia, y calculamos la suma por fila
for(i in un_family)
{
  #Caso 1: solo hay un taxon en la familia 
  if(sum(family==i,na.rm=T)==1)
  {mat_diat_fam[,i] <- sp_abund_mat[,which(family==i)]
  }else{ # Caso 2 existe una matriz de más de una columna en esta familia
    mat_diat_fam[,i]<-rowSums(sp_abund_mat[,which(family==i)])
  }
}


## ----------------------------------------------------------------------------------------------------------------------
# Ejercicio 2 sobre la lectura de archivos complejos ----
require(openxlsx)
fFF <- loadWorkbook(paste0(fol_Datos,"fitosociologiaFagusFrancia.xlsx"))
(sheetNames<-names(fFF))


## ----------------------------------------------------------------------------------------------------------------------
rawList<-list()
rawList$Lapraz1<- read.xlsx(fFF,sheet=sheetNames[1],colNames = F, rowNames = F, skipEmptyRows=T, skipEmptyCols = T)
rawList$Lapraz2<- read.xlsx(fFF,sheet=sheetNames[2],colNames = F, rowNames = F, skipEmptyRows=T, skipEmptyCols = T)



## ----------------------------------------------------------------------------------------------------------------------
rawList[[1]]<-rawList[[1]][,-ncol(rawList[[1]])]
rawList[[2]]<-rawList[[2]][,-ncol(rawList[[2]])]


## ----------------------------------------------------------------------------------------------------------------------
(lastRowEnv_1 <- which(rawList[[1]][,1] == "Genre")-1)
(lastRowEnv_2 <- which(rawList[[2]][,1] == "Genre")-1)


## ----------------------------------------------------------------------------------------------------------------------
# Extraer las filas que corresponden, las columnas que tienen un numero de relevé,
# sacar la columna que contiene los nombres de filas, transponer lo extraído y
# pasarlos en formato data.frame
fFF_env_info1 <- data.frame(t(rawList[[1]][1:lastRowEnv_1, !is.na(rawList[[1]][1,])][,-1]))
fFF_env_info2 <- data.frame(t(rawList[[2]][1:lastRowEnv_2, !is.na(rawList[[2]][1,])][,-1]))
# Añadir los colnames
colnames(fFF_env_info1) <- rawList[[1]][1:lastRowEnv_1,1] 
colnames(fFF_env_info2) <- rawList[[2]][1:lastRowEnv_2,1] 


## ----------------------------------------------------------------------------------------------------------------------
fFF_env_info1$`Taux de carbonates`[fFF_env_info1$`Taux de carbonates` == "Traces"] <- 0
fFF_env_info1 <- type.convert(fFF_env_info1,as.is=T)
fFF_env_info2 <- type.convert(fFF_env_info2,as.is=T)


## ----------------------------------------------------------------------------------------------------------------------
fFF_env_info1$cd_rel <- paste("LZ1_",fFF_env_info1$`Numéros des relevés`,sep="")
fFF_env_info2$cd_rel <- paste("LZ2_",fFF_env_info2$`Numéros des relevés`,sep="")


## ----------------------------------------------------------------------------------------------------------------------
# Cuales son todos los nombres de columnas
totCol <- union(colnames(fFF_env_info1),colnames(fFF_env_info2))
# Cuales las columnas que faltan en la tabla1
(missingCol1 <- totCol[! totCol %in% colnames(fFF_env_info1)])
# Cuales las columnas que faltan en la tabla2
(missingCol2 <- totCol[! totCol %in% colnames(fFF_env_info2)])
# Crear las columnas faltantes y llenarlas de NA
fFF_env_info1[,missingCol1]<-NA
fFF_env_info2[,missingCol2]<-NA
# Hacer que las columnas de las dos tablas tengan el mismo orden
fFF_env_info1<-fFF_env_info1[totCol]
fFF_env_info2<-fFF_env_info2[totCol]


## ----------------------------------------------------------------------------------------------------------------------
fFF_env_info<-rbind(fFF_env_info1, fFF_env_info2)


## ----------------------------------------------------------------------------------------------------------------------
raw_mat1 <- as.matrix(t(rawList[[1]][(lastRowEnv_1+2):nrow(rawList[[1]]), 3:ncol(rawList[[1]])]))
raw_mat2 <- as.matrix(t(rawList[[2]][(lastRowEnv_2+2):nrow(rawList[[2]]), 3:ncol(rawList[[2]])]))


## ----------------------------------------------------------------------------------------------------------------------
(tax1 <- paste(
  rawList[[1]][(lastRowEnv_1+2):nrow(rawList[[1]]),1],
  rawList[[1]][(lastRowEnv_1+2):nrow(rawList[[1]]),2]
))[1:10]
(tax2 <- paste(
  rawList[[2]][(lastRowEnv_2+2):nrow(rawList[[2]]),1],
  rawList[[2]][(lastRowEnv_2+2):nrow(rawList[[2]]),2]
))[1:10]
# Los pongamos como nombre de columnas
colnames(raw_mat1) <- tax1
colnames(raw_mat2) <- tax2


## ----------------------------------------------------------------------------------------------------------------------
rownames(raw_mat1) <- fFF_env_info1$cd_rel
rownames(raw_mat2) <- fFF_env_info2$cd_rel


## ----------------------------------------------------------------------------------------------------------------------
# Lista completa de especies
totSp <- union(colnames(raw_mat1), colnames(raw_mat2))
# especies faltantes en cada matriz
missingSpMat1 <- totSp[! totSp %in% colnames(raw_mat1)]
missingSpMat2 <- totSp[! totSp %in% colnames(raw_mat2)]
# crear las matrices de NA con las especies faltantes.
# Anotar: el argumento dimnames tiene que ser una lista: el primer elemento contiene los
# nombres de filas, el segundo los nombres de columnas
matSupp1 <- matrix(NA, nrow=nrow(raw_mat1), ncol=length(missingSpMat1), 
                   dimnames=list(rownames(raw_mat1), missingSpMat1))
matSupp2 <- matrix(NA, nrow=nrow(raw_mat2), ncol=length(missingSpMat2), 
                   dimnames=list(rownames(raw_mat2), missingSpMat2))
# Ahora pegamos por columna esas matrices a las matrices originales
raw_mat1 <- cbind(raw_mat1, matSupp1)
raw_mat2 <- cbind(raw_mat2, matSupp2)
#Ponemos las columnas en el mismo orden en las 2 matrices
raw_mat1 <- raw_mat1[,totSp]
raw_mat2 <- raw_mat2[,totSp]


## ----------------------------------------------------------------------------------------------------------------------
raw_mat <- rbind(raw_mat1, raw_mat2)


## ----------------------------------------------------------------------------------------------------------------------
bbScale <-data.frame(
  code=c("r","+",as.character(1:5)),
  description= c("Menos del 1% de cobertura, 3-5 individuos", "Menos del 5% de cobertura, pocos individuos","~5% más individuos","5%-25%","25%-50%","50%-75%","75%-100%"),
  minPercent=c(0,1,5,5,25,50,75),
  maxPercent=c(1,5,5,25,50,75,100)
)
bbScale$finalVal <- (bbScale$minPercent+bbScale$maxPercent)/2


## ----------------------------------------------------------------------------------------------------------------------
m_raw_mat_code <- match(raw_mat,bbScale$code)


## ----------------------------------------------------------------------------------------------------------------------
matCobertura <- matrix(bbScale$finalVal[m_raw_mat_code],nrow=nrow(raw_mat),ncol=ncol(raw_mat), dimnames=dimnames(raw_mat))


## ----------------------------------------------------------------------------------------------------------------------
matCobertura[is.na(matCobertura)] <- 0


## ----------------------------------------------------------------------------------------------------------------------
# Crear el workbook
wbFinal <- createWorkbook()
# Crear las pestañas
addWorksheet(wbFinal,"Sampling units")
addWorksheet(wbFinal,"Cobertura")
# Poner los datos
writeData(wbFinal,"Sampling units", fFF_env_info, rowNames = F)
writeData(wbFinal,"Cobertura", matCobertura, rowNames = T)
# Exportar el archivo excel
saveWorkbook(wbFinal, file = paste(fol_Results,"fitosociologíaFagusFranciaFinal.xlsx"), overwrite = T)


## ----------------------------------------------------------------------------------------------------------------------
require(openxlsx)
fileXlsx<-loadWorkbook(paste(fol_Datos,"ORIGINAL_especiesclasificadaschile_junio2025.xlsx",sep="/"))
names(fileXlsx)
tableEspecies<-read.xlsx(paste(fol_Datos,"ORIGINAL_especiesclasificadaschile_junio2025.xlsx",sep="/"),sheet="Especies")


## ----------------------------------------------------------------------------------------------------------------------
(load(paste(fol_Datos,"bog_chinga.RData",sep="/")))
df_bog_chinga


## ----------------------------------------------------------------------------------------------------------------------
load(paste(fol_Datos,"bog_chinga.RData",sep="/"))
mat_bog_chinga
(sup0 <- which(mat_bog_chinga > 0, arr.ind = T))
mat_bog_chinga[sup0]


## ----------------------------------------------------------------------------------------------------------------------
(ROW <- unique(df_bog_chinga$UniMuestreo))
(COL <- unique(df_bog_chinga$Especie)) 


## ----------------------------------------------------------------------------------------------------------------------
(mat <- matrix(0,nrow=length(ROW),ncol=length(COL),dimnames=list(ROW,COL)))


## ----------------------------------------------------------------------------------------------------------------------
match(df_bog_chinga$UniMuestreo, rownames(mat))


## ----------------------------------------------------------------------------------------------------------------------
matRowCol<-cbind(row=match(df_bog_chinga$UniMuestreo, rownames(mat)),
      col=match(df_bog_chinga$Especie, colnames(mat))
      )


## ----------------------------------------------------------------------------------------------------------------------
mat[matRowCol]<-df_bog_chinga$abundancia
mat


## ----------------------------------------------------------------------------------------------------------------------
dbTab2mat <-
function(dbTab,col_samplingUnits="SU",col_species="sp",col_content="abundance",empty=NA,checklist=F)
{
  COLS<-unique(as.character(dbTab[,col_species]))
  ROWS<-unique(as.character(dbTab[,col_samplingUnits]))
  arr.which<-matrix(NA,ncol=2,nrow=nrow(dbTab),dimnames=list(1:nrow(dbTab),c("row","col")))
  arr.which[,1]<-match(as.character(dbTab[,col_samplingUnits]),ROWS)
  arr.which[,2]<-match(as.character(dbTab[,col_species]),COLS)
  # Esta linea es para determinar el modo de los datos, según los argumentos
  # Si checklist está verdadero, entonces el modo es TRUE/FALSE (logico), sino corresponde al modo de la columna col_content
  modeContent<-ifelse(checklist,"logical",mode(dbTab[,col_content]))
  # Ahora que tenemos el modo, entonces podemos saber con que llenar la matriz:
  if(is.na(empty)){empty<-switch(modeContent,character="",numeric=0,logical=F)}
  # Creamos la matriz
  res<-matrix(empty,ncol=length(COLS),nrow=length(ROWS),dimnames=list(ROWS,COLS))
  # La llenamos
	if(checklist){ res[arr.which]<-T}else{res[arr.which]<-dbTab[,col_content]}
  return(res)
}


## ----------------------------------------------------------------------------------------------------------------------
dbTab2mat(df_bog_chinga, col_samplingUnits = "UniMuestreo", col_species = "Especie", col_content = "abundancia")


## ----------------------------------------------------------------------------------------------------------------------
which(mat_bog_chinga>0,arr.ind =T)


## ----------------------------------------------------------------------------------------------------------------------
mat2dbTab <-
function(mat,checklist=F,col_samplingUnits="SU", col_taxon="taxon",col_content="Abundance")
{
  #busquemos los contenidos de la matriz superiores a 0
  W<-which(mat>0,arr.ind=T)
  
	if(!checklist){# Si es presencia ausencia
  dbTab<-data.frame(SU=rownames(mat)[W[,"row"]],sp=colnames(mat)[W[,"col"]],ab=mat[W])
	}else{# Si no es presencia ausencia
  dbTab<-data.frame(SU=rownames(mat)[W[,"row"]],sp=colnames(mat)[W[,"col"]])
	}
  # Reorganizamos la tabla por sitios, luego por especie
  dbTab<-dbTab[order(dbTab$SU,dbTab$sp),]
  # Ponemos los colnames
  COLNAMES <- c(col_samplingUnits,col_taxon)
  if(!checklist){COLNAMES<-c(COLNAMES,col_content)}
  colnames(dbTab)<-COLNAMES
  return(dbTab)
}



## ----------------------------------------------------------------------------------------------------------------------
mat2dbTab(mat_bog_chinga)
mat2dbTab(mat_bog_chinga,checklist = T)


## ----------------------------------------------------------------------------------------------------------------------
(df_bog_chinga<-rbind.data.frame(df_bog_chinga, 
                                data.frame(UniMuestreo="Bogotá",
                                           Especie="Felis catus",
                                           abundancia=50
                                )))


## ----------------------------------------------------------------------------------------------------------------------
resultSUM<-by(df_bog_chinga,list(df_bog_chinga$UniMuestreo,df_bog_chinga$Especie),function(x){
  x$abundancia<-sum(x$abundancia)
  return(unique(x))
})


## ----------------------------------------------------------------------------------------------------------------------
(gatosMas <- Reduce(rbind,resultSUM))


## ----------------------------------------------------------------------------------------------------------------------
sumRepeated<- function(dbTab,col_sampUnit ="SU",col_taxon="taxon",col_content="Abundance")
{
  resSum<-by(dbTab,list(dbTab[,col_sampUnit],dbTab[,col_taxon]),function(x,col_cont)
  {
    x[,col_cont]<-sum(x[,col_cont])
    return(unique(x))
  },col_cont=col_content)
  return(Reduce(rbind,resSum))
}


## ----------------------------------------------------------------------------------------------------------------------
sumRepeated(df_bog_chinga, col_sampUnit = "UniMuestreo", col_taxon="Especie", col_content = "abundancia")


## ----------------------------------------------------------------------------------------------------------------------
dbTabDiat <- mat2dbTab(sp_abund_mat)
head(dbTabDiat)


## ----------------------------------------------------------------------------------------------------------------------
dbTabDiat$genus<-taxonomy$genus[match(dbTabDiat$taxon,taxonomy$cd)]
head(dbTabDiat)


## ----------------------------------------------------------------------------------------------------------------------
dbTabDiatGenus<-dbTabDiat[,-which(colnames(dbTabDiat)=="taxon")]
dbTabDiatGenus<-sumRepeated(dbTabDiatGenus, col_taxon="genus")
head(dbTabDiatGenus)


## ----------------------------------------------------------------------------------------------------------------------
matDiatGenus<-dbTab2mat(dbTabDiatGenus,col_samplingUnits = "SU",col_species = "genus",col_content = "Abundance")


## ----------------------------------------------------------------------------------------------------------------------
save(list=c("dbTab2mat","mat2dbTab"), file=paste0(fol_Results,"funcionesMatDbTab.RData"))
write.csv(matDiatGenus,file = paste0(fol_Results,"matDiatGenus.csv"))

