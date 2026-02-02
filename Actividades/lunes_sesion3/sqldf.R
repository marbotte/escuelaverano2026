library(sqldf)
fol_Datos <- "../../../data_documents/Data/"
fol_Results <- "../../../data_documents/Results/"
(load(paste(fol_Datos,"diatomeas.RData",sep="/")))


sp_abund_mat2<-data.frame(id=1:nrow(sp_abund_mat),sp_abund_mat)
env_info2<-data.frame(id=1:nrow(env_info), env_info, ref=reference_sites)

sqldf("
SELECT id,HER1Lyon,ref
FROM env_info2
WHERE HER1Lyon=6
      ")

sqldf("
SELECT id,HER1Lyon,ref
FROM env_info2
WHERE (HER1Lyon=6 OR HER1Lyon=2 OR HER1Lyon=7) AND ref
      ")

sqldf("
SELECT count(*)
FROM env_info2
WHERE (HER1Lyon=6 OR HER1Lyon=2 OR HER1Lyon=7) AND ref
      ")



sqldf("
SELECT sam2.*
FROM env_info2 AS ei2
JOIN sp_abund_mat2 AS sam2 ON ei2.id=sam2.id
WHERE (HER1Lyon=6 OR HER1Lyon=2 OR HER1Lyon=7) AND ref
      ")
