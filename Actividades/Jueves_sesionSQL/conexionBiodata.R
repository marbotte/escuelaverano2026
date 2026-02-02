



#paquetes
library(RPostgres)
# conexion base de datos
#pp_bst_col <- dbConnect(Postgres(),
#          dbname="pp_bst_col",
#          password="***********",
#          host="54.94.221.113",
#          user="verano2026")

pp_bst_col <- dbConnect(Postgres(),
          dbname="pp_bst_col",
          host="54.94.221.113",
          user="mbottin")
reg<-dbReadTable(pp_bst_col,Id(schema="main",table="register"))
taxo<-dbReadTable(pp_bst_col,Id(schema="main",table="taxo"))
dbGetQuery(pp_bst_col,"SELECT * FROM main.taxo WHERE cd_rank='KG'")
dbGetQuery(pp_bst_col,"SELECT *,find_higher_id(cd_tax,'FAM') FROM main.taxo WHERE cd_rank='GN' LIMIT 10")
dbGetQuery(pp_bst_col,
"SELECT tfam.name_tax, SUM(qt_int) 
FROM main.register r
JOIN main.identification i USING (cd_reg)
JOIN main.taxo t USING (cd_tax)
JOIN main.taxo tfam ON find_higher_id(t.cd_tax,'FAM')=tfam.cd_tax
GROUP BY tfam.name_tax
ORDER BY SUM(qt_int) DESC
LIMIT 10")
