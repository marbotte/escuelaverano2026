library(RSQLite)
biobio2026<-dbConnect(SQLite(),"./biobio2026.sqlite")


fol_Data <- "../../../data_documents/Data/"
occ<-read.csv(paste(fol_Data,"registros_gbif_biobiomma.csv",sep="/"),na.strings = c("NA",""))

dbExecute(biobio2026,
"CREATE TABLE basis_record
(
  cd_basis_record INTEGER PRIMARY KEY,--postgres: cd_basis_record serial PRIMARY KEY
  basis_record text UNIQUE NOT NULL 
);"
)
dbExecute(biobio2026,"
CREATE TABLE occ_tax
(
  occ_id integer PRIMARY KEY,
  species text,
  fecha text,
  lat real,
  long real,
  cd_basis_record integer REFERENCES basis_record(cd_basis_record)
);
")

dbWriteTable(biobio2026,"tmp_occ",occ)
dbExecute(biobio2026,
"INSERT INTO basis_record(basis_record)
SELECT DISTINCT basisofrecord FROM tmp_occ;")
dbExecute(biobio2026,
"INSERT INTO occ_tax(species,  fecha,  lat,  long,cd_basis_record)
SELECT species, eventDate, decimalLatitude, decimalLongitude,cd_basis_record
FROM tmp_occ
LEFT JOIN basis_record ON basisOfRecord=basis_record")

dbExecute(biobio2026,"DROP TABLE tmp_occ")

dbGetQuery(biobio2026,
           "SELECT species,count(*)
           FROM occ_tax
           GROUP BY species
           ORDER BY count(*) DESC")

sf_occ <- st_as_sf(occ, coords=c("decimalLongitude", "decimalLatitude"))
st_crs(sf_occ)<-4326