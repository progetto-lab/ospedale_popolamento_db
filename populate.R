#
# populate.R
# populate the PostgresSQL database with the generated data
#
library("RPostgreSQL")

# NOTE: data generation takes a substantial amount of
# time (~5 mins) and is laughably inefficient!
# As the code only has to be run once or twice,
# further optimization was deemed unnecessary.
source("generate.R")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="ospedale", host="127.0.0.1", port="5432")

# wr_df
#   convenience function to write a dataframe (append data, discard row names)
wr_df <- function(tblname, df) {
  dbWriteTable(con, name=c("public", tblname), value=df, append=T, row.names=F)
}

# write data to database
wr_df("provincia", df_provincia)

# paziente, ricovero must be inserted in a single transaction, to
# respect the 1:N constraint on the side of paziente in the
# relationship with ricovero
dbBegin(con)
  wr_df("paziente", df_paziente)
  wr_df("ricovero", df_ricovero)
dbCommit(con)

wr_df("medico", df_medico)

# write farmaco tuples first, then the included active principles
wr_df("farmaco", df_farmaco)
wr_df("principio_attivo", df_principio_attivo)

# while not normally necessary, to simplify insertion of large, interdependent
# sets of diagnosi/terapia, we initially remove the reference to terapia in
# diagnosi, and add it back after inserting terapia
df_diagnosi_ne <- df_diagnosi
df_diagnosi_ne$effetto_di_terapia <- NA
wr_df("diagnosi", df_diagnosi_ne)

dbBegin(con)
  # terapia_per must be inserted before terapia, to respect rel.12 constraint
  wr_df("terapia_per", df_terapia_per)
  wr_df("terapia", df_terapia)
dbCommit(con)

# add back effetto_di_terapia
diagnosi_effetto <- df_diagnosi[!is.na(df_diagnosi$effetto_di_terapia),]
for (i in 1:nrow(diagnosi_effetto)) {
    dbExecute(con, "UPDATE diagnosi SET effetto_di_terapia=$1 WHERE ricovero=$2 AND dnumero=$3",
              diagnosi_effetto[i, c("effetto_di_terapia", "ricovero", "dnumero")])
}

dbDisconnect(con)

