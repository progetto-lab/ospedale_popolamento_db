#
# populate.R
# populate the PostgresSQL database with the generated data
#
library("RPostgreSQL")
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

# while not strictly necessary, at the moment for simplicity
# potentially interdependent tuples of terapia and diagnosi
# are generated in conjunction, so it is necessary to use
# a transaction to handle foreign key references
dbBegin(con)
  wr_df("terapia", df_terapia)
  wr_df("diagnosi", df_diagnosi)
dbCommit(con)

wr_df("terapia_per", df_terapia_per)

dbDisconnect(con)

