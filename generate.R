#
# generate.R
# generate populated data frames for database tables
#
source("random.R")
source("volumes.R")
source("data.R")

# gen_[name] functions
#   generate a dataframe containing data to populate
#   the [name] SQL table
#   dataframes for tables referenced in foreing key
#   relationships in the table are given as arguments

gen_provincia <- function() {
  ldf_provincia()
}

gen_paziente <- function(provincia) {
  regione_ospedale <- "LAZIO"
  in_regione <- provincia$regione == regione_ospedale
  set_loc_in_sede <- data.frame(sigla=provincia[in_regione,]$sigla, regione=NA, ulss=NA)
  set_loc_fuori_sede <- merge(provincia[!in_regione,], ldf_ulss(), by="regione")
  
  # generazione provincia, regione, ulss per pazienti in sede, fuori sede
  # in base ai volumi prefissati
  dati_loc = rbind(
    random_rows(set_loc_fuori_sede, vol_paziente_fuori_sede, replace=T),
    random_rows(set_loc_in_sede, vol_paziente_in_sede, replace=T)
  )
  
  # generazione luogo di nascita con la seguente distribuzione
  #   1. 40% nella stessa provincia di residenza
  #   2. 25% nella stessa regione di residenza
  #   3. 25% in altre regioni italiane
  #   4. 10% in altri paesi
  cat = sample(1:4, vol_paziente, replace=T, prob=c(0.40, 0.25, 0.25, 0.10))
  comune <- merge(ldf_comune(), provincia, by="sigla")
  regione <- unique(provincia$regione)

  for (re in regione) {
    for (pr in provincia[provincia$regione == re, 'sigla']) {
      ff1 <- cat == 1 & dati_loc$sigla == pr
      cn1 <- sample(comune[comune$sigla == pr, 'comune'], sum(ff1), replace=T)
      dati_loc[ff1,'luogo_nascita'] <- paste(cn1, pr, 'Italia', sep=', ')

      ff2 <- cat == 2 & dati_loc$sigla == pr
      cn2 <- random_rows(comune[comune$sigla != pr & comune$regione == re, c('comune','sigla')], sum(ff2), replace=T)
      dati_loc[ff2,'luogo_nascita'] <- paste(cn2$comune, cn2$sigla, 'Italia', sep=', ')
      
      ff3 <- cat == 3 & dati_loc$sigla == pr
      cn3 <- random_rows(comune[comune$regione != re, c('comune','sigla')], sum(ff3), replace=T)
      dati_loc[ff3,'luogo_nascita'] <- paste(cn3$comune, cn3$sigla, 'Italia', sep=', ')
    }
  }

  ff4 <- cat == 4
  dati_loc[ff4,'luogo_nascita'] <- sample(lc_paese(), sum(ff4), replace=T)

  
  # randomize codice_sanitario, nome, cognome, data_nascita
  data.frame(
    codice_sanitario = replicate(vol_paziente, { random_str(ALPHANUM, 16) }),
    nome = sample(lc_nome(), vol_paziente, replace=T),
    cognome = sample(lc_cognome(), vol_paziente, replace=T),
    data_nascita = Sys.Date() - sample(1:(36500), vol_paziente, replace=T),
    luogo_nascita = dati_loc$luogo_nascita,
    ulss = dati_loc$ulss,
    regione_di_appartenenza = dati_loc$regione,
    risiede_in_provincia = dati_loc$sigla
  )
}

gen_ricovero <- function(paziente) {
  # TODO
  data.frame(
    codice_univoco = NULL,
    divisione_ospedaliera = NULL,
    data_inizio = NULL,
    data_fine = NULL,
    motivo = NULL,
    paziente_ricoverato = NULL
  )
}

gen_medico <- function() {
  data.frame(
    cf = replicate(vol_medico, { random_str(ALPHANUM, 16) }),
    nome = sample(lc_nome(), vol_medico, replace=T),
    cognome = sample(lc_cognome(), vol_medico, replace=T)
  )
}

gen_farmaco <- function() {
  dosaggio <- c("10 ml", "30 mg", "50 mg", "1 ml", "2 compresse", "20 gocce")
  dose_giornaliera <- paste(
    sample(dosaggio, vol_farmaco, replace=T),
    sample(1:4, vol_farmaco, replace=T),
    "volte al giorno"
  )
  
  data.frame(
    nome_commerciale = sample(lc_nome_commerciale(), vol_farmaco),
    nome = sample(lc_nome_farmaco(), vol_farmaco, replace=T),
    azienda_produttrice = sample(lc_nome_azienda(), vol_farmaco, replace=T),
    dose_giornaliera_raccomandata = dose_giornaliera
  )
}

gen_principio_attivo <- function(farmaco) {
  # TODO
  data.frame(
    nome = NULL,
    farmaco = NULL,
    quantita = NULL
  )
}

gen_terapia <- function(ricovero, medico, farmaco) {
  # TODO
  data.frame(
    ricovero = NULL,
    tnumero = NULL,
    utilizza_farmaco = NULL,
    prescritta_da_medico = NULL,
    data_inizio = NULL,
    data_fine = NULL,
    modalita_somministrazione = NULL,
    dose_giornaliera = NULL
  )
}

gen_diagnosi <- function(ricovero, medico, terapia) {
  # TODO
  data.frame(
    ricovero = NULL,
    dnumero = NULL,
    effetto_di_terapia = NULL,
    diagnosticato_da_medico = NULL,
    icd10 = NULL,
    time_stamp = NULL,
    grave = NULL
  )
}

gen_terapia_per <- function(ricovero, terapia, diagnosi) {
  # TODO
  data.frame(
    ricovero = NULL,
    tnumero = NULL,
    dnumero = NULL
  )
}

# generate data
df_provincia <- gen_provincia()
df_paziente <- gen_paziente(df_provincia)
df_ricovero <- gen_ricovero(df_paziente)
df_medico <- gen_medico()
df_farmaco <- gen_farmaco()
df_principio_attivo <- gen_principio_attivo(df_farmaco)
df_terapia <- gen_terapia(df_ricovero, df_medico, df_farmaco)
df_diagnosi <- gen_diagnosi(df_ricovero, df_medico, df_terapia)
df_terapia_per <- gen_terapia_per(df_ricovero, df_terapia, df_diagnosi)