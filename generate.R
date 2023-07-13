#
# generate.R
# generate populated data frames for database tables
#
source("config.R")
source("random.R")
source("volumes.R")
source("data.R")

data_cutoff <- Sys.Date()
ts_cutoff <- as.POSIXct(data_cutoff)

# gen_[name] functions
#   generate a dataframe containing data to populate
#   the [name] SQL table
#   dataframes for tables referenced in foreing key
#   relationships in the table are given as arguments

gen_provincia <- function() {
  ldf_provincia()
}

# numero di pazienti per provincia di residenza (grafico a torta)
gen_paziente <- function(provincia) {
  regione_ospedale <- lc_regione_ospedale()
  provincia_adiacente <- ldf_provincia_adiacente()
  in_regione <- provincia$regione == regione_ospedale

  # distribuzione province adiacenti in base a cfg_prov_adiac
  prob_distr <- function(provincia, scale) {
    distr = rep(NA, nrow(provincia))

    # distribuzione probabilità province adiacenti tra ULSS in esse contenute
    for (i in 1:nrow(provincia_adiacente)) {
      match = provincia_adiacente$sigla[i] == provincia$sigla
      distr[match] = (provincia_adiacente$distr[i] * scale) / sum(match)
    }

    # equiprobabilità province rimanenti
    remaining = is.na(distr)
    assigned_prob = sum(distr[!remaining])
    if (assigned_prob > 1.0) {
      stop("sum of fixed probability distribution for provincia subset elements exceeds 1.0!")
    }
    distr[remaining] = (1.0 - assigned_prob) / sum(remaining)
    distr
  }

  # generazione paziente in sede
  set_loc_in_sede <- data.frame(sigla=provincia[in_regione,]$sigla, regione=NA, ulss=NA)
  prob_loc_in_sede <- prob_distr(set_loc_in_sede, vol_paziente / vol_paziente_in_sede)

  # generazione paziente fuori sede
  set_loc_fuori_sede <- merge(provincia[!in_regione,], ldf_ulss(), by="regione")
  prob_loc_fuori_sede <- prob_distr(set_loc_fuori_sede, vol_paziente / vol_paziente_fuori_sede)

  # generazione provincia, regione, ulss per pazienti in sede, fuori sede
  # in base ai volumi prefissati
  dati_loc <- rbind(
    random_rows(set_loc_fuori_sede, vol_paziente_fuori_sede, replace=T, prob=prob_loc_fuori_sede),
    random_rows(set_loc_in_sede, vol_paziente_in_sede, replace=T, prob=prob_loc_in_sede)
  )[sample(1:vol_paziente),]
  
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
    data_nascita = data_cutoff - sample(1:(36500), vol_paziente, replace=T),
    luogo_nascita = dati_loc$luogo_nascita,
    ulss = dati_loc$ulss,
    regione_di_appartenenza = dati_loc$regione,
    risiede_in_provincia = dati_loc$sigla
  )
}

gen_ricovero <- function(paziente) {
  # distribuzione ammissioni ospedale in giorni apertura
  giorni_apertura = cfg_giorni_apertura()
  dist_giorni <- cfg_ric_fin - ((cfg_ric_fin - cfg_ric_ini) / giorni_apertura) * (1:giorni_apertura)

  # distribuzione durata media, rischio ricoveri, relative all'età di un paziente
  # (valori fittizi, regressione cubica e quartica di curve arbitrarie)
  eta_paziente <- as.numeric(data_cutoff - paziente$data_nascita) / 365
  dati_paziente <- data.frame(
    paziente,
    eta_paziente=eta_paziente,
    durata_media=2.966408 - 0.1639684*eta_paziente + 0.003922258*eta_paziente^2 - 0.00001979411*eta_paziente^3,
    fattore_richio=2.287047 + 0.9262761*eta_paziente - 0.0410491*eta_paziente^2 + 0.0006174624*eta_paziente^3 - 0.000002921158*eta_paziente^4
  )

  gen_periodo_ricovero <- function(dati_paziente) {
    # genera periodo ricovero paziente [per costruzione, vincolo data_fine >= data_inizio]
    data_inizio <- data_cutoff - sample(1:giorni_apertura, nrow(dati_paziente), replace=T, prob=dist_giorni)
    data_fine <- data_inizio + rpois(nrow(dati_paziente), lambda=dati_paziente$durata_media)
    data_fine[data_fine < data_inizio] = data_inizio[data_fine < data_inizio] + 1

    data.frame(data_inizio=data_inizio, data_fine=data_fine)
  }
  chk_data_nascita <- function(dati_paziente, ricovero) {
    # vincolo data_nascita paziente <= data_inizio ricovero
    dati_paziente$data_nascita <= ricovero$data_inizio
  }
  chk_disgiunzione <- function(dati_paziente, ricoveroL, ricoveroR) {
    # vincolo disgiunzione ricoveri stesso paziente
    ((ricoveroL$data_fine < ricoveroR$data_inizio) |
     (ricoveroL$data_inizio > ricoveroR$data_fine))
  }

  #
  # generazione ricovero primario per ogni paziente
  #
  periodo_ricovero1 <- best_effort_generator(dati_paziente,
      generator=gen_periodo_ricovero,
      constraint=constraint_intra(chk_data_nascita))
  # (inter-row constraint unnecessary, each attr subject only appears once)

  inv1 <- !complete.cases(periodo_ricovero1)
  if (any(inv1)) {
    total <- sum(inv1)
    sprintf("WARN: best effort ricovero1 incompleto, assegnazione statica %d elem", total)
    periodo_ricovero1[inv1,"data_inizio"] <- paziente[inv1,"data_nascita"] + 10
    periodo_ricovero1[inv1,"data_fine"] <- paziente[inv1,"data_nascita"] + 12
  }

  #
  # generazione ricoveri secondari per alcuni pazienti
  #
  dati_pazienteM <- random_rows(dati_paziente,
    vol_ricovero - nrow(dati_paziente),
    replace=T, prob=dati_paziente$fattore_rischio)

  periodo_ricoveroM <- best_effort_generator(dati_pazienteM,
    generator=gen_periodo_ricovero,
    constraint=constraint_and(
      constraint_intra(chk_data_nascita),
      constraint_inter(chk_disgiunzione,
        df_head_subj=dati_paziente,
        df_head_attr=periodo_ricovero1)))

  dati_pazienteM <- dati_pazienteM[complete.cases(periodo_ricoveroM),]
  periodo_ricoveroM <- periodo_ricoveroM[complete.cases(periodo_ricoveroM),]

  #
  # unione ricovero primario, ricoveri secondari
  #
  paziente_ricoverato <- rbind(dati_paziente, dati_pazienteM)$codice_sanitario
  periodo_ricovero <- rbind(periodo_ricovero1, periodo_ricoveroM)
  vol_reale_ricovero <- length(paziente_ricoverato)

  # rimozione date fine ricovero future (ricoveri ancora in corso)
  periodo_ricovero$data_fine[periodo_ricovero$data_fine > data_cutoff] <- NA

  # aggregazione dati generati
  data.frame(
    codice_univoco=1:vol_reale_ricovero,
    divisione_ospedaliera=sample(lc_divisione_ospedaliera(), vol_reale_ricovero, replace=T),
    paziente_ricoverato=paziente_ricoverato,
    data_inizio=periodo_ricovero$data_inizio,
    data_fine=periodo_ricovero$data_fine,
    motivo=replicate(vol_reale_ricovero, { random_str(alphanum, 100) })
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
  dose_giornaliera <- paste(
    sample(lc_dosaggio_farmaco(), vol_farmaco, replace=T),
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
  # ogni farmaco contiene tra gli uno e i tre principi attivi
  farmaco_pr1 <- random_rows(farmaco, 0.95 * nrow(farmaco))
  farmaco_pr2 <- random_rows(farmaco_pr1, 0.5 * nrow(farmaco_pr1))
  farmaco_pr3 <- random_rows(farmaco_pr2, 0.2 * nrow(farmaco_pr2))
  farmaco_pr <- rbind(farmaco_pr1, farmaco_pr2, farmaco_pr3)

  nome_pr <- best_effort_generator(farmaco_pr,
    generator=function(farmaco){
      data.frame(principio_attivo=sample(lc_principio_attivo(), nrow(farmaco), replace=T))
    },
    constraint=constraint_inter(function(farmaco, pr1, pr2){ pr1 != pr2 }))

  nome <- nome_pr[complete.cases(nome_pr),"principio_attivo"]
  farmaco <- farmaco_pr[complete.cases(nome_pr),"nome_commerciale"]

  data.frame(
    nome=nome,
    farmaco=farmaco,
    quantita=sample(1:10000, nrow(farmaco_pr), replace=T) / 100)
}

# distribuzione di farmaci della Bribera Farmaceuticals (o RxCorrupt?) prescritti in media, confrontati con quelli di un particolare medico
gen_terapia <- function(ricovero, medico, farmaco) {
  gen_periodo_terapia <- function(dati_terapia) {
    # ts_inizio_ric < data_inizio < data_fine < ts_fine_ric
    durata_ricovero <- as.numeric(difftime(
      dati_terapia$ts_fine_ric,
      dati_terapia$ts_inizio_ric,
      units="secs"))
    durata_ricovero[is.na(durata_ricovero)] <- 10*24*3600

    offs_periodo <- random_intervals(
      nrow(dati_terapia), min=1, max=durata_ricovero, minsize=3600)
    data.frame(
      data_inizio=dati_terapia$ts_inizio_ric + offs_periodo[,1],
      data_fine=dati_terapia$ts_inizio_ric + offs_periodo[,2])
  }

  medico_terapia <- sample(medico$cf, vol_terapia, replace=T)
  farmaco_terapia <- random_rows(farmaco, vol_terapia, replace=T)
  
  # un numero molto ridotto di medici è coinvolto in un caso di corruzione
  compagnia_corr <- lc_nome_azienda()[1]
  corr_prob_prescr <- rep(1, nrow(farmaco))
  corr_prob_prescr[farmaco$azienda_produttrice == compagnia_corr] <- 1000
  medico_corr <- sample(medico$cf, 3)
  for (m in medico_corr) {
    prescrizioni <- medico_terapia == m
    farmaco_terapia[prescrizioni,] <- random_rows(
      farmaco, sum(prescrizioni), prob=corr_prob_prescr, replace=T)
  }

  # genera dati iniziali terapia
  ricovero_terapia = random_rows(ricovero, vol_terapia, replace=T)
  dati_terapia <- data.frame(
    ricovero=ricovero_terapia$codice_univoco,
    ts_inizio_ric=as.POSIXct(ricovero_terapia$data_inizio),
    ts_fine_ric=as.POSIXct(ricovero_terapia$data_fine+1)-1,
    prescritta_da_medico=medico_terapia,
    utilizza_farmaco=farmaco_terapia$nome_commerciale,
    dose_giornaliera=farmaco_terapia$dose_giornaliera_raccomandata) # (utilizza sempre dose raccomandata)

  # genera periodo terapia
  periodo_terapia <- best_effort_generator(dati_terapia,
    generator=gen_periodo_terapia,
    constraint=constraint_inter(
      function(dati_terapia, t1, t2) {
        # non intersezione terapie relative a stesso farmaco
        ((t1$data_fine < t2$data_inizio) |
         (t1$data_inizio > t2$data_fine))
      }))
  dati_terapia <- dati_terapia[complete.cases(periodo_terapia),,drop=F]
  periodo_terapia <- periodo_terapia[complete.cases(periodo_terapia),,drop=F]

  n_terapia <- nrow(dati_terapia)
  data.frame(
    ricovero = dati_terapia$ricovero,
    tnumero = 1:n_terapia,
    utilizza_farmaco = dati_terapia$utilizza_farmaco,
    prescritta_da_medico = dati_terapia$prescritta_da_medico,
    data_inizio = periodo_terapia$data_inizio,
    data_fine = periodo_terapia$data_fine,
    modalita_somministrazione = sample(lc_modalita_somministrazione(), n_terapia, replace=T),
    dose_giornaliera = dati_terapia$dose_giornaliera
  )
}

gen_diagnosi <- function(ricovero, medico, terapia) {
  dati_ricovero <- data.frame(
    codice_univoco=ricovero$codice_univoco,
    ts_inizio_ric=as.POSIXct(ricovero$data_inizio),
    ts_fine_ric=as.POSIXct(ricovero$data_fine+1)-1)
  dati_ricovero$durata_ric <- as.numeric(difftime(
    dati_ricovero$ts_fine_ric,
    dati_ricovero$ts_inizio_ric,
    units="secs"))
  dati_ricovero$durata_ric[is.na(dati_ricovero$durata_ric)] <- 10*24*3600

  # genera diagnosi iniziali (semplificazione: la prima diagnosi avviene
  # sempre in concomitanza con l'inizio del ricovero)
  diagnosi_iniziale <- data.frame(
    ricovero=dati_ricovero$codice_univoco,
    time_stamp=dati_ricovero$ts_inizio_ric,
    effetto_di_terapia=NA)

  # genera diagnosi aggiuntive
  vol_diagnosi_secondarie <- vol_diagnosi - nrow(diagnosi_iniziale)
  ric_diagnosi_secondarie <- random_rows(dati_ricovero, vol_diagnosi_secondarie, replace=T)
  diagnosi_secondarie <- data.frame(
    index=1:vol_diagnosi_secondarie,
    ricovero=ric_diagnosi_secondarie$codice_univoco,
    time_stamp=ric_diagnosi_secondarie$ts_inizio_ric + runif(
      vol_diagnosi_secondarie, 1, ric_diagnosi_secondarie$durata_ric),
    effetto_di_terapia=NA)

  # selezione di alcune diagnosi secondarie per relazione effetto_di
  diagnosi_terapia <- merge(diagnosi_secondarie, terapia, by="ricovero")
  diagnosi_terapia <- diagnosi_terapia[diagnosi_terapia$time_stamp > diagnosi_terapia$data_inizio,]
  diagnosi_secondarie$effetto_di_terapia[diagnosi_terapia$index] <- diagnosi_terapia$tnumero ## errore?
  diagnosi_secondarie$index <- NULL

  diagnosi_effetto <- which(!is.na(diagnosi_secondarie$effetto_di_terapia))
  if (length(diagnosi_effetto) > vol_effetto_di) {
    rimozione_effetto <- sample(diagnosi_effetto, length(diagnosi_effetto) - vol_effetto_di)
    diagnosi_secondarie$effetto_di_terapia[rimozione_effetto] <- NA
  }

  # genera codici ICD10 patologie
  icd_10 <- replicate( cfg_num_icd10, { random_str(ALPHANUM, 7) })

  diagnosi_base <- rbind(diagnosi_iniziale, diagnosi_secondarie)
  n_diagnosi <- nrow(diagnosi_base)
  data.frame(
    ricovero=diagnosi_base$ricovero,
    dnumero=1:n_diagnosi,
    effetto_di_terapia=diagnosi_base$effetto_di_terapia,
    diagnosticato_da_medico=sample(medico$cf, n_diagnosi, replace=T),
    icd10=sample(icd_10, n_diagnosi, replace=T),
    time_stamp=diagnosi_base$time_stamp,
    grave=sample(c(F,T), n_diagnosi, replace=T, prob=c(1-cfg_prob_grave, cfg_prob_grave))
  )
}

gen_terapia_per <- function(ricovero, terapia, diagnosi) {
  terapia_per <- merge(terapia, diagnosi, by="ricovero")
  terapia_per <- terapia_per[terapia_per$time_stamp < terapia_per$data_inizio,]
  terapia_per <- random_rows(terapia_per,
    min(nrow(terapia_per), vol_terapia_per))

  data.frame(
    ricovero=terapia_per$ricovero,
    tnumero=terapia_per$tnumero,
    dnumero=terapia_per$dnumero
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