#
# config.R
# tunable configuration for data generation
#
source("volumes.R")

sql_integer_max <- 2147483647

# simulazione tasso di crescita lineare del numero di pazienti,
# determinato a partire da numero di ricoveri (tabella volumi),
# ricoveri giornalieri attuali e ricoveri giornalieri iniziali
cfg_ric_fin <- 250 # da tabella delle frequenze
cfg_ric_ini <- 80


# tasso decrescita lineare dei ricoveri plurimi per paziente
# (es. 0.9 indica "almeno 1 ricovero" con cardinalità vol_paziente,
#  "almeno 2 ricoveri" con cardinalità (0.9*vol_paziente),
#  "almeno i ricoveri" con cardinalità (0.9^i * vol_paziente))
# NOTA: una tasso di decrescita troppo rapido non permette di generare
# vol_ricovero ricoveri; in tale circostanza verrà segnalato un errore nella
# fase di generazione dei dati.
cfg_decr_ric_multipli <- 0.9


# numero di ICD10 distinti per patologie
cfg_num_icd10 <- 5000

# percentuale diagnosi gravi
cfg_prob_grave <- 0.1


#
# utility functions based on config constants
#

cfg_giorni_apertura = function() {
  ceiling((2 * vol_ricoverato) / (cfg_ric_fin + cfg_ric_ini))
}