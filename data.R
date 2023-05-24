#
# data.R
# utility function to lazily load data from data/ files
#


# load_txt
#   loads a vector from data/[name].csv
#   each vector element should be separated by EOL
load_txt <- function(name) {
  f_name <- paste0("data/", name, ".txt")
  readLines(f_name)
}

# load_csv
#   loads a data frame from data/[name].csv
#   project csv settings:
#     - comma separator
#     - with header
#     - quote all strings (required by default by read.table)
load_csv <- function(name) {
  f_name <- paste0("data/", name, ".csv")
  read.table(f_name, header=T, sep=',', na.strings=c('NULL'))
}

# I can't decide whether this is absolutely horrifying or somewhat reasonable
# This function generates prefix_[name]() functions to load the data when necessary
# (forcing lazy evaluation through the use of functions)
gen_loader_fn <- function(prefix, loader, namelist) {
  for (name in namelist) local({
    l_name <- name
    l_fname <- paste0(prefix, l_name)
    assign( l_fname, function() loader(l_name), envir=.GlobalEnv )
  })
}


# lc_nome() loads data/nome.txt as a vector (c(_,..)) of lines,
# lc_cognome() loads data/cognome.txt, etc.
gen_loader_fn("lc_", load_txt, c(
  "nome",
  "cognome",
  "paese",
  "nome_azienda",
  "nome_farmaco",
  "nome_commerciale"
))

# ldf_provincia() loads data/provincia.csv as a data frame, etc.
gen_loader_fn("ldf_", load_csv, c(
  "comune",
  "provincia",
  "ulss"
))

