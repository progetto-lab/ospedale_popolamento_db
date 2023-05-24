#
# random.R
# utility functions for randomic data generation/selection
#
alphanum = c(letters, LETTERS, 0:9)
ALPHANUM = c(LETTERS, 0:9)

# random_str
#   generate a random string of the specified length from the given alphabet
random_str <- function(alphabet, length) {
  return (paste0(sample(alphabet, length, replace=T), collapse=""))
}

# random_int
#   generate a random in in [min,max] range (including min/max)
random_int <- function(min, max) {
  return (sample(min:max, 1))
}

# random_rows
#   pick count random rows from dataframe df
#   (set replace=T to allow repetitions)
random_rows <- function(df, count, replace=F) {
  out <- df[sample(1:nrow(df), count, replace),]
  rownames(out) <- NULL
  return (out)
}

