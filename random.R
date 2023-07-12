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
random_rows <- function(df, count, replace=F, prob=NULL) {
  out <- df[sample(1:nrow(df), count, replace, prob),]
  rownames(out) <- NULL
  return (out)
}


# inter_row_constraint
#   given:
#   - a dataframe of subjects (which may contain repetitions)
#   - a dataframe of attributes for each subject
#   - an inter-row constraint(subject, attr1, attr2)
#   - (optionally) a dataframe of fixed initial subjects
#   - (optionally) a dataframe of fixed initial attributes
#   determines, returning a list of booleans, whether each row of
#   the subject/attribute dataframe respects the constraint or
#   provokes a conflict with another row
#   In each pair of rows violating an inter-row constraint,
#   only one of the two rows will be marked as bad.
inter_row_violations <- function(
    df_subj, df_attr, constraint,
    df_head_subj=NULL, df_head_attr=NULL,
    rows_filter=T)
{
  row_count <- nrow(df_subj)
  names_subj <- names(df_subj)
  names_attr <- names(df_attr)

  irc_index <- 1:row_count
  df_complH <- cbind(irc_index, df_subj, df_attr)[rows_filter,]

  if (is.null(df_head_subj) || is.null(df_head_attr)) {
    df_complL <- df_complH
  } else {
    irc_index <- -1
    df_head <- cbind(irc_index, df_head_subj, df_head_attr)
    df_complL <- rbind(df_head, df_complH)
  }

  # merge rows with the same subject
  df_merge <- merge(df_complL, df_complH, by=names_subj, suffixes=c("L", "H"))
  df_merge <- df_merge[df_merge$irc_indexL < df_merge$irc_indexH,]

  # only remove one of the two rows in a conflicting pair
  # (in this implementation, the one with the highest index)
  lo_set <- setNames(df_merge[,paste0(names_attr,"L"),drop=F], names_attr)
  hi_set <- setNames(df_merge[,paste0(names_attr,"H"),drop=F], names_attr)
  violations <- !constraint(df_merge[,names_subj,drop=F], lo_set, hi_set)
  
  # return the set of indices of rows violating the constraint
  df_merge$irc_indexH[violations]

}

inter_row_constraint <- function(...) {
  bad_index <- inter_row_violations(...)

  # return set of good indices
  good <- rep(T, row_count)
  good[bad_idex] <- F
  good
}



#
# utility functions to construct/combine attribute constraints
#
constraint_none <- function(df_subj, df_attr) { T }

constraint_intra <- function(check_intra) {
  function(df_subj, df_attr, rows_filter, rows_okay) {
    rows_okay[rows_filter] <- check_intra(df_subj[rows_filter,], df_attr[rows_filter,])
    rows_okay
  }
}

constraint_inter <- function(check_inter, ...) {
  function(df_subj, df_attr, rows_filter, rows_okay) {
    violations <- inter_row_violations(df_subj, df_attr, check_inter, ..., rows_filter=rows_filter)
    rows_okay[rows_filter] <- T
    rows_okay[violations] <- F
    rows_okay
  }
}

constraint_and <- function(c1, c2) {
  function(df_subj, df_attr, rows_filter, rows_okay) {
    rows_okay <- c1(df_subj, df_attr, rows_filter, rows_okay)
    rows_okay <- c2(df_subj, df_attr, rows_filter & rows_okay, rows_okay)
    rows_okay
  }
}

# best_effort_generator
#   Best effort generation of subject attributes, conforming to the given constraints
#
#   Oftentimes, as the number of constraints increases, generating data that
#   by construction is completely devoid of any conflict can become rather
#   complicated. Especially in situations where a violation of the constraints
#   is exceedingly unlikely, it is oftentimes easier to just repeat the generation
#   procedure for invalid items, until most issues are solved. Success is not
#   guaranteed, as this is a probabilistic procedure, but after a sufficient number
#   of iterations, if the probability of violating a constraint is low enough,
#   violations are, in practice, extremely unlikely.
#
#   NOTE: A  relative to specified subjects
#   (df_head_attr / df_head_subj) can be provided as additional L-tuples for
#   inter-row constraints check
best_effort_generator <- function(
    df_subj, generator,
    constraint=constraint_none,
    max_iter=10
)
{
  # repeat n_iter probabilistic generation cycles
  df_attr <- generator(df_subj)
  bad_attr <- rep(T, nrow(df_attr))

  # purge and regenerate rows violating the constraint
  for (i in 1:max_iter) {
    # TODO: this can still be somewhat optimized if necessary
    bad_attr <- !constraint(df_subj, df_attr, bad_attr, !bad_attr)
    if (!any(bad_attr)) break

    df_attr[bad_attr,] <- generator(df_subj[bad_attr,])
  }

  df_attr[bad_attr,] <- NA
  df_attr
}

