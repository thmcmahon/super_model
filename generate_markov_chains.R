df <- readRDS('data/census_super_data.Rds')

# Split the data frame into a list of data frames
df_list <- split(df, f = paste(df$sex_2006, df$age_2006, sep = "_"))

create_matrices <- function(x) { 
  # Now that the data is split into a list of DFs then the data can be converted
  # to matrices
  matrix_col_start <- 4
  rownames(x) <- x$lf_income
  x <- as.matrix(x[matrix_col_start:ncol(x)])
  x
}

#
mat_list <- lapply(df_list, create_matrices)

# prop.table does row totals with the 
trans_probs <- lapply(mat_list, prop.table, margin = 1)

# mc_list <- lapply(trans_probs, function(x) new("markovchain",
#                                                transitionMatrix = x))

men <- new("markovchain", transitionMatrix = trans_probs[['Male_Total']])
women <- new("markovchain", transitionMatrix = trans_probs[['Female_Total']])

mcs <- list("men" = men, "women" = women)

saveRDS(mcs, file = "data/mc_men_women_total.Rds")