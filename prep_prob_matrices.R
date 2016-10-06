library(readr)
library(markovchain)

df <- read_csv('data_raw/super_clean_161004.csv')

# remove tibble
df <- as.data.frame(df)

# adjusted for 2.9 per cent inflation as per the RBA
cleaned_names <- c("sex_2006", "age_2006", "income_2006", "Negative_income",
                  "Nil_income", "1-172", "173-259", "260-345", "345-519",
                  "519-692", "692-866", "867-1082", "1083-1299", "1300-1732",
                  "1733_or_more")

names(df) <- cleaned_names

df$income_2006 <- gsub(" *\\(.*?\\) *", "", df$income_2006)
df$income_2006 <- gsub("\\$", "", df$income_2006)
df$income_2006 <- gsub("\\,", "", df$income_2006)
df$income_2006 <- gsub(" ", "_", df$income_2006)

df$age_2006 <- gsub(" years", "", df$age_2006)

df_list <- split(df, f = paste(df$sex_2006, df$age_2006, sep = "_"))

create_matrices <- function(x) { 
  # Now that the data is split into a list of DFs then the data can be converted
  # to matrices
  matrix_col_start <- 4
  rownames(x) <- x$income_2006
  x <- as.matrix(x[matrix_col_start:ncol(x)])
  x
}

mat_list <- lapply(df_list, create_matrices)

# prop.table does row totals with the margin 1 option
trans_probs <- lapply(mat_list, prop.table, margin = 1)

mc_list <- lapply(trans_probs,
                  function(x) new("markovchain", transitionMatrix = x))

saveRDS(mc_list, 'data/mc_list.Rds')