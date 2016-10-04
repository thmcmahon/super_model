library(dplyr)

clean_data <- function() {
  # A function to normalise and clean the following dataset:
  #
  # Australian Census Longitudinal Dataset, 2006-2011
  # 
  # Sex 2006, Age 2006, Labour Force Status 2006 and Total Personal Income
  # (weekly) 2006 by Persons (weighted), Labour Force Status 2011 and Total
  # Personal Income (weekly) 2011 
  # 
  # Counting: Persons (weighted)
  # 
  # This data can't be published as it's confidential. To gain access contact
  # the ABS.
  #
  file <- 'data_raw/super_v6.csv'
  
  df <- read.csv(file, header = FALSE, stringsAsFactors = FALSE, skip = 5,
                 na.strings = "")
  
  # Junk columns and rows with ABS metadata
  actual_data <- 1:580
  metadata_col <- 1
  df <- df[actual_data,-metadata_col]
  
  # na.locf fills NAs with last available name
  lf_names <- 2
  income_names <- 3
  names_vector <- paste(zoo::na.locf(
    t(df[lf_names,]), na.rm = FALSE),
    t(df[income_names,]),
    sep = "_")
  
  # fix the names
  names_vector <- gsub(" *\\(.*?\\) *", "", names_vector) # annual totals
  # nice short names
  names_vector <- gsub("Employed_", "e_", names_vector) 
  names_vector <- gsub("Not in the labour force_", "nilf_", names_vector)
  names_vector <- gsub("Unemployed_", "u_", names_vector)
  # junk chars
  names_vector <- gsub("\\$", "", names_vector)
  names_vector <- gsub("\\,", "", names_vector)
  names_vector <- gsub(" ", "_", names_vector)
  
  names(df) <- names_vector
  
  # Normalise the column variables which
  col_data_starts <- 4
  col_vars <- 1:3
  df[col_data_starts:nrow(df),col_vars] <- sapply(df[col_data_starts:nrow(df),
                                                     col_vars],
                                             zoo::na.locf)
  
  # remove junk empty rows now that the names have been added
  df <- df[5:nrow(df),]
  
  names(df)[1:4] <- c("sex_2006", "age_2006", "lf_status_2006",
                      "weekly_income_2006")
  
  df <- df %>% select(everything(), -contains("_-_RSE"),
                      -contains("_-_Annotations"), -contains("nilf_NA"))
  
  # now create the left side of the matrix
  df$lf_status_2006[df$lf_status_2006 == "Employed"] <- "e"
  df$lf_status_2006[df$lf_status_2006 == "Unemployed"] <- "u"
  df$lf_status_2006[df$lf_status_2006 == "Not in the labour force"] <- "nilf"
  
  df$weekly_income_2006 <- gsub(" *\\(.*?\\) *", "", df$weekly_income_2006)
  df$weekly_income_2006 <- gsub("\\$", "", df$weekly_income_2006)
  df$weekly_income_2006 <- gsub("\\,", "", df$weekly_income_2006)
  df$weekly_income_2006 <- gsub(" ", "_", df$weekly_income_2006)
  
  df$age_2006 <- gsub(" years", "", df$age_2006)
  
  df$lf_income <- paste(df$lf_status_2006, df$weekly_income_2006, sep = "_")
  
  df <- df %>% select(sex_2006, age_2006, lf_income, everything(),
                      -lf_status_2006, -weekly_income_2006)
  
  # convert strings to floats for the counts
  numeric_vars_start <- 4
  df[,numeric_vars_start:ncol(df)] <- sapply(df[,numeric_vars_start:ncol(df)],
                                             as.numeric)
  
  saveRDS(df, file = 'data/census_super_data.Rds')
}

clean_data()

