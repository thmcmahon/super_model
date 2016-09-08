library(dplyr)

clean_data <- function() {
  file <- 'data_raw/super_v6.csv'
  
  df <- read.csv(file, header = FALSE, stringsAsFactors = FALSE, skip = 5,
                 na.strings = "")
  
  # Junk columns and rows with ABS metadata
  df <- df[1:580,-1]
  
  # combine the 2nd and 3rd rows for the column name
  # na.locf fills NAs with last available name
  names_vector <- paste(zoo::na.locf(t(df[2,]), na.rm = FALSE), t(df[3,]),
                        sep = "_")
  
  # fix the names
  names_vector <- gsub(" *\\(.*?\\) *", "", names_vector) # annual totals
  names_vector <- gsub("\\$", "", names_vector) # dollar signs
  names_vector <- gsub("Employed_", "e_", names_vector) # nice short names
  names_vector <- gsub("Not in the labour force_", "nilf_", names_vector)
  names_vector <- gsub("Unemployed_", "u_", names_vector)
  names_vector <- gsub("\\,", "", names_vector)
  names_vector <- gsub(" ", "_", names_vector)
  
  names(df) <- names_vector
  
  # demerge and fill
  df[4:nrow(df),1:3] <- sapply(df[4:nrow(df),1:3], zoo::na.locf)
  
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
  df[,4:ncol(df)] <- sapply(df[,4:ncol(df)], as.numeric)
  
  saveRDS(df, file = 'data/census_super_data.Rds')
}

clean_data()