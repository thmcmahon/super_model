library(dplyr)

tax <- ozTaxData::sample_13_14

initial_state <- function(wages, lf_status) {
  
  # check for integer
  if (wages %% 1 != 0) {
    stop("Wages must be an integer")
  }
  
  string = ""
  
  if (lf_status == "employed") {
    string <- paste("e_", string, sep = "")
  } else if (lf_status == "unemployed") {
    string <- paste("u_", string, sep = "")
  } else if (lf_status == "nilf") {
    string <- paste("nilf_", string, sep = "")
  } else {
    stop("lf_status must be either 'employed', 'unemployed' or 'nilf'")
  }
  
  if (wages < 0) {
    string <- paste(string, "Negative_income", sep = "")
  } else if (wages == 0) {
    string <- paste(string, "Nil_income", sep = "")
  } else if (wages >= 1 && wages <= 149) {
    string <- paste(string, "1_149", sep = "")
  } else if (wages >= 150 && wages <= 249) {
    string <- paste(string, "150-249", sep = "")
  } else if (wages >= 250 && wages <= 399) {
    string <- paste(string, "250-399", sep = "")
  } else if (wages >= 400 && wages <= 599) {
    string <- paste(string, "400-499", sep = "")
  } else if (wages >= 600 && wages <= 799) {
    string <- paste(string, "600-799", sep = "")
  } else if (wages >= 800 && wages <= 999) {
    string <- paste(string, "800-999", sep = "")
  } else if (wages >= 1000 && wages <= 1299) {
    string <- paste(string, "1000-1299", sep = "")
  } else if (wages >= 1300 && wages <= 1599) {
    string <- paste(string, "1300-1599", sep = "")
  } else if (wages >= 1600 && wages <= 1999) {
    string <- paste(string, "1600_1999", sep = "")
  } else if (wages >= 2000) {
    string <- paste(string, "2000_or_more", sep = "")
  }
  
  string
}

convert_age_ranges <- function(age_range) {
  warning("Needs to be fixed to add a probabilistic age split for old people")
  if (age_range == "under 20" | age_range == "20 to 24") {
    output <- '15-24'
  } else if (age_range == "25 to 29" | age_range == "30 to 34") {
    output <- '25-34'
  } else if (age_range == "35 to 39" | age_range == "40 to 44") {
    output <- '35-44'
  } else if (age_range == "45 to 49" | age_range == "50 to 54") {
    output <- '45-54' 
  } else if (age_range == "55 to 59" | age_range == "60 to 64") {
    output <- '55-64'
  } else if (age_range == "65 to 69" | age_range == "70 and over") {
    output <- '65-74'
  } else {
    stop("age_range does not match one of the ATO age ranges")
  }
  output
}

prep_data <- function(tax_data) {
  tax$age_range_census <- sapply(tax$age_range, convert_age_ranges)
  tax %>%
    select(id = Ind, gender = Gender, age = age_range_census,
           wages = Sw_amt, transfers = Aust_govt_pnsn_allw_amt,
           other_inc = Tot_inc_amt - Sw_amt - Aust_govt_pnsn_allw_amt) %>%
    mutate(wages = round(wages / 52, 0), transfers = round(transfers / 52,0),
           other_inc = round(other_inc / 52,0))
}


# TO DO - Classify each person into one of the appropriate buckets

