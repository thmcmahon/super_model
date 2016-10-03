library(dplyr)

tax <- ozTaxData::sample_13_14

initial_state <- function(wages, lf_status) {
  # assigns an initial state string to a taxpayer based on their salary and
  # labour force status
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
    string <- paste(string, "1-149", sep = "")
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
    string <- paste(string, "1600-1999", sep = "")
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
  tax <- tax %>%
    select(id = Ind, gender = Gender, age = age_range_census,
           wages = Sw_amt, transfers = Aust_govt_pnsn_allw_amt,
           other_inc = Tot_inc_amt - Sw_amt - Aust_govt_pnsn_allw_amt,
           super_balance = MCS_Ttl_Acnt_Bal) %>%
    mutate(wages = round(wages / 52, 0), transfers = round(transfers / 52,0),
           other_inc = round(other_inc / 52,0))
  # This needs to be fixed to add some kind of dynamic labour force
  # probabilities.
  tax$lf_status <- sample(c('employed', 'unemployed', 'nilf'),
                          size = nrow(tax), prob = c(64.7, 5.6, 29.7),
                          replace = TRUE)
  tax$state <- mapply(initial_state, tax$wages, tax$lf_status)
  tax
}

# This is on the way but doesn't quite work it's creating a huge vector
add_mc_states <- function(tax) {
  mc <- readRDS('data/mc_men_women_total.Rds')
  container_list <- list()
  for (i in 1:nrow(tax)) {
    container_list[[i]] <- rmarkovchain(n = 5, mc$men)
  }
  state_df <- do.call(rbind, container_list)
  cbind(tax, state_df)
}

