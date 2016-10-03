library(dplyr)
library(markovchain)

tax <- ozTaxData::sample_13_14
mc_list <- readRDS('data/mc_list.Rds')

convert_age_ranges <- function(age_range) {
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
    filter(age != '65-74') %>%
    mutate(wages = round(wages / 52, 0), transfers = round(transfers / 52,0),
           other_inc = round(other_inc / 52,0))
  tax
}

build_chain <- function(age_range, gender) {
  # Build a markov chain in 5 year increments from a starting age
  # age_range should be of the form "min-max" eg "55-64"
  age_range <- unlist(strsplit(age_range, "-"))
  min_age <- age_range[1]
  max_age <- age_range[2]
  age <- sample(min_age:max_age, 1)
  # initialise the markov chain
  if (gender == "Female") {
    if (age >= 15 & age <= 24) {
      t0 <- rmarkovchain(1, mc_list$`Female_15-24`)
    } else if (age >= 25 & age <= 34) {
      t0 <- rmarkovchain(1, mc_list$`Female_25-34`)
    } else if (age >= 35 & age <= 44) {
      t0 <- rmarkovchain(1, mc_list$`Female_35-44`)
    } else if (age >= 45 & age <= 54) {
      t0 <- rmarkovchain(1, mc_list$`Female_45-54`)
    } else if (age >= 55 & age <= 64) {
      t0 <- rmarkovchain(1, mc_list$`Female_55-64`)
    } else {
      stop('Age range out of bounds')
    }
  } else if (gender == "Male") {
    if (age >= 15 & age <= 24) {
      t0 <- rmarkovchain(1, mc_list$`Male_15-24`)
    } else if (age >= 25 & age <= 34) {
      t0 <- rmarkovchain(1, mc_list$`Male_25-34`)
    } else if (age >= 35 & age <= 44) {
      t0 <- rmarkovchain(1, mc_list$`Male_35-44`)
    } else if (age >= 45 & age <= 54) {
      t0 <- rmarkovchain(1, mc_list$`Male_45-54`)
    } else if (age >= 55 & age <= 64) {
      t0 <- rmarkovchain(1, mc_list$`Male_55-64`)
    } else {
      stop('Age range out of bounds')
    }
  } else {
    stop('Gender must be either Male or Female')
  }
  chain <- t0
  while (age < 65) {
    age <- age + 5
    
    if (gender == "Female") {
      if (age >= 15 & age <= 24) {
        chain <- c(chain, rmarkovchain(1, mc_list$`Female_15-24`),
                   t0 = chain[length(chain)])
      } else if (age >= 25 & age <= 34) {
        chain <- c(chain, rmarkovchain(1, mc_list$`Female_25-34`),
                   t0 = chain[length(chain)])
      } else if (age >= 35 & age <= 44) {
        chain <- c(chain, rmarkovchain(1, mc_list$`Female_35-44`),
                   t0 = chain[length(chain)])
      } else if (age >= 45 & age <= 54) {
        chain <- c(chain, rmarkovchain(1, mc_list$`Female_45-54`),
                   t0 = chain[length(chain)])
      } else if (age >= 55 & age <= 64) {
        chain <- c(chain, rmarkovchain(1, mc_list$`Female_55-64`),
                   t0 = chain[length(chain)])
      }
    } else if (gender == "Male") {
      if (age >= 15 & age <= 24) {
        chain <- c(chain, rmarkovchain(1, mc_list$`Female_15-24`),
                   t0 = chain[length(chain)])
      } else if (age >= 25 & age <= 34) {
        chain <- c(chain, rmarkovchain(1, mc_list$`Female_25-34`),
                   t0 = chain[length(chain)])
      } else if (age >= 35 & age <= 44) {
        chain <- c(chain, rmarkovchain(1, mc_list$`Female_35-44`),
                   t0 = chain[length(chain)])
      } else if (age >= 45 & age <= 54) {
        chain <- c(chain, rmarkovchain(1, mc_list$`Female_45-54`),
                   t0 = chain[length(chain)])
      } else if (age >= 55 & age <= 64) {
        chain <- c(chain, rmarkovchain(1, mc_list$`Female_55-64`),
                   t0 = chain[length(chain)])
      } 
    }
  }
  unname(chain)
}

tax <- prep_data(tax)

future_states <- mapply(build_chain, tax$age, tax$gender)

fs_df <- ldply(future_states, rbind)


merged <- cbind(tax, fs_df)

names(merged) <- c("id", "gender", "age", "wages", "transfers", "other_inc",
                  "super_balance", "id", "2013", "2018", "2023", "2028", "2033",
                  "2038", "2043", "2048", "2053", "2058", "2063", "2068",
                  "2073", "2078", "2083", "2088", "2093", "2098", "2103")



