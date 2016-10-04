library(dplyr)
library(markovchain)
library(tibble)

tax <- ozTaxData::sample_13_14
mc_list <- readRDS('data/mc_list.Rds')

# Helper functions --------------------

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

range_to_value <- function(range, delimiter) {
  # Randomly choose a value between a text range split by a delimiter
  rng <- unlist(strsplit(range, split = delimiter))
  min <- rng[1]
  max <- rng[2]
  x <- sample(min:max, 1)
  x
}

prep_data <- function(tax_data) {
  tax$age_range_census <- sapply(tax$age_range, convert_age_ranges)
  tax <- tax %>%
    select(ind = Ind, gender = Gender, age = age_range_census,
           super_balance = MCS_Ttl_Acnt_Bal) %>%
    filter(age != '65-74')
  tax
}

build_chain <- function(age_range, gender) {
  # Build a markov chain in 5 year increments from a starting age
  # age_range should be of the form "min-max" eg "55-64"
  # age_range <- unlist(strsplit(age_range, "-"))
  # min_age <- age_range[1]
  # max_age <- age_range[2]
  # age <- sample(min_age:max_age, 1)
  age <- range_to_value(age_range, "-")
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



range_to_income <- function(income_range) {
  # Select an income level from within a range
  stopifnot(typeof(income_range) == "character")
  if (income_range == "Negative_income") {
    income <- 0
  } else if (income_range == "Nil_income") {
    income <- 0
  } else if (income_range == "2000_or_more") {
    income <- 2107 # median income > 2000 in the tax data deflated to 2006
  } else {
    income <- range_to_value(income_range, "-")
  }
  income
}


gender_income_to_range <- function(gender, income) {
  # Convert a gender and income to a range to lookup markov chain
  rnges <- tribble(
    ~rnge, ~min, ~max,
    "15-24", 15, 24,
    "25-34", 25, 34,
    "35-44", 35, 44,
    "45-54", 45, 54,
    "55-64", 55, 64
  )
  rng <- as.character(rnges[rnges$min < income & rnges$max > income,1])
  paste(gender, rng, sep = "_")
}


forecast_income <- function(age_range, gender) {
  chain <- build_chain(age_range, gender)
  unname(sapply(chain, range_to_income) * 50)
}

# Analysis -----------------

tax <- prep_data(tax)

future_states <- mapply(forecast_income, tax$age, tax$gender)

fs_df <- plyr::ldply(future_states, rbind)


merged <- cbind(tax, fs_df)

names(merged) <- c("ind","gender", "age", "super_balance", "id", "2013", "2018", "2023", "2028",
                   "2033", "2038", "2043", "2048", "2053", "2058", "2063", 
                   "2068", "2073", "2078", "2083", "2088", "2093", "2098",
                   "2103")


# A small simulation

microsim <- function(n, mc, mc_len) {
  set.seed(1)
  output_df <- data.frame()
  for (i in 1:n) {
    chain <- rmarkovchain(n = mc_len, object = mc_list[[mc]])
    output_df <- rbind(output_df, sapply(chain, range_to_income))
  }
  names(output_df) <- sapply(seq(1:mc_len),
                                 function(x) paste('t', x, sep = "")
                                 )
  output_df
}
