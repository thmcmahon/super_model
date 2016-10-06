library(dplyr)
library(markovchain)
library(tibble)
library(zoo)

tax <- ozTaxData::sample_13_14
mc_list <- readRDS('data/mc_list.Rds')

# Helper functions --------------------

range_to_value <- function(range, delimiter) {
  # Randomly choose a value between a text range split by a delimiter
  rng <- unlist(strsplit(range, split = delimiter))
  min <- rng[1]
  max <- rng[2]
  x <- sample(min:max, 1)
  x
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


range_to_age <- function(age_range) {
  range_to_value(age_range, " to ")
}


gender_age_to_range <- function(gender, age) {
  # Convert a gender and age to a range to lookup markov chain
  rnges <- tribble(
    ~rnge, ~min, ~max,
    "15-24", 15, 24,
    "25-34", 25, 34,
    "35-44", 35, 44,
    "45-54", 45, 54,
    "55-64", 55, 64
  )
  rng <- as.character(rnges[rnges$min <= age & rnges$max >= age,1])
  paste(gender, rng, sep = "_")
}


income_to_range <- function(income) {
  # Convert an income to the ranges in the markov chain
  rnges <- tribble(
    ~rng, ~min, ~max,
    "Negative_income", -Inf, -1,
    "Nil_income", 0, 0,
    "1-149", 1, 149,
    "150-249", 150, 249,
    "250-399", 250, 399,
    "400-599", 400, 599,
    "600-799", 600, 799,
    "800-999", 800, 999,
    "1000-1299", 1000, 1299,
    "1300-1599", 1300, 1599,
    "1600-1999", 1600, 1999,
    "2000_or_more", 2000, Inf
  )
  rng <- as.character(rnges[rnges$min <= income & rnges$max >= income,1])
  rng
}

# Prep data and build chains ---------------------------------------------------

build_chain <- function(age, gender, income) {
  chain <- vector()
  # first age the person by five years so we don't get an extra year
  age <- age + 5
  while (age < 65) {
    # For the first chain set t0 to be current income
    if (length(chain) == 0) {
      t0 <- income_to_range(income)
    } else {
      # For the second chain set t0 to be last income
      t0 <- chain[length(chain)]
    }
    chain <- c(chain,
               rmarkovchain(
                 n = 1,
                 object = mc_list[[gender_age_to_range(gender, age)]],
                 t0 = t0
               )
    )
    age <- age + 5
  }
  if (length(chain) < 1) return(chain)
  # sapply introduces some ugly names
  unname(sapply(chain, range_to_income))
}


prep_data <- function(tax_data) {
  # Prepare the ATO tax sample to be the base file for analysis
  tax$age_range <- as.character(tax$age_range)
  tax$age_range[tax$age_range == "under 20"] <- "15 to 19"
  too_old <- c("65 to 69", "70 and over")
  tax <- tax[!tax$age_range %in% too_old,]
  tax$imputed_age <- sapply(tax$age_range, range_to_age)
  
  tax <- tax %>%
    select(ind = Ind, gender = Gender, wages = Sw_amt,
           super_balance = MCS_Ttl_Acnt_Bal, age_range, imputed_age) %>%
    mutate(weekly_wage = as.integer(wages / 52))
  tax
}


add_annual_data <- function(sim_data, ...) {
  # Adds annual data with interpolation. Interpolation can be modified with ...
  year_names <- sapply(seq(2013, 2058), function(x) paste("y", x, sep = ""))
  sim_data[,year_names] <- NA
  five_year_names <- sapply(seq(2013,2058,5),
                            function(x) paste("y", x, sep = "")
  )
  five_week_names <- sapply(seq(2013,2058,5),
                            function(x) paste("w", x, sep = "")
  )
  sim_data[,five_year_names] <- sim_data[,five_week_names] * 52
  
  # add the interpolated
  interpolate <- function(x, ...){
    t(round(na.approx(t(x), ...)))
  }
  
  interpolated_data <- sim_data %>% select(starts_with('y')) %>% interpolate
  sim_data[,year_names] <- interpolated_data
  sim_data
}



create_final_dataset <- function(tax) {
  tax <- prep_data(tax)
  future_states <- mapply(
    build_chain, tax$imputed_age, tax$gender, tax$weekly_wage
  )
  fs_df <- plyr::ldply(future_states, rbind)
  merged <- cbind(tax, fs_df)
  names(merged) <- c("ind", "gender", "wages", "super_balance", "age_range",
                     "imputed_age", "w2013", "w2018", "w2023", "w2028",
                     "w2033", "w2038", "w2043", "w2048", "w2053", "w2058")
  merged
  merged <- add_annual_data(merged)
}

sim_data <- create_final_dataset(tax)

saveRDS(sim_data, 'data/sim_data.Rds')