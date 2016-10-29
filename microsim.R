# An s3 object and methods for the microsimulation
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)

# Superannuation functions -----------------------------------------------------

wage_to_super <- function(x, sgc = .095, tax = .15) {
  # convert a wage into superannuation contributions
  (x * sgc) - ((x * sgc) * tax)
}


calc_super_balance <- function(x, r, sgc = .095, tax = .15, p = 0, na.rm = TRUE) {
  # the sum of all previous contributions * rate + current contribution
  if (na.rm == TRUE) x <- x[!is.na(x)]
  x <- wage_to_super(x, sgc = sgc, tax = tax)
  balance <- vector()
  # This loop checks if there's an existing balance, if so then it calculates a 
  # period of interest. If not then it assigns the first item to the balance and
  # doesn't calculate interest as no time has yet elapsed
  for (i in 1:length(x)) {
    if (length(balance) == 0) {
      if (p == 0) {
        balance <- x[i]
      } else {
        balance <- x[i] + p * (1 + r)
      }
    } else {
      balance <- c(balance, (balance[i - 1] * (1 + r) + x[i]))
    }
  }
  unname(round(balance, 2))
}



add_balances <- function(base_file, r, sgc = .095, tax = .15, na.rm = FALSE,
                         prefix = "b") {
  # Add superannuation balances to the base file
  col_names <- paste(prefix, seq(2014, 2059), sep = "")
  balances <- base_file %>%
    select(super_balance, starts_with("y")) %>%
    apply(1, function(x) {
      current_balance_col_index <- 1
      calc_super_balance(
        x[-current_balance_col_index],
        r = r, sgc = sgc, tax = tax,
        p = x[current_balance_col_index], na.rm = na.rm)}) %>%
    t %>%
    rbind %>%
    as.data.frame
  names(balances) <- col_names
  balances
}

calc_pension <- function(income, partner_status = "single") {
  # calculate weekly aged pension rates
  max_single <- 877.1
  max_couple <- 661.2
  
  inc_test_single <- 162
  ince_test_couple <- 288
  
  taper_single <- .5
  taper_couple <- .25
  
  
  income <- income
  
  if (partner_status == "single") {
    if (income > inc_test_single) {
      pension <- max_single - (income - inc_test_single) * taper_single
    } else {
      pension <- max_single
    }
  }
  pension
}


calc_pension <- function(income, partner_status = "single") {
  # Simple aged pension calculator that includes an income but no assets test
  #
  # Returns fortnightly pension amount
  stopifnot(partner_status == "single" | partner_status == "couple")
  
  # lookup table for values
  params <- tribble(
    ~partner_status, ~max, ~inc_test, ~taper,
    "single", 877.1, 162, .5,
    "couple", 661.2, 288, .25
  )
  
  # set relevant parameters from the lookup table
  max <- params[params$partner_status == partner_status,][['max']]
  inc_test <- params[params$partner_status == partner_status,][['inc_test']] 
  taper <- params[params$partner_status == partner_status,][['taper']] 
  
  # if income is greater than the max income_test then taper the benefit
  if (income > max) {
    pension <- max - (income - inc_test) * taper
  } else {
    pension <- max
  }
  
  # You can't get a negative pension
  ifelse(pension > 0, pension, 0)
}



add_final_balance <- function(df) {
  # This adds final super balances to the base dataset once they've been
  # calculated
  last_non_na <- function(x) {
    x[length(x[!is.na(x)])]
  }
  
  output <- df %>%
    select(starts_with("b")) %>%
    apply(1, last_non_na) %>%
    cbind(df, final_balance = .)
  output
}


add_final_income <- function(df) {
  # This adds final super balances to the base dataset once they've been
  # calculated
  fincome <- function(x) {
    if (length(x[x > 0 & !is.na(x)]) == 0) {
      0
    } else {
      x[length(x[!is.na(x) & x > 0])]
    }
  }
  
  output <- df %>%
    select(starts_with("y")) %>%
    apply(1, fincome) %>%
    unlist %>%
    cbind(df, final_income = .)
  output
}


# Microsimulation code ---------------------------------------------------------

base_file <- readRDS('data/sim_data_partner.Rds')

microsim <- function(sgc_current = .095, sgc_change,
                     tax_current = .15, tax_change = .15,
                     # base_file = base_file,
                     R = .03) {
  
  parameters <- tribble(
    ~current, ~change,
    sgc_current, sgc_change,
    tax_current, tax_change
  )
  
  super_current <- add_balances(base_file, r = R, sgc = sgc_current,
                                tax = tax_current, prefix = "b")
  super_current <- cbind(base_file, super_current)
  super_current$var <- "current"
  
  super_change <- add_balances(base_file, r = R, sgc = sgc_change,
                               tax = tax_change, prefix = "b")
  super_change <- cbind(base_file, super_change)
  super_change$var <- "change"
  
  x <- list()
  
  x$data <- rbind(super_current, super_change)
  
  # x$data$decile <- ntile(x$data$wages, 10)
  # 
  # x$data$ratio <- ifelse(x$data$final_income > 0,
  #                        x$data$retirement_income / x$data$final_income,
  #                        NA)
  
  x$data <- x$data %>%
    select(ind, gender, wages, super_balance, age_range, imputed_age, # decile,
           var, everything()) %>%
    arrange(desc(ind))
                    
  x$data$decile <- ntile(x$data$wages, 10)
  
  x$data <- add_final_balance(x$data)
  
  x$data$retirement_income <- x$data$final_balance * .04
  
  x$data <- add_final_income(x$data)
  
  x$data$ratio <- ifelse(x$data$final_income > 0,
                         x$data$retirement_income / x$data$final_income,
                         NA
                        )
  
  x$parameters <- parameters
  
  x$metadata <- list(run_time = Sys.time())
  
  class(x) <- "microsim"
  x
}


# Microsim methods -------------------------------------------------------------


print.microsim <- function(x) {
  cat("A superannuation microsimulation:", "\n")
  cat("\n")
  cat("Current rate:", x$parameters$current[1],
      "Simulation rate:", x$parameters$change[1], "\n", "\n")
  cat("Model run time:", format(x$metadata$run_time, "%m/%d/%y %H:%M"))
}


plot.microsim <- function(x) {
  df <- x$data %>%
    select(gender, age_range, var, starts_with("b")) %>%
    group_by(age_range, gender, var) %>%
    summarise_all(funs(mean(., na.rm = TRUE))) %>%
    gather(year, value, -age_range, -gender, -var)
  df$year <- as.Date(paste0(substring(df$year, 2), '-01-01'))
  df$gender_var <- paste(df$gender, df$var, sep = "_")
  p <- df %>% ggplot(aes(y = value, x = year, colour = gender_var))
  p + geom_line() + 
    facet_wrap(~age_range, ncol = 5) + theme_bw() +
    theme(legend.position = "bottom") +
    scale_y_continuous(labels = scales::dollar) +
    ggtitle(
      paste0(
        "Average super balance by age group\nfor super contribution rates of ",
        x$parameters[1,1], " and ", x$parameters[1,2]))
}


summary.microsim <- function(x) {
  x$data %>%
    select(gender, age_range, var, starts_with("b"), final_balance) %>%
    group_by(age_range, gender, var) %>%
    summarise_all(funs(mean(., na.rm = TRUE)))
}


super_summary <- function(x, variable, na.rm = FALSE, ...) {
  # Summarise according to variables supplied in dots
  # 
  # variable = the variable to summarise
  # ... = the categories to group the summary by
  dots <- c(...)
  
  x$data %>%
    select_("ind", .dots = dots, "var", variable) %>%
    spread_("var", variable) %>%
    select_(.dots = dots, "current", "change") %>%
    mutate(difference = change - current) %>%
    group_by_(.dots = dots) %>%
    summarise(average = mean(difference, na.rm = na.rm),
              median = median(difference, na.rm = na.rm),
              sd = sd(difference, na.rm = na.rm),
              min = min(difference, na.rm = na.rm),
              max = max(difference, na.rm = na.rm))
}

