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


# Microsimulation code ---------------------------------------------------------

base_file <- readRDS('data/sim_data.Rds')

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
  
  x$parameters <- parameters
  
  x$metadata <- list(run_time = Sys.time())
  
  class(x) <- "microsim"
  x
}

print.microsim <- function(microsim) {
  cat("\n")
  print(microsim$rules)
  cat("\n")
  cat("\n")
  cat("Data:", microsim$data)
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
    facet_wrap(~age_range) + theme_bw() +
    scale_y_continuous(labels = scales::dollar) +
    ggtitle(
      paste0(
        "Average super balance by age group\nfor super contribution rates of ",
        x$parameters[1,1], " and ", x$parameters[1,2]))
}


summary.microsim <- function(x) {
  x$data %>%
    select(gender, age_range, var, starts_with("b")) %>%
    group_by(age_range, gender, var) %>%
    summarise_all(funs(mean(., na.rm = TRUE)))
}

