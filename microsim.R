# An s3 object and methods for the microsimulation
base_file <- readRDS('data/sim_data.Rds')

rules <- function(base = .0925, change, growth) {
  x <- list(base = base, change = change, growth = growth)
  class(x) <- "rules"
  x
}

microsim <- function(rules, data) {
  if (!is(rules, "rules")) {
    stop("rules argument must be an object of class 'rules'")
  }
  x <- list(rules = rules, data = data)
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

print.rules <- function(rules) {
  cat("Base contribution rate:", rules$base, "\n")
  cat("Altered contribution rate:", rules$change, "\n")
  cat("Real growth rate of balances:", rules$growth)
}


compound_interest <- function(P, r, t) {
  P * (1 + r) ^ t
}


compound_interest_contrib <- function(P, r, t, c = 0) {
  # compound interest with regular contributions
  P * (1 + r) ^ t + c * (((1 + r) ^ t - 1) / r)
}


calc_super_balance <- function(x, r, na.rm = TRUE) {
  # the sum of all previous contributions * rate + current contribution
  if (na.rm == TRUE) x <- x[!is.na(x)]
  balance <- vector()
  for (i in 1:length(x)) {
    if (length(balance) == 0) {
      balance <- x[i]
    } else {
      balance <- c(balance, (balance[i - 1] * (1 + r) + x[i]))
    }
  }
  unname(round(balance, 2))
}


add_balances <- function(base_file, r, na.rm = FALSE) {
  col_names <- paste("b", seq(2013, 2058), sep = "")
  balances <- base_file %>% select(super_balance, starts_with("y")) %>%
    apply(1, function(x) calc_super_balance(x, r = r, na.rm = na.rm)) %>%
    t %>%
    rbind %>%
    as.data.frame
  names(balances) <- col_names
  balances
}
