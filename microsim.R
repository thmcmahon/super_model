# An s3 object and methods for the microsimulation
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

