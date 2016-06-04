
# Print Functions ---------------------------------------------------------


##' @method print contagion
##' @export
print.contagion <- function(x, head = 3, tail = 3, ...){
  cat("Contagion Simulations\n\n")
  cat("Method:\n")
  cat(" Function","-", x$info$method,"\n With parameters:", "\n")
  
  params <- as.data.frame(x$info$params)
  print(params, row.names = F)
  
  scenarios <- names(x$simulations)
  n <- length(scenarios)
  
  show <- min(n, head + tail)
  
  cat("\nScenarios (showing ", show, " of ", length(scenarios)," simulations):", "\n", sep = "")
  
  if (n <= show) {
    for (i in 1:n) {
      cat(" Simulation", i, "-", scenarios[i], "\n")
    }
  } else {
    for (i in 1:head) {
      cat(" Simulation", i, "-", scenarios[i], "\n")
    }
    cat(" (...)\n")
    for (i in (n - tail + 1):n) {
      cat(" Simulation", i, "-", scenarios[i], "\n")
    }
  }
  
  cat("\nUse summary to get more details.")
  
}

##' @method str contagion
##' @export
str.contagion <- function(object, max.level = 2, list.len = 5, give.attr = F, ...) 
  str(unclass(object), max.level = max.level, list.len = list.len, give.attr = give.attr, ...)


##' @method print contagion_summary
##' @export
print.contagion_summary <- function(x, head = 10, ...){
  cat("Contagion Simulations Summary\n\n")
  
  cat("Info:\n")
  cat(" Propagation Function:", x$info$method,"\n With parameters:", "\n")
  params <- as.data.frame(x$info$params)
  print(params, row.names = F)
  # number of vertices
  # number of edges
  # etc
  
  cat("\n")
  
  n   <- nrow(x$summary_table)
  for_printing <- head(x$summary_table[order(x$summary_table$additional_stress, decreasing = T),], head)
  
  names(for_printing) <- gsub("_", " ", names(for_printing))
  names(for_printing) <- gsub("(\\b[a-z]{1})", "\\U\\1", names(for_printing), perl = T)
  
  show <- min(n, head)
  cat("Simulation summary (showing ", show, " of ",n," -- decreasing order of additional stress):\n", sep = "")
  print(for_printing, row.names = F, digits = 2)
  if (n < show) cat("  (...)\n")
  
}