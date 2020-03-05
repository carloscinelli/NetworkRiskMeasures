
# contagion ---------------------------------------------------------------

##' Contagion Simulations
##' 
##' @description 
##' Given a matrix of exposures, a vector of buffers and weights (optional) 
##' the functions simulates contagion for all the shock vectors provided. You may
##' choose from the implemented propagation contagion method or create you own propagation method. Details on how to create
##' your own method will be provided in a future version.
##' 
##' 
##' 
##' @usage 
##' contagion(exposures, 
##'           buffer, 
##'           shock = "all", 
##'           weights = NULL, 
##'           method = c("debtrank", "threshold"), 
##'           ...,
##'           exposure_type = c("assets", "liabilities", "impact", "vulnerability"),
##'           keep.history = FALSE, 
##'           abs.tol = .Machine$double.eps ^ 0.2, 
##'           max.it = min(1000, nrow(v)*10),
##'           verbose = TRUE)
##' @inheritParams impact_susceptibility
##' @param shock a list with the shock vectors. If \code{"all"} (the default) the function will run a 
##' simulation for the default of each vertex in the network.
##' @param method the contagion propagation method. Currently, you should use either "debtrank" for the DebtRank propagation method or "threshold" for the traditional default
##' cascades. The DebtRank version implemented is the one proposed in Bardoscia et al (2015). 
##' If you want to use the old "single-hit" DebtRank of Battiston et al (2012), simply provide the argument \code{single.hit = TRUE}.
##' @param ... other arguments to be passed to the contagion propagation method.
##' @param keep.history keep all the history of stress levels? This can use a lot of memory, so
##' the default is \code{FALSE}.
##' @inheritParams matrix_estimation
##' 
##' @return 
##' 
##' The function returns an object of class \code{"contagion"} with the results of the simulation.
##' 
##' @references 
##' Bardoscia M, Battiston S, Caccioli F, Caldarelli G (2015) DebtRank: A Microscopic Foundation for Shock Propagation. PLoS ONE 10(6): e0130406. doi: 10.1371/journal.pone.0130406
##' 
##' Battiston, S., Puliga, M., Kaushik, R., Tasca, P., and Caldarelli, G. (2012). DebtRank: Too central to fail? Financial networks, the FED and systemic risk. Scientific reports, 2:541.
##' @examples 
##' # Loads simulated banking data
##' data("sim_data")
##' head(sim_data)
##' 
##' # seed for reproducibility
##' set.seed(15)
##' 
##' # minimum density estimation
##' # verbose = F to prevent printing
##' md_mat <- matrix_estimation(sim_data$assets, sim_data$liabilities, method = "md", verbose = FALSE)
##' 
##' # rownames and colnames for the matrix
##' rownames(md_mat) <- colnames(md_mat) <- sim_data$bank
##' 
##' # DebtRank simulation
##' contdr <- contagion(exposures = md_mat, buffer = sim_data$buffer, weights = sim_data$weights, 
##'                     shock = "all", method = "debtrank", verbose = FALSE)
##' summary(contdr)
##' 
##' plot(contdr)
##' 
##' # Traditional default cascades simulation
##' contthr <-  contagion(exposures = md_mat, buffer = sim_data$buffer, weights = sim_data$weights, 
##'                       shock = "all", method = "threshold", verbose = FALSE)
##' summary(contthr)
##' 
##'# simulating shock scenarios 1% to 25% shock in all vertices
##'s <- seq(0.01, 0.25, by = 0.01)
##'shocks <- lapply(s, function(x) rep(x, nrow(md_mat)))
##'names(shocks) <- paste(s*100, "pct shock")
##'
##'cont <- contagion(exposures = md_mat, buffer = sim_data$buffer, shock = shocks, 
##'                  weights = sim_data$weights, method = "debtrank", verbose = FALSE)
##'summary(cont)
##'plot(cont)
##' @export
##' @importFrom graphics plot
##' @importFrom stats runif setNames
##' @importFrom utils str
contagion <- function(exposures, 
                      buffer, 
                      shock = "all", 
                      weights = NULL, 
                      method = c("debtrank", "threshold"), 
                      ...,
                      exposure_type = c("assets", "liabilities", "impact", "vulnerability"),
                      keep.history = FALSE, 
                      abs.tol = .Machine$double.eps ^ 0.2, 
                      max.it = min(1000, nrow(v)*10),
                      verbose = TRUE){
  
  exposure_type <- match.arg(exposure_type)
  
  # weights
  if (is.null(weights)) {
    weights <- rep(1, nrow(v))
    warning("No weights provided: using equal weights for all vertices.")
  } 
  
  # weights
  if (missing(buffer)) {
    buffer <- rep(1, nrow(v))
    if (exposure_type != "vulnerability") 
      warning("No buffer provided: using buffer = 1 for all vertices.")
  } 
  
  v <- impact_matrix(exposures = exposures,
                     buffer = buffer,
                     exposure_type = exposure_type)
  
  
  if (!is.list(shock)) shock <- list(shock)
  if (shock[[1]][[1]] == "all") shock <- all_shock(v)
  shock <- set_names_shock(shock)
  
  info <- list(method = method,
               params = list(...))
  
  weights <- weights/sum(weights)
  names(weights) <- NULL
  
  info[[paste0(exposure_type, "_", "matrix")]] <- v
  info$exposures <- exposures
  info$buffer  <- buffer
  info$weights <- weights
  info$keep.history <- keep.history
  info$abs.tol <- abs.tol
  info$max.it  <- max.it
    
  method <- get(method)
  
  # Results vector
  simulations <- setNames(vector("list", length = length(shock)), names(shock))
  vertex_names <- rownames(v)
  
  for (i in seq_along(simulations)) {
    
    if (verbose) cat("\nStarting contagion:", names(shock)[[i]], "\n")
    
    shock_i <- make_shock(shock[[i]], vertex_names)
    
    simulations[[i]] <- contagion_engine(v = v, 
                                         s0 = shock_i, 
                                         p = method, ...,
                                         keep.history = keep.history,
                                         abs.tol = abs.tol,
                                         max.it = max.it,
                                         verbose = verbose)  
  }
  
  
  results <- list(info = info,
                  simulations = simulations)
  
  class(results) <- "contagion"
  
  return(results)
  
}




# shocks ------------------------------------------------------------------

make_shock <- function(shock, vertex_names){
  UseMethod("make_shock")
}

make_shock.character <- function(shock, vertex_names){
  shock <- (vertex_names %in% shock)*1
  return(shock)
}
make_shock.numeric <- function(shock, vertex_names){
  return(shock)
}

set_names_shock <- function(shock){
  
  # Make provisory names
  provisory_names <- paste("scenario", seq_along(shock), sep = "")
  
  # If shock names are NULL, names = provisory
  if (is.null(names(shock))) names(shock) <- provisory_names
  
  # If only some names are empty, these names = provisory
  no_name <- names(shock) == ""
  names(shock)[no_name] <- provisory_names[no_name]
  return(shock)
}


all_shock <- function(v){
  impacts <- rowSums(v)
  # impacts <- impacts[impacts > 0] # remove?
  shock   <- lapply(names(impacts), function(x) x)
  names(shock) <- names(impacts)
  return(shock)
}

make_shocks <- function(shock, vertex_names){
  
  # Turn it into a list
  if (!is.list(shock)) {
    shock <- list(shock)
  }
  
  # Make provisory names
  provisory_names <- paste("Scenario", seq_along(shock))
  
  # If shock names are NULL, names = provisory
  if (is.null(shock)) names(shock) <- provisory_names
  
  # If only some names are empty, these names = provisory
  no_name <- names(shock) == ""
  names(shock)[no_name] <- provisory_names[no_name]
  
  # Turn character shocks into numeric shocks 
  is_shock_character        <- sapply(shock, is.character)
  shock_character           <- shock[is_shock_character]
  shock[is_shock_character] <- lapply(shock[is_shock_character], function(x) (vertex_names %in% x)*1)
  
  # Check if there is a invalid character shock
  check_shock <- sapply(shock, sum)
  if (any(check_shock == 0)) 
    stop("No vertices with names: ", paste(shock_character[check_shock == 0], collapse = ", "))
  
  # Return shock
  return(shock)
  
}


# metrics -----------------------------------------------------------------

system_stress <- function(s, w, cap = 1){
  c(pmin(cap, s) %*% w)
}

system_defaults <- function(s, thr = 1, abs.tol = 1e-12){
  sum(s > thr - abs.tol)
}

system_losses   <- function(s, b){
  c(s %*% b)
}




# summary -----------------------------------------------------------------


##' @method summary contagion
##' @export
summary.contagion <- function(object, thr = 1, cap = 1, sims = "all", ...){
  
  
  simulations <- object$simulations
  
  if (sims[[1]] != "all") {
    simulations <- object$simulations[sims]
  }
  
  w <- object$info$weights 
  b <- object$info$buffer
  
  # scenarios
  summary_table <- data.frame(scenario = names(simulations))
  
  # stress
  summary_table$original_stress     <- sapply(simulations, function(x) system_stress(x$s0, w, cap = cap))
  summary_table$additional_stress   <- sapply(simulations, function(x) system_stress(x$st, w, cap = cap)) - 
    summary_table$original_stress 
  
  # losses
  if (!is.null(b)) {
    summary_table$original_losses     <- sapply(simulations, function(x) system_losses(x$s0, b))
    summary_table$additional_losses   <- sapply(simulations, function(x) system_losses(x$st - x$s0, b))
  } else {
    warning("No buffer provided, losses not calculated.")
  }
  
  # defaults
  summary_table$additional_defaults <- sapply(simulations, function(x) system_defaults(x$st[x$s0 < thr], thr = thr))
  
  results <- list(info = object$info,
                  summary_table = summary_table)
  
  class(results) <- "contagion_summary"
  
  return(results)
}




##' @importFrom dplyr bind_rows
vertex_stats <- function(x, sims = 1:length(x$simulations)){
  vertex_info <- bind_rows(x$simulations[sims], .id = "scenario")
  names(vertex_info)[-1] <- c("initial_stress", "final_stress")
  vertex_info$weight <- x$info$weights
  vertex_info$vertex <- rownames(x$info$assets_matrix)
  vertex_info <- vertex_info[c("scenario", "vertex", "weight", "initial_stress", "final_stress")]
  if (!is.null(x$info$buffer)) { 
    vertex_info$buffer <- x$info$buffer
    vertex_info$loss  <- vertex_info$buffer * vertex_info$final_stress
    vertex_info <- vertex_info[c("scenario", "vertex", "weight", "buffer", 
                                 "initial_stress", "final_stress", "loss")]
  }
  return(as.data.frame(vertex_info))
}




# plot --------------------------------------------------------------------


##' @method plot contagion
##' @export
plot.contagion <- function(x, labels = TRUE, nudge_y = 0.01, check_overlap = TRUE, size = 3, color = "black", ...){
  cont_summary <- summary(x)
  plot(cont_summary, labels = labels, nudge_y = nudge_y, check_overlap = check_overlap, size = size, color = color, ...)
}


##' @import ggplot2
##' @method plot contagion_summary
##' @export
plot.contagion_summary <- function(x, labels = TRUE, nudge_y = 0.01, check_overlap = TRUE, size = 3, color = "black", ...){

  smtable <- x$summary_table
  
  p <- ggplot(smtable, aes_string("original_stress", "additional_stress", label = "scenario")) + 
    geom_point(shape = 21) 
  
  if (labels) {
    p <- p + geom_text(nudge_y = nudge_y,
                       check_overlap = check_overlap,
                       size = size,
                       color = color)
  }
  
  p <- p + 
    xlab("\nOriginal Stress") +
    ylab("Additional Stress\n") +
    ggtitle("Original Stress vs Additional Stress\n") +
    aes_string(fill = "additional_defaults") +
    scale_fill_gradient(name = "Additional\nDefaults", 
                        low = "green",
                        high = "red") +
    theme_minimal() 
  return(p)
}