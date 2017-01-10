# contagion engine --------------------------------------------------------

contagion_engine <- function(v, s0, p, ..., 
                             keep.history = TRUE, 
                             abs.tol = .Machine$double.eps ^ 0.2, 
                             max.it = min(1000, nrow(v)*10),
                             verbose = TRUE){
  
  # results
  results <- list()
  
  if (keep.history) results$s <- vector("list", length = max.it)
  
  # sets environment for p
  propagation <- p
  environment(propagation) <- environment()
  
  # initial shock 
  names(s0) <- rownames(v)
  st = s0
  
  # initial state = every vertex with zero stress
  stm1 = setNames(rep(0, length(s0)), names(st))
  
  # difference in stress levels
  diff_st <- sqrt(sum((st - stm1) ^ 2)) 
  
  # initial total propagation
  spt = rep(0, length(s0))
  
  # initialize counter 
  i = 0
  
  # initial propagation
  pt =  propagation(...)
  
  # updates stress level t-1
  stm1 = s0
  
  # loops begin
  repeat {
    
    if (verbose) cat("- Iteration number:", i, "-- diff stress level:",diff_st,"\n")
    
    # updates counter
    i = i + 1
    
    # total propagation so far
    spt = spt + pt
    
    # updates stress level t
    st = stm1 + c(pt %*% v)
    
    # difference in stress levels
    diff_st <- sqrt(sum((st - stm1) ^ 2)) 
    
    # keeps historical s and p
    if (keep.history) results$s[[i]] <- st
    
    # udpates propagation t
    pt =  propagation(...)
    
    # if differences in stress level are too small, break
    if (diff_st < abs.tol) break
    
    # updates stress level t-1
    stm1 = st
    
    # if i reaches maximum number of iterations, break
    if (i >= max.it) { 
      message("Algorithm has reached the maximum number of iterations:", max.it, "\n")
      break
    }
    
  }
  
  results$s0 <- s0
  results$st <- st
  
  if (keep.history) results$s <- results$s[1:i]
  
  class(results) <- "single.contagion"
  
  return(results)
}

# c method for Sparse Matrices
# work around to use c instead of as.vector with debt_rank
##' @export
c.Matrix <- function(...) as.vector(...)

# propagation functions ---------------------------------------------------

# ##' Contagion methods
# ##' 
# ##' Currently you should use the general contagion engine. 
# ##' 
# ##' 
# ##' @name contagion_methods
# NULL

globalVariables(c("spt", "st", "stm1"))

# Generalizes traditional default cascade and debtrank


general <- function(begin.prop = 0, stop.prop = 1, full.prop = 1, ...){
  st[st > 1] <- 1
  pt <- st - spt
  pt[st >= full.prop] <- 1 - spt[st >= full.prop]
  pt[st < begin.prop | spt > stop.prop] <- 0
  return(pt)
}

threshold <- function(alpha = 1){
  environment(general) <- environment()
  general(begin.prop = alpha, stop.prop = 1, full.prop = alpha)
}


debtrank <- function(single.hit = F){
  environment(general) <- environment()
  sh <- 1
  if (single.hit) sh <- 0
  general(begin.prop = 0, stop.prop = sh, full.prop = 1)
}
# 
# fbeta <- function(a,b){
#   pt <- pbeta(st, shape1 = a, shape2 = b) - pbeta(stm1, shape1 = a, shape2 = b)
# }
# 
# # motores
# 
# ddr <- function(){
#   pt = st - stm1
#   pt[stm1 >= 1] <- 0
#   return(pt)
# }
# 
# dr <- function(){
#   pt = st - stm1
#   pt[stm1 >= 0] <- 0
#   return(pt)
# }
# 
# 
# traditional <- function(alpha = 1){
#   pt <- rep(0, length(st))
#   pt[st >= alpha & stm1 < 1] <- 1
#   return(pt)
# }
# 
# 
# traditional <- function(alpha = 1){
#   environment(general) <- environment()
#   general(begin.prop = alpha, full.prop = alpha)
# }