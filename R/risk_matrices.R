
# Risk Matrix -------------------------------------------------------------

##' Computes the (binary) impact or vulnerability matrices 
##'
##' The function computes an impact or vulnerability matrix given
##' a network of bilateral exposures and a vector of capital buffers. 
##' 
##' The impact matrix represents how much a vertex impacts the capital buffer of another vertex
##' when it defaults.
##' 
##'  The vulnerability matrix  is just the transpose of the impact matrix. It represents how much a vertex is impacted 
##'  by the default of another vertex.
##'
##'
##' @param exposures an adjacency \code{\link{matrix}}, (sparse) \code{\link{Matrix}} or an \code{\link[igraph]{igraph}}
##' object with the network of bilateral exposures between vertices. By default, the function
##' expects the exposures in the form of an assets matrix
##' in which A -> B means that A has an asset with B. However, you can
##' change that with the parameter \code{exposure_type}. When using a matrix, preferably it should have
##' rows and columns names.
##'
##' @param buffer a numeric vector with the capital buffer for each vertex.
##' Values should be in the same row/column order as the network of bilateral exposures. The
##' buffer is not needed if \code{exposure_type = "vulnerability"}.
##'
##' @param binary if \code{binary = TRUE} the function computes a 'binary' impact or vulnerability matrix.
##' It truncates all values less than 1 to 0 and all values greater than 1 to 1.
##'
##' @param exposure_type character vector indicating the type of the bilateral exposures. It can be
##' an \code{"assets"} network (where A -> B means that A has an asset with B),
##' a \code{"liabilities"} network (where A -> B means that A has a debt with B),
##' a (binary) \code{"impact"} matrix (where A -> B indicates the relative impact
##'  of A in B's capital buffer), or 
##' a (binary) \code{"vulnerability"} matrix 
##' (where A -> B indicates the relative impact A suffers from B's default). 
##' The default is \code{"assets"}.
##'  
##' @param returns will the function return the impact or the vulnerability matrix?  
##' The default is \code{"impact"}. 
##'
##' @return The function returns a (binary) impact or vulnerability matrix.
##'
##' The term V[i,j] of the impact matrix represents
##' the impact of i's default in j's capital buffer. 
##' 
##' The term V[i,j] of the vulnerability matrix represents
##' how much i's capital buffer is impacted by j's default. 
##' 
##' If \code{binary = TRUE}
##' the values less than 1 are truncated to zero.
##'
##' 
##' @examples
##' # Creating example data
##' ## Assets Matrix (bilateral exposures)
##' assets_matrix <- matrix(c(0, 10, 3, 1, 0, 2, 0, 3, 0), ncol = 3)
##' rownames(assets_matrix) <- colnames(assets_matrix) <- letters[1:3]
##'
##' ## Capital Buffer
##' buffer <- c(a = 2, b = 5, c = 2)
##'
##' # Vulnerability matrices
##' vulnerability_matrix(assets_matrix, buffer, binary = FALSE)
##' vulnerability_matrix(assets_matrix, buffer, binary = TRUE)
##' @importFrom expm balance
##' @export
risk_matrix <- function(exposures,
                        buffer,
                        binary = FALSE,
                        exposure_type = c("assets", "liabilities", "impact", "vulnerability"),
                        returns = c("impact", "vulnerability")){
  
  if (!is.numeric(buffer)) stop("buffer must be a numeric vector")
  if (!is.logical(binary)) stop("binary must be either TRUE or FALSE")
  
  UseMethod("risk_matrix")
  
}


##' @export
risk_matrix.igraph <- function(exposures,
                               buffer,
                               binary = FALSE,
                               exposure_type = c("assets", "liabilities", "impact", "vulnerability"),
                               returns = c("impact", "vulnerability")){
  exposures <- exposures[,]
  risk_matrix(exposures,
              buffer,
              binary = binary,
              exposure_type = exposure_type,
              returns = returns)
}





##' @export
risk_matrix.default <- function(exposures,
                                buffer,
                                binary = FALSE,
                                exposure_type = c("assets", "liabilities", "impact", "vulnerability"),
                                returns = c("impact", "vulnerability")) {
  
  
  exposure_type <- match.arg(exposure_type)
  returns       <- match.arg(returns)
  
  exposures <- rowcolnames(exposures)
  
  if (exposure_type == "impact") {
    if (binary) exposures <- (exposures >= 1)*1
    if (returns == "vulnerability") exposures <- t(exposures)
    return(exposures)
  }
  
  
  if (exposure_type == "vulnerability") {
    if (binary) exposures <- (exposures >= 1)*1
    if (returns == "impact") exposures <- t(exposures)
    return(exposures)
  }
  
  if (exposure_type == "liabilities") exposures <- t(exposures)
  
  vulnerability_matrix <- exposures / buffer
  
  if (returns == "impact") vulnerability_matrix <- t(vulnerability_matrix)
  if (binary) vulnerability_matrix <- (vulnerability_matrix >= 1)*1
  
  return(vulnerability_matrix)
}


##' @name risk_matrix
##' @export
vulnerability_matrix <- function(exposures,
                                 buffer,
                                 binary = FALSE,
                                 exposure_type = c("assets", "liabilities", "impact", "vulnerability")){
  
  risk_matrix(exposures,
              buffer,
              binary = binary,
              exposure_type = exposure_type,
              returns = "vulnerability")
  
}

##' @name risk_matrix
##' @export
impact_matrix <- function(exposures,
                          buffer,
                          binary = FALSE,
                          exposure_type = c("assets", "liabilities", "impact", "vulnerability")){
  
  risk_matrix(exposures,
              buffer,
              binary = binary,
              exposure_type = exposure_type,
              returns = "impact")
  
}



# checks for rownames or colnames in a matrix
# internal function
rowcolnames <- function(exposures){
  
  if (is.null(rownames(exposures)) & !is.null(colnames(exposures))) {
    rownames(exposures) <- colnames(exposures)
    warning("No rownames found, colnames used as rownames.")
  }
  
  if (!is.null(rownames(exposures)) & is.null(colnames(exposures))) {
    colnames(exposures) <- rownames(exposures)
    warning("No colnames found, rownames used as colnames")
  }
  
  if (is.null(rownames(exposures)) & is.null(colnames(exposures))) {
    rownames(exposures) <- colnames(exposures) <- paste0("vertex_", 1:nrow(exposures))
    warning("No rownames or colnames found. Vertices named sequentially.")
  }
  
  return(exposures)
}
