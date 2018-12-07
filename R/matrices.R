#' @name blockmatrices
#' @rdname matrices
#'
#' @title Create block diagonal matrix
#' @description  Build a block diagonal matrix
#'
#' @param M1 top-left block
#' @param M2 bottom-right block
#' @param Ms list of matrices
#' @return A matrix.
#' @seealso The \code{\link[Matrix]{bdiag}} function in the \code{Matrix} package.
#'
#' @examples
#' M1 <- diag(2); M2 <- matrix(2, nrow=3, ncol=3)
#' blockdiag(M1,M2)
#' blockdiag_list(list(M1,M2,M1))
NULL

#' @rdname matrices
#' @export
blockdiag <- function(M1, M2){
  out <- rbind(cbind(M1, matrix(0, nrow=nrow(M1), ncol=ncol(M2))),
               cbind(matrix(0, nrow=nrow(M2), ncol=ncol(M1)), M2))
  colnames(out) <- c(colnames(M1), colnames(M2))
  rownames(out) <- c(rownames(M1), rownames(M2))
  return(out)
}

#' @rdname matrices
#' @export
blockdiag_list <- function(Ms){
  n <- 2
  out <- blockdiag(Ms[[1]], Ms[[2]])
  while(n < length(Ms)){
    n <- n+1
    out <- blockdiag(out, Ms[[n]])
  }
  return(out)
}

