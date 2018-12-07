#' @title Type II ANOVA table
#' @description Type II ANOVA table for a model with two covariates including the interaction
#' @param fit a \code{lm} object, with a formula of the form \code{y~a+b+a:b}
#' @return The table, class \code{anova} and \code{data.frame}.
#' @export
#' @importFrom stats anova as.formula update
#' @examples fit <- lm(yield ~ block + N + block:N, data=npk)
#' typeIItable(fit)
typeIItable <- function(fit){
  # to do: test perfect fit
  factors <- attr(fit$terms, "factors")
  terms <- colnames(factors)
  if(terms[3] != paste0(terms[1],":",terms[2])){
    stop("Model not of the form `y ~ a+b+a:b`.")
  }
  tab1 <- stats::anova(fit)[2:4,]
  terms <- rownames(factors)
  newCall <- stats::as.formula(sprintf("%s ~ %s + %s + %s:%s", terms[1], terms[3], terms[2], terms[2], terms[3]))
  weights <- NULL
  if(!is.null(model.frame(fit)$`(weights)`)) weights <- model.frame(fit)[["(weights)"]]
  out <- rbind(stats::anova(stats::lm(formula=newCall, data=model.frame(fit), weights=weights))[2,], tab1)
  attr(out, "heading") <- "Anova Type II Table\n"
  return(out)
}

