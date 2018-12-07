#' @importFrom stats pbeta
pnct.pos.ncp.pos.q.finite.sum <- function (q, df, ncp, start, end)
{
  j <- start:end
  ncp.sq.o.2 <- (ncp^2)/2
  jo2 <- j/2
  q.sq <- q^2
  summand.vec <- exp((-ncp.sq.o.2) + jo2 * log(ncp.sq.o.2) -
                       lgamma(jo2 + 1) + log(pbeta(q.sq/(df + q.sq), jo2 + 0.5,
                                                   df/2)))/2
  sum(summand.vec)
}

pnct.pos.ncp.pos.q <- function (i, q, df, ncp, max.terms = 1e+05, num.terms.inc = 20,
                                rel.tol = .Machine$double.eps)
{
  if (any(q < .Machine$double.eps))
    stop("All values of 'q' must be positive.")
  q <- q[i]
  df <- df[i]
  ncp <- ncp[i]
  num.terms <- num.terms.inc
  p <- pnct.pos.ncp.pos.q.finite.sum(q, df, ncp, start = 0,
                                     end = num.terms - 1)
  num.terms <- 2 * num.terms.inc
  while (num.terms <= max.terms) {
    p.inc <- pnct.pos.ncp.pos.q.finite.sum(q, df, ncp, start = num.terms -
                                             num.terms.inc, end = num.terms - 1)
    if (p > 0 && (p.inc/p) < rel.tol)
      break
    p <- p + p.inc
    num.terms <- num.terms + num.terms.inc
  }
  warn <- ifelse(num.terms > max.terms, 1, 0)
  c(p = p, warn = warn)
}

#' @importFrom stats pf pnorm
pnct.pos.ncp <- function (q, df, ncp, max.terms = 1e+05, num.terms.inc = 20,
                          rel.tol = .Machine$double.eps)
{
  n <- length(q)
  if (any(sapply(list(q, df, ncp), length) != n))
    stop("'q', 'df', and 'ncp' must all have the same length.")
  if (any(ncp < .Machine$double.eps))
    stop("All values of 'ncp' must be positive.")
  p <- numeric(n)
  warn <- logical(n)
  P0 <- pnorm(-ncp)
  index <- q == 0
  if (any(index))
    p[index] <- P0[index]
  index <- q > 0
  if (any(index)) {
    PF <- pf(q = q^2, df1 = 1, df2 = df, ncp = ncp^2)
    new.index <- index & ((P0 + PF) == 0)
    if (any(new.index)) {
      p[new.index] <- 0
    }
    new.index <- index & (PF == 1)
    if (any(new.index)) {
      p[new.index] <- 1
    }
    new.index <- index & ((P0 + PF) > 0) & (PF < 1)
    if (any(new.index)) {
      temp.mat <- sapply(1:sum(new.index), pnct.pos.ncp.pos.q,
                         q[new.index], df[new.index], ncp[new.index],
                         max.terms, num.terms.inc, rel.tol)
      p[new.index] <- P0[new.index] + temp.mat[1, ]
      warn[new.index] <- as.logical(temp.mat[2, ])
    }
  }
  index <- q < 0
  if (any(index)) {
    new.index <- index & P0 == 0
    if (any(new.index)) {
      p[new.index] <- 0
      new.index <- index & P0 > 0
      if (any(new.index)) {
        temp.mat <- sapply(1:sum(new.index), pnct.pos.ncp.pos.q,
                           -q[new.index], df[new.index], ncp[new.index],
                           max.terms, num.terms.inc, rel.tol)
        p[new.index] <- P0[new.index] + temp.mat[1, ] -
          pf(q[new.index]^2, 1, df[new.index], ncp[new.index]^2)
        warn[new.index] <- as.logical(temp.mat[2, ])
      }
    }
    else {
      temp.mat <- sapply(1:sum(index), pnct.pos.ncp.pos.q,
                         -q[index], df[index], ncp[index], max.terms,
                         num.terms.inc, rel.tol)
      p[index] <- P0[index] + temp.mat[1, ] - pf(q[index]^2,
                                                 1, df[index], ncp[index]^2)
      warn[index] <- as.logical(temp.mat[2, ])
    }
  }
  list(p = p, warn = warn)
}

#' @name StudentDistribution
#' @rdname Student
#' @title Cumulative distribution function and quantile function of the Student t distribution
#' @description Cumulative distribution function and quantile function of the central or noncentral Student t distribution.
#' @param q vector of quantiles
#' @param p vector of probabilities
#' @param df degrees of freedom
#' @param ncp non-centrality parameter
#' @return \code{pT} returns the value(s) of the cdf, \code{qT} returns the value(s) of the quantile function.
#' @note These functions are copied from the \code{EnvStats} package.
#' They give a correct result when the non-centrality parameter is large while \code{\link[stats]{pt}} or \code{\link[stats]{qt}} fails.
NULL


#' @rdname Student
#' @export
#' @importFrom stats pt
pT <- function (q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE, max.terms = 1e+05,
                num.terms.inc = 20, rel.tol = .Machine$double.eps)
{
  if (missing(ncp)) {
    p <- stats::pt(q = q, df = df, lower.tail = lower.tail,
                   log.p = log.p)
  }
  else if (all(ncp == 0 | is.na(ncp))) {
    p <- stats::pt(q = q, df = df, ncp = ncp, lower.tail = lower.tail,
                   log.p = log.p)
  }
  else {
    names.q <- names(q)
    arg.mat <- cbind(q = as.vector(q), df = as.vector(df),
                     ncp = as.vector(ncp))
    na.index <- apply(arg.mat, 1, function(x) any(is.na(x)))
    if (all(na.index))
      p <- rep(NA, nrow(arg.mat))
    else {
      n <- nrow(arg.mat)
      p <- numeric(n)
      p[na.index] <- NA
      p.no.na <- p[!na.index]
      for (i in c("q", "df", "ncp")) assign(i, arg.mat[!na.index,i])
      if (any(df < .Machine$double.eps))
        stop("All non-missing values of 'df' must be positive.")
      ncp.eq.0 <- ncp == 0
      if (any(ncp.eq.0)) {
        p.no.na[ncp.eq.0] <- stats::pt(q = q[ncp.eq.0],
                                       df = df[ncp.eq.0], lower.tail = TRUE, log.p = FALSE)
      }
      if (!all(ncp.eq.0)) {
        finite.index <- is.finite(q) & is.finite(df) &
          is.finite(ncp)
        index <- !finite.index & !ncp.eq.0
        p.no.na[index] <- stats::pt(q = q[index], df = df[index],
                                    ncp = ncp[index], lower.tail = TRUE, log.p = FALSE)
        if (any(finite.index)) {
          if (max.terms < 1 || trunc(max.terms) != max.terms)
            stop("'max.terms' must be a positive integer.")
          if (num.terms.inc < 1 || num.terms.inc > max.terms ||
              trunc(num.terms.inc) != num.terms.inc)
            stop(paste("'num.terms.inc' must be a positive integer,",
                       "and must be less than or equal to 'max.terms'."))
          if (rel.tol < .Machine$double.eps)
            stop(paste("'rel.tol' must be at least",
                       .Machine$double.eps, "."))
          warn.no.na <- logical(length(p.no.na))
          index <- finite.index & ncp > 0
          if (any(index)) {
            temp.list <- pnct.pos.ncp(q = q[index], df = df[index],
                                      ncp = ncp[index], max.terms = max.terms,
                                      num.terms.inc = num.terms.inc, rel.tol = rel.tol)
            p.no.na[index] <- temp.list$p
            warn.no.na[index] <- temp.list$warn
          }
          index <- finite.index & ncp < 0
          if (any(index)) {
            temp.list <- pnct.pos.ncp(q = -q[index],
                                      df = df[index], ncp = -ncp[index], max.terms = max.terms,
                                      num.terms.inc = num.terms.inc, rel.tol = rel.tol)
            p.no.na[index] <- 1 - temp.list$p
            warn.no.na[index] <- temp.list$warn
          }
          if (any(warn.no.na)) {
            if (n == 1) {
              p.no.na <- NA
              warning(paste("No convergence for the finite",
                            "sum approximation to 'p'.  Change the values",
                            "of 'max.terms' and/or 'rel.tol'."))
            }
            else {
              p.no.na[warn.no.na] <- NA
              warning(paste("No convergence for the finite ",
                            "sum approximation to 'p' for the following ",
                            "element index or indices:", "\n\n\t",
                            paste((1:n)[!na.index][warn.no.na], collapse = " "),
                            "\n\n  ", "Change the value(s) of 'max.terms' and/or ",
                            "'rel.tol'.", sep = ""))
            }
          }
        }
      }
      p[!na.index] <- p.no.na
    }
    lower.tail <- as.logical(lower.tail)
    if (length(lower.tail) != 1) {
      lower.tail <- lower.tail[1]
      warning("Only first element of 'lower.tail' used")
    }
    if (!lower.tail)
      p <- 1 - p
    log.p <- as.logical(log.p)
    if (length(log.p) != 1) {
      log.p <- log.p[1]
      warning("Only first element of 'log.p' used")
    }
    if (log.p)
      p <- log(p)
    if (!is.null(names.q))
      names(p) <- rep(names.q, length = length(p))
  }
  p
}

#' @rdname Student
#' @export
#' @importFrom stats nlminb qt
qT <- function (p, df, ncp = 0, lower.tail = TRUE, log.p = FALSE, max.terms = 1e+05,
                num.terms.inc = 20, rel.tol = .Machine$double.eps)
{
  if (missing(ncp)) {
    q <- stats::qt(p = p, df = df, lower.tail = lower.tail,
                   log.p = log.p)
  }
  else if (all(ncp == 0 | is.na(ncp))) {
    q <- stats::qt(p = p, df = df, ncp = ncp, lower.tail = lower.tail,
                   log.p = log.p)
  }
  else {
    names.p <- names(p)
    arg.mat <- cbind(p = as.vector(p), df = as.vector(df),
                     ncp = as.vector(ncp))
    na.index <- apply(arg.mat, 1, function(x) any(is.na(x)))
    if (all(na.index))
      q <- rep(NA, nrow(arg.mat))
    else {
      q <- numeric(nrow(arg.mat))
      q[na.index] <- NA
      q.no.na <- q[!na.index]
      for (i in c("p", "df", "ncp")) assign(i, arg.mat[!na.index,i])
      if (log.p) {
        if (any(p > 0))
          stop("When log.p=TRUE all values of p must be less than or equal to 0")
        p <- exp(p)
      }
      else if (any(p < 0) || any(p > 1))
        stop("All non-missing values of 'p' must be between 0 and 1.")
      if (!lower.tail)
        p <- 1 - p
      if (any(df < .Machine$double.eps))
        stop("All non-missing values of 'df' must be positive.")
      index <- ncp == 0
      if (any(index)) {
        q.no.na[index] <- stats::qt(p = p[index], df = df[index],
                                    lower.tail = TRUE, log.p = FALSE)
      }
      index <- ncp != 0
      if (any(index)) {
        q.no.na[index & p == 0] <- -Inf
        q.no.na[index & p == 1] <- Inf
        index <- (1:length(q.no.na))[index & (0 < p &
                                                p < 1)]
        if (any(index)) {
          func.to.min <- function(q.weird, p.weird, df.weird,
                                  ncp.weird, max.terms.weird, num.terms.inc.weird,
                                  rel.tol.weird) {
            (pT(q = q.weird, df = df.weird, ncp = ncp.weird,
                lower.tail = TRUE, log.p = FALSE, max.terms = max.terms.weird,
                num.terms.inc = num.terms.inc.weird, rel.tol = rel.tol.weird) -
               p.weird)^2
          }
          o.warn <- options("warn")
          for (i in index) {
            options(warn = -1)
            start <- stats::qt(p = p[i], df = df[i],
                               ncp = ncp[i], lower.tail = TRUE, log.p = FALSE)
            options(o.warn)
            q.no.na[i] <- stats::nlminb(start = start, objective = func.to.min,
                                 p.weird = p[i], df.weird = df[i], ncp.weird = ncp[i],
                                 max.terms.weird = max.terms, num.terms.inc.weird = num.terms.inc,
                                 rel.tol.weird = rel.tol)$par
          }
        }
      }
      q[!na.index] <- q.no.na
    }
    if (!is.null(names.p))
      names(q) <- rep(names.p, length = length(q))
  }
  q
}
