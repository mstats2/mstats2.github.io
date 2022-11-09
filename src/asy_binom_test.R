asy.binom.test <- function(x, n, p = 0.5, alternative = c("two.sided", "less", "greater"),
                           conf.level = 0.95, sd.method = c("bootstrap", "robust")) {
  if (missing(n)) {
    stopifnot(length(x) > 1, length(unique(x)) <= 2)
    tab <- table(as.factor(x))
    if (length(tab) < 2) {
      phat <- 0
      snam <- "success"
    } else {
      phat <- tab[2] / sum(tab)
      snam <- names(tab)[2]
    }
    n <- sum(tab)
    dnam <- deparse(substitute(x))
  } else {
    stopifnot(length(n) == 1, length(x) == 1, is.numeric(n), is.numeric(x))
    phat <- x / n
    dnam <- paste(x, "and", n)
    snam <- "success"
  }

  ## test, p-value
  zstat <- (phat - p) / sqrt(p * (1 - p) / n)
  alternative <- match.arg(alternative)
  pval <- switch(alternative,
                 "two.sided" = pnorm(abs(zstat), lower.tail = FALSE) * 2,
                 "less" = pnorm(zstat),
                 "greater" = pnorm(zstat, lower.tail = FALSE))

  ## confidence interval
  sd.method <- match.arg(sd.method)
  SDhat <- switch(sd.method,
                  "bootstrap" = sqrt(phat * (1 - phat) / n),
                  "robust" = 1 / (2 * sqrt(n)))
  ci <- switch(alternative,
               "two.sided" = phat + c(1, -1) * qnorm((1 - conf.level) / 2) * SDhat,
               "less" = c(0, phat + qnorm(conf.level) * SDhat),
               "greater" = c(phat - qnorm(conf.level) * SDhat, 1))
  attr(ci, "conf.level") <- conf.level

  rval <- list(statistic = structure(zstat, .Names = "Z"),
               p.value = pval,
               conf.int = ci,
               estimate = structure(phat, .Names = paste("probability of", snam)),
               null.value = structure(p, .Names = paste("probability of", snam)),
               alternative = alternative,
               method = "Asymptotic binomial test",
               data.name = dnam)

  class(rval) <- "htest"
  return(rval)
}
