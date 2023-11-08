# Forecasting Function for ARMA Models via Bootstrap

bootCast <- function(model, X, p = NULL, q = NULL,
  n.start = 1000, h = 5, it = 5000, pb = TRUE,
  cores = future::availableCores() - 1, alpha = c(0.95, 0.99), 
  quant.type = 8, bootstrap_type = c("simple", "advanced"), ...) {

  bootF <- bootSetup(model = model, X, p = p, q = q, h = h, n.start = n.start,
                     b_type = bootstrap_type)

  old.plan <- future::plan()
  if (is.null(cores)) {
    future::plan(future::sequential)
  } else {
    future::plan(future::multisession, workers = cores)
  }
  on.exit(future::plan(old.plan))

  progressr::with_progress({
    if (pb) {
      prog <- progressr::progressor(it / 100)
    } else {
      prog <- function() {invisible(NULL)}
    }
    errs <- future.apply::future_lapply(1:it,
      function(x, ...) {
        out <- bootF(x)
        if (x %% 100 == 0) prog()
        out
      }, future.seed = TRUE)

  }, handlers = progressr::handler_progress)

  err.mat <- matrix(unlist(errs), nrow = it, ncol = h, byrow = TRUE)

  alpha.s <- 1 - alpha
  
  q_check <- sort(c(alpha.s / 2, 1 - alpha.s / 2))
  
  quants <- t(apply(err.mat, MARGIN = 2, quantile,
                  probs = q_check, type = quant.type))

  quants

}

bootSetup <- function(model, X, p, q, h, n.start, b_type = c("simple", "advanced")) {
  force(n.start)
  force(h)
  force(X)
  
  b_type <- match.arg(b_type)
  
  innov <- model$residuals
  n <- length(innov)
  Fi <- innov - mean(innov)
  coefs <- model$coef
  ar <- numeric(0)
  ma <- numeric(0)
  ar.t <- 0
  ma.t <- 0
  if (p > 0) {
    ar <- ar.t <- coefs[1:p]
  }
  if (q > 0) {
    ma <- ma.t <- coefs[(p + 1):(p + q)]
  }

  rm(coefs)

  ar.star <- 0
  ma.star <- 0

  if (b_type == "simple") {
    
    X.fc <- c(stats::predict(model, n.ahead = h)$pred)   
    
    out_fun <- function(x) {
      eps_boot <- sample(Fi, size = h, replace = TRUE)
      X.future <- c(tfcastCpp(X, innov,
        eps_boot, ar.t, ma.t, h))

      X.future - X.fc
    }    
    
  } else if (b_type == "advanced") {
  
    out_fun <- function(x) {
      eps_boot <- sample(Fi, size = n.start + n + h, replace = TRUE)
      X.star <- as.numeric(stats::arima.sim(model = list(ar = ar, ma = ma),
        n = n, n.start = n.start, innov = eps_boot[(n.start + 1):(n.start + n)],
        start.innov = eps_boot[1:n.start]))
      est <- suppressWarnings(
        tryCatch(
          stats::arima(X.star, order = c(p, 0, q), include.mean = FALSE),
          error = function(c1) {
            stats::arima(X.star, order = c(p, 0, q),
              include.mean = FALSE, method = "ML")
          }
        )
      )

      coef <- est$coef
      if (p > 0) {
        ar.star <- coef[1:p]
      }
      if (q > 0) {
        ma.star <- coef[(p + 1):(p + q)]
      }

      eps.star <- suppressWarnings(stats::arima(X, order = c(p, 0, q),
        fixed = c(head(ar.star, p), head(ma.star, q)), include.mean = FALSE)[["residuals"]])
      X.star.hat <- c(fcastCpp(X, eps.star, ar.star, ma.star, h))
      X.true <- c(tfcastCpp(X, innov,
        eps_boot[(n.start + n + 1):(n.start + n + h)], ar.t, ma.t, h))
      X.true - X.star.hat
    }
    
  }
  
  out_fun
}


# Forecasting Function for ARMA Models under Normally Distributed Innovations

normCast <- function(model, p = NULL, q = NULL, h = 1,
  alpha = c(0.95, 0.99), ...) {

  coefs <- model$coef
  lc <- length(coefs)

  if (p > 0) {
    ar <- head(coefs, p)
  } else {
    ar <- 0
  }
  if (q > 0) {
    ma <- head(coefs[(p + 1):lc], q)
  } else {
    ma <- 0
  }

  innov <- model$residuals
  sig2 <- model$sigma2

  c.coef <- arma_to_ma(ar = ar, ma = ma, max_i = h - 1)
  sd.fcast <- sqrt(sig2 * cumsum(c.coef^2))
  alpha.s <- 1 - alpha
  q_check <- sort(c(alpha.s / 2, 1 - alpha.s / 2))
  q_norm <- matrix(rep(qnorm(q_check), h), ncol = length(q_check), nrow = h, byrow = TRUE)
  
  q_out <- q_norm * sd.fcast
  cname <- paste0(q_check * 100, "%")
  colnames(q_out) <- cname
  
  q_out

}