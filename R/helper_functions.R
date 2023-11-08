mu_select <- function(kernel_fun) {
  switch(kernel_fun,
    "uniform" = 0,
    "epanechnikov" = 1,
    "bisquare" = 2,
    "triweight" = 3
  )
}

bf_select <- function(boundary_method) {
  switch(boundary_method,
    "extend" = 1,
    "shorten" = 0
  )
}

# Obtain frequency from given time series

define_frequency <- function(y, season_input) {

  if (is.na(season_input)) {
    if (inherits(y, c("ts", "zoo", "xts"))) {
      s <- stats::frequency(y)
    } else {
      s <- 1
    }
  } else {
    s <- season_input
  }
  
  s
  
}

# Obtain all smoothing values from input

determine_options <- function(y, smoothing_options) {
  
  list(
    bb = bf_select(smoothing_options@boundary_method),
    mu = mu_select(smoothing_options@kernel_fun),
    p = smoothing_options@order_poly,
    s = define_frequency(y, smoothing_options@season)
  )
  
}

# Determine the inflation setting
determine_InfR <- function(inflation_rate, p) {
  
  if (all(inflation_rate == c("optimal", "naive"))) {
    inflation_rate <- c("optimal", NA, "naive")[[p]]
  }
  
  list("optimal" = c(5 / 7, NA, 9 / 11), "naive" = c(5 / 9, NA, 9 / 13))[[inflation_rate]][[p]]  
    
}

determine_err <- function(autocor) {
  
  as.numeric(autocor)
  
}

determine_CF <- function(correction_factor, err_m) {
  
  c(0, 1)[[(correction_factor + 1) * err_m + (err_m == 0)]]
  
}

determine_err_m <- function(error_model = c("free", "ARMA")) {
  
  error_model <- match.arg(error_model)
  c("free" = 1, "ARMA" = 0)[[error_model]]
  
}

determine_drop <- function(drop, p) {
  
  if (is.null(drop)) {
    drop <- c(0.05, NA, 0.1)[[p]]
  }
  
  drop
  
}

determine_bwidth_start <- function(bwidth_start, p) {
  
  if (is.null(bwidth_start)) {
    bwidth_start <- c(0.1, NA, 0.2)[[p]]
  }  
  
  bwidth_start
  
}

determine_alg <- function(
    inflation_rate, 
    autocor, 
    correction_factor, 
    error_model,
    drop,
    bwidth_start,
    p
  ) {
  
    err_m <- determine_err_m(error_model)
    
    list(
      infr = determine_InfR(inflation_rate, p),
      err = determine_err(autocor),
      CF = determine_CF(correction_factor, err_m),
      err_m = err_m,
      drop = determine_drop(drop, p),
      bwidth_start = determine_bwidth_start(bwidth_start, p)
    )
  
}


set_default <- function(obj, repl) {
  arg_names <- names(repl)
  for (i in seq_along(arg_names)) {
    if (is.null(obj[[arg_names[[i]]]])) {
      obj[[arg_names[[i]]]] <- repl[[i]]
    }
  }
  obj
}

maInfty <- function(ar, ma, m = 1000) {
  p <- length(ar)
  q <- length(ma)
  if (m - q > 0) {
    times <- m - q
  } else {
    times <- 0
  }
  ma.s <- c(ma, rep(0, times = times))
  c.out <- c(rep(0, times = p - 1), 1, rep(NA, times = m))
  lc <- length(c.out)

  if (m >= 1) {
    for (i in (p + 1):(m + p)) {
      c.out[i] <- ar %*% c.out[(i - 1):(i - p)] + ma.s[i - p]
    }
  }

  c.out[p:lc]
}


check_which <-function(which) {
  
    if (is.null(which)) {
    text_prompt <- data.frame(
      c("(1) facets:", "(2) observations:", "(3) fitted:", "(4) detailed_fit:", "(5) trend_season:",
        "(6) residuals:",
        "(7) deseasonalized:", "(8) detrended:"),
      c(
        "Facet plot of the components",
        "Observed time series",
        "Obs. together with trend + seasonality",
        "Obs. together with fitted values and trend",
        "Obs. together with trend and seasonality separately",
        "Residual series",
        "Seasonally adjusted series",
        "Detrended series"
      )
    )
    colnames(text_prompt) <- NULL
    
    cat("\nSelect one of the following plots via the keyword or the position number (exit with 0):\n")
    
    print.data.frame(text_prompt, row.names = FALSE, right = FALSE)
    
    cat("\n")
    
    which <- ""
    
    while(!(which %in% c(0, 1, 2, 3, 4, 5, 6, 7, 8, "facets", "observations", "fitted",
                         "detailed_fit", "trend_season", "residuals", "deseasonalized", 
                         "detrended"))) {
    
        which <- readline("Keyword or position number: ")
        
    }
    cat("\n")
    }
  
    which
}


check_which_hf <- function(which) {
  
    if (is.null(which)) {
    text_prompt <- data.frame(
      c("(1) facets:", "(2) observations:", "(3) fitted:", "(4) residuals:"),
      c(
        "Facet plot of the components",
        "Observed time series",
        "Obs. together with trend + seasonality",
        "Residual series"
      )
    )
    colnames(text_prompt) <- NULL
    
    cat("\nSelect one of the following plots via the keyword or the position number (exit with 0):\n")
    
    print.data.frame(text_prompt, row.names = FALSE, right = FALSE)
    
    cat("\n")
    
    which <- ""
    
    while(!(which %in% c(0, 1, 2, 3, 4, "facets", "observations", "fitted",
                         "residuals"))) {
    
        which <- readline("Keyword or position number: ")
        
    }
    cat("\n")
    }
  
    which
}

aniSave <- function(fun) {
  switch(
    fun,
    HTML = animation::saveHTML,
    Latex = animation::saveLatex,
    GIF = animation::saveGIF
  )
}

select_arma_orders <- function(xt, ar, ma) {
  
  if (is.null(ar) && is.null(ma)) {
    n <- length(xt)
    bic <- matrix(NA, nrow = 4, ncol = 4)
    P <- Q <- 0:3
    for (p0 in P) {
      for (q0 in Q) {
        arma <- tryCatch({
          suppressWarnings(stats::arima(xt,
            order = c(p0, 0, q0),
            include.mean = FALSE))
          }, error = function(e1) {data.frame(loglik = -10000000)}
        )
        bic[p0 + 1, q0 + 1] <- -2 * arma$loglik + (p0 + q0) * log(n)
      }
    }
    orders <- c(which(bic == min(bic), arr.ind = TRUE)) - 1
    ar <- orders[[1]]
    ma <- orders[[2]]
  } else if (is.null(ar)) {
    ar <- 0
  } else if (is.null(ma)) {
    ma <- 0
  }
  c(ar, ma)
}

cap_1st <- function(string) {

    first_letter <- substr(string, 1, 1)
    w <- which(letters == first_letter)
    sub(paste0("^", first_letter), LETTERS[[w]], string)
}

check_input_deseats <- function(y, smoothing_options, bwidth_start, inflation_rate, correction_factor, autocor, drop, error_model) {
  
  stopifnot(
    'y needs to be a time series object of class "ts" or a numeric vector' = (inherits(y, "ts") || (is.atomic(y) && is.numeric(y))),
    'smoothing_options must be an object of S4 class "smoothing_options"' = inherits(smoothing_options, "smoothing_options"),
    "bwidth_start must be a single numeric value between 0 and 0.5 or NULL" = (is.null(bwidth_start) || (length(bwidth_start) == 1 && is.numeric(bwidth_start) && bwidth_start > 0 && bwidth_start < 0.5)),
    'inflation_rate must be either "optimal" or "naive"' = (is.character(inflation_rate) && (all(inflation_rate == c("optimal", "naive")) || (length(inflation_rate) == 1 && inflation_rate %in% c("optimal", "naive")))),
    "correction_factor must be either TRUE or FALSE" = (is.logical(correction_factor) && length(correction_factor) == 1),
    "autocor must be either TRUE or FALSE" = (is.logical(autocor) && length(autocor) == 1), 
    "drop must be a single numeric value between 0 and 0.25 or NULL" = (is.null(drop) || (length(drop) == 1 && is.numeric(drop) && drop >= 0 && drop <= 0.25)),
    'error_model must be either "free" or "ARMA"' = (is.character(error_model) && (all(error_model == c("free", "ARMA")) || (length(error_model) == 1 && error_model %in% c("free", "ARMA"))))   
    
  )   
  
}