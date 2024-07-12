# deseats 1.1.0
- the argument `correction_factor` in the functions `deseats`, `s_semiarma` and 
  `select_bwidth` is now set to `FALSE` by default.
- further arguments were added to `deseats`, `s_semiarma` and `select_bwidth` to
  allow for a fine-adjustments of the algorithm, when an ARMA model is assumed
  as the error process.
- the functions `seasonplot` and `seasonplot_gg` were added to allow for the
  creation of seasonal plots.
- the faulty link in the manual to the `RcppArmadillo` package was removed.
- the example in the documentation of `ts_conversion` was adjusted to use `file.path` for the path to the temporary data file.