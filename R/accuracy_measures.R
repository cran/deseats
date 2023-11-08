mae <- function(preds, obs) {
  n_out <- length(preds)
  obs_out <- utils::tail(obs, n_out)
  mean(abs(c(preds) - obs_out))
}

rmse <- function(preds, obs) {
  n_out <- length(preds)
  obs_out <- utils::tail(obs, n_out)  
  sqrt(mean((c(preds) - obs_out)^2))
}

pc <- function(preds, obs) {
  n_out <- length(preds)
  obs_out <- utils::tail(obs, n_out)  
  stats::cor(c(preds), obs_out)
}

mase <- function(preds, obs) {
  n_out <- length(preds)
  n_in <- length(obs) - n_out
  obs_out <- utils::tail(obs, n_out)
  obs_in <- utils::head(obs, n_in)
  
  mae_out <- mae(preds, obs_out)
  mae_in <- mae(utils::head(obs_in, n_in - 1), utils::tail(obs_in, n_in - 1))
  mae_out / mae_in
}

rmsse <- function(preds, obs) {
  n_out <- length(preds)
  n_in <- length(obs) - n_out  
  obs_out <- utils::tail(obs, n_out)
  obs_in <- utils::head(obs, n_in)
  
  rmse_out <- rmse(preds, obs_out)
  rmse_in <- rmse(utils::head(obs_in, n_in - 1), utils::tail(obs_in, n_in - 1))
  rmse_out / rmse_in
}

#'Forecasting Accuracy Measure Calculation
#'
#'Given point forecasts and observations, calculate various forecasting 
#'accuracy measures.
#'
#'@param preds the point predictions for the test data period.
#'@param obs the observation series (training data and test data)
#'
#'@details
#'Given one-step-ahead rolling forecasts as well as the whole series of given 
#'observations (training together with test data), different forecasting 
#'accuracy measures (MAE, RMSE, Pearson's correlation, MASE, RMSSE) are being 
#'calculated.
#'
#'@return 
#'A named vector with the obtained criteria values is returned.
#'
#'@export
#'
#'
#'@examples
#'\donttest{
#'xt <- EXPENDITURES
#'xt_in <- window(xt, end = c(2017, 4))
#'yt <- log(xt_in)
#'est <- s_semiarma(yt, set_options(order_poly = 3), inflation_rate = "optimal")
#'fc_results <- predict(est, n.ahead = 8, expo = TRUE)
#'point_fc <- fc_results@pred
#'measures(point_fc, xt)
#'}
#'

measures <- function(preds, obs) {
  funs <- c(MAE = mae, RMSE = rmse, PC = pc, MASE = mase, RMSSE = rmsse)
  
  out <- vapply(
    X = funs,
    FUN = function(.f, preds, obs) {
      .f(preds, obs)
    },
    FUN.VALUE = numeric(1),
    preds = preds,
    obs = obs,
    USE.NAMES = TRUE
  )
  out
}