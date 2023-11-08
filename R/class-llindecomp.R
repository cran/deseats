setClass("llindecomp",
  contains = "decomp",
  slots = c(
    bwidth_trend = "numeric",
    bwidth_season = "numeric",
    boundary_method = "character",
    kernel_par = "numeric"
  )
)

create_llindecomp <- function(decomp, ts_name, frequency, bwidth_trend, bwidth_season, boundary_method, kernel_par) {
  
  methods::new("llindecomp",
    decomp = decomp,
    ts_name = ts_name,
    frequency = frequency,
    bwidth_trend = bwidth_trend,
    bwidth_season = bwidth_season,
    boundary_method = boundary_method,
    kernel_par = kernel_par
  )
  
}