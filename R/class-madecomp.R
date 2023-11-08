setClass("madecomp",
  contains = "decomp",
  slots = c(
    k_trend = "numeric",
    k_season = "numeric"
  )
)

create_madecomp <- function(decomp, ts_name, frequency, k_trend, k_season) {
  
  methods::new("madecomp",
    decomp = decomp,
    ts_name = ts_name,
    frequency = frequency,
    k_trend = k_trend,
    k_season = k_season
  )
  
}