setClass("lmdecomp",
  contains = "decomp",
  slots = c(
    regression_output = "lm"
  )
)

create_lmdecomp <- function(decomp, ts_name, frequency, regression_output) {
  
  methods::new("lmdecomp",
    decomp = decomp,
    ts_name = ts_name,
    frequency = frequency,
    regression_output = regression_output
  )
  
}
