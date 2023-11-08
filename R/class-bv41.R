setClass("bv41",
  contains = "decomp"
)

create_bv41 <- function(decomp, ts_name, frequency) {
  
  methods::new("bv41",
    decomp = decomp,
    ts_name = ts_name,
    frequency = frequency
  )
  
}
