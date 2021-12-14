remove_row <-
function(x){
  All_HI <- subset(All_HI, Locality.1 != x)
  assign("All_HI",All_HI,.GlobalEnv)
}
