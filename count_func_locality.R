count_func_locality <-
function(){
  aggregate(All_HI$IndividualCount, by=list(Locality.1=All_HI$Locality.1), FUN=sum, na.rm = T) %>%
  view()
}
