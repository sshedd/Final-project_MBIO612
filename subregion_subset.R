subregion_subset <-
function(x,sub.region){
  subreg <- subset(All_HI_2, Sub.Region==x, drop = F)
  assign(sub.region,subreg,.GlobalEnv)
}
