iNext_func <-
function(df,x,y){
  #Fix Bug
  source ("https://gist.githubusercontent.com/zdealveindy/f30fa1f0264eabe95828218f61e63df0/raw/333937c2155dc3b8afddd1663e266b6183062602/plot.iNEXT_quickfix")
  iNext_data <- function(){
    df[,c(1,8,y)] %>%
      pivot_wider(names_from =x, 
                  values_from ="IndividualCount", 
                  values_fn = length,
                  values_fill = 0
      ) %>%
      data.frame() 
  }
  
  HI_iNext_locality <- iNext_data()
  
  #organize data into matrix to put into iNext
  HI_iNext_locality_2 <- as.matrix(apply(HI_iNext_locality[,-1],2,as.integer))
  
  #must add species name for row names 
  row.names(HI_iNext_locality_2) <- HI_iNext_locality[,1]
  
  HI_locality_rare <- iNEXT(HI_iNext_locality_2, q=0, datatype="abundance", size=NULL, endpoint=NULL, knots=40,
                            se=TRUE, conf=0.95, nboot=50)
  ggiNEXT(HI_locality_rare)+
    theme_bw(base_size = 7)
}
