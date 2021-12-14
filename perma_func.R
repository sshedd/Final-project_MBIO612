perma_func <-
function(df,x,y){
  df.pivot <- df %>%
    pivot_wider(names_from = all_of(x), 
                values_from = "IndividualCount", 
                values_fn = length,
                values_fill = 0) %>%
    data.frame()
  df.1 <- select_if(df.pivot, is.integer)%>%
    lapply(as.numeric)%>%
    data.frame()
  df.env <- select(df.pivot,where(is.factor))
  df.dist <- vegdist(df.1, method="bray")
  a <- df.env[,y]
  dispersion <- betadisper(df.dist, group=a)
  assign("dispersion",dispersion,.GlobalEnv)
  df.perma <- permutest(dispersion)
  assign("df.perma",df.perma,.GlobalEnv)
  plot(dispersion, hull=FALSE, ellipse=TRUE)
}
