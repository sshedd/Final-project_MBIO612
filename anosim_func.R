anosim_func <-
function(df,x,y){
  df.pivot <- df %>%
    pivot_wider(names_from = x, 
                values_from = "IndividualCount", 
                values_fn = length,
                values_fill = 0) %>%
    data.frame()
  df.1 <- select_if(df.pivot, is.integer)%>%
    lapply(as.numeric)%>%
    data.frame()
  df.env <- select(df.pivot,where(is.factor))
  a <- df.env[[y]]
  df.anosim <- adonis2(df.1~a, data = df.env, permutations = 999, method="bray")
  assign("df.anosim",df.anosim,.GlobalEnv)
  view(df.anosim)
}
