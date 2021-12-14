pca_func <-
function(df,x,y,w){
  df[,x] <- as.numeric(as.factor(df[,x]))
  df_pca = prcomp(df[,y], scale = TRUE) #select col12:14 for temp, sal, oxy + col27 for depth + col31 for habitat
  autoplot(df_pca, data = df, colour = w,
           loadings = TRUE, loadings.colour = 'blue',
           loadings.label = TRUE, loadings.label.size = 3)+
    theme(text = element_text(size = 8))
}
