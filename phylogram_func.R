phylogram_func <-
function(df,x){
  species_name <- df[,x] #make list of species in all of Hawaiian region
  species_name <- species_name[!duplicated(species_name)] #remove duplicate species names
  
  out <- classification(species_name, db='itis') #this takes a long time to run, finds associated taxa ratings in order to make phylo tree
  tr <- class2tree(out) #removes species without classification
  plot(tr, type = 'phylogram', no.margin= T,srt=0, label.offset=0.8, font = 1, cex=0.38, adj=0, node.pos=2,)
  title("Phylogram", line = -1.3, adj = 0.01)
}
