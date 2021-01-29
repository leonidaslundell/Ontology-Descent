#BP is slow, all is painfully slow due to size...
#is there some kind of smart solution of pruning? Not sure...

#Moreover, this should not be generated every singe time, there should be 
#some kind of check system where the MD5 of the existing file should be checked?

networkeR <- function(ont = "MF", species = "HSA"){
  library(GO.db)
  library(igraph)
  library(data.table)
  
  if(!any(ont %in% c("CC", "BP", "MF","All","Reactome"))){
    stop("ont needs to be one of: CC, BP, MF, All, Reactome")
  }
  
  if(ont == "Reactome"){
    
    flatNet <- fread("https://reactome.org/download/current/ReactomePathwaysRelation.txt", header = F)
    flatNet <- flatNet[grep(species, V1)]
    colnames(flatNet) <- c("from", "to") 
    flatNet
    
  }else{
    
    flatNet <- switch(ont,
                      MF = as.list(GOMFCHILDREN),
                      BP = as.list(GOBPCHILDREN),
                      CC = as.list(GOCCCHILDREN),
                      all = c(as.list(GOCCCHILDREN),
                              as.list(GOMFCHILDREN),
                              as.list(GOBPCHILDREN))) 
    
    flatNet <- flatNet[!is.na(flatNet)]
    flatNet <- reshape2::melt(flatNet)
    
    flatNet <- as.data.table(flatNet)
    colnames(flatNet) <- c("from", "to") 
    
    flatNet <- flatNet[,c("from", "to"):=.(as.character(from), 
                                           as.character(to))]
    
  }
  
  graphNet <- graph_from_data_frame(flatNet)
  
  return(graphNet)
}
