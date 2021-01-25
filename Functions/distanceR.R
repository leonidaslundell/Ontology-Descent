#things to handle: ontologies not found on the tree (warning at least)
# the ontology all from the GO

termClustereR <- function(ontoGraph, ontoTargets){
  
  ontoDist <- distances(ontoGraph, v = ontoTargets, to = ontoTargets)
  
  #a sligtlhy arbitrary operation: missing pathway gets scored as 3X longest pathway
  ontoDist[is.infinite(ontoDist)] <- max(ontoDist[!is.infinite(ontoDist)])*3
  
  ontoDist
}