#things to handle: ontologies not found on the tree (warning at least)
# the ontology all from the GO

#also output is a list object with missing ontologies and distance table.
#missing ontologies should be warned to user.

distanceR <- function(ontoGraph, ontoTargets){
  
  #save to output for a message
  ontoMiss <- ontoTargets[!ontoTargets %in% V(ontoGraph)$name]
  ontoTargets <- ontoTargets[ontoTargets %in% V(ontoGraph)$name]
  
  ontoDist <- distances(ontoGraph, v = ontoTargets, to = ontoTargets, mode = "all")
  
  #a sligtlhy arbitrary operation: missing pathway gets scored as 3X longest pathway
  ontoDist[is.infinite(ontoDist)] <- max(ontoDist[!is.infinite(ontoDist)])*3
  
  list(ontoDist = ontoDist, ontoMiss = ontoMiss)
}

