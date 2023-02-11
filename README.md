# Aim


It is fairly trivial to perform ontology enrichment analysis. Interpreting and presenting these is not. We have created a simple and intuitive shiny app that allows a subject matter expert to cluster and interpret large number of GO enrichment results. 

# Approach

Clustering is performed on an undirected GO graph using the leiden algorith. Briefly, for a given number of GO terms, we initially find and retain all the all to all shortest paths, after witch we perform leiden clustering. Each given cluster is named by the GO term with the highest eigen centrality value. As it seems impropable that any single clustering approach will ever give clusters of satisfying accuracy for all situations, we have built tools that allow the researcher to redefine the clusters as they see fit.

We also include bechmark comparisons to other clustering methods under Benchmarks.html.

You can access the package either by cloning and runing the repo, or at https://cbmr-eel.shinyapps.io/OntologyDescent/
