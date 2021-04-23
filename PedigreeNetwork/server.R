#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

GetPedigree_fromGraph <- function(graph = AllCrosses_igraph, cultivar = "NC-Roy", MaxDepth = 5){
    
    LocalGraph <- make_ego_graph(graph, order = MaxDepth, cultivar, mode = "in")
    LocalGraph <- LocalGraph[[1]]
    
    Igraph_toDataframe <- function(graph){
        
        Edges <- get.edgelist(graph) %>%
            as.data.frame() %>%
            rename(source = V1, target = V2)
        
        Nodes <- V(graph) %>% names()
        NodeDF <- data.frame(id = Nodes)
        
        return(list(Edges = distinct(Edges), Nodes = distinct(NodeDF), FullGraph = graph))
    }
    
    Igraph_toDataframe(LocalGraph)
}

Pedigree_asString <- function(PedigreeGraph, FullGraph = AllCrosses_igraph){
    
    Pedigree_toDF <- function(PedigreeGraph){
        
        SoybaseString <- "https://soybase.org/uniformtrial/index.php?filter=L49-4091&page=lines&test=ALL"
        
        # Topological sort of pedigree
        Ped_topo <- topo_sort(PedigreeGraph) %>% names()
        
        Ped_edges    <- igraph::as_data_frame(PedigreeGraph, what = "edges")
        Ped_edges$to <- factor(Ped_edges$to, levels = Ped_topo)
        
        # Convert the edgelist to a dataframe with columns for female parent, male parent, and cultivar
        Ped_edges %>% 
            distinct() %>%
            pivot_wider(id_cols = to, names_from = name, values_from = from) %>%
            arrange(desc(to)) %>% 
            mutate(to = as.character(to)) %>%
            rename(Cultivar = to) %>%
            select(Female, Male, Cultivar) %>%
            mutate(CrossString = paste(Cultivar, "=", Female, "X", Male), 
                   SoybaseURL = paste0("https://soybase.org/uniformtrial/index.php?filter=", Cultivar, "&page=lines&test=ALL"))
        
    }
    
    EdgeDF <- Pedigree_toDF(PedigreeGraph)
    MissingParents <- unique(which(is.na(EdgeDF$Male) | is.na(EdgeDF$Female)))
    
    if(length(MissingParents) > 0){
        for(i in 1:length(MissingParents)){
            
            MissingIndex <- MissingParents[[i]]
            
            res <- tryCatch({
                
                CultivarName <- as.character(EdgeDF$Cultivar[[MissingIndex]])
                
                NewRow <- make_ego_graph(FullGraph, order = 1, CultivarName, 
                                         mode = "in")[[1]] %>% Pedigree_toDF()
                
                NewRow[1, ]
            }, 
            error = function(cond){
                EdgeDF[MissingIndex, ]
            })
            
            EdgeDF[MissingIndex, ] <- res
            
        }
    }
    
    
    # String representation of the cross
    Ped_String <- reduce(EdgeDF$CrossString, paste, sep = ", ")
    
    return(list(PedigreeDF = EdgeDF, PedigreeString = Ped_String))
}

# Make a pedigree network from the PedigreeDF value returned by the function above
pedigree_VisNetwork_fromDF <- function(graphDF){
    
    GraphData <- graphDF %>% 
        dplyr::select(Cultivar, Male, Female, SoybaseURL) %>%
        pivot_longer(c(Male, Female)) %>%
        select(value, Cultivar, SoybaseURL) %>%
        rename(from = value, to = Cultivar)
    
    Edges <- GraphData %>% select(from, to)
    Nodes <- GraphData %>% select(from, to) %>% unlist() %>% unique()
    Nodes <- Nodes[!is.na(Nodes)]
    
    Nodes <- data.frame(id = Nodes) %>%
        mutate(label = id, 
               SoybaseURL = paste0("https://soybase.org/uniformtrial/index.php?filter=", id, "&page=lines&test=ALL"), 
               title = paste0("<p><a href=", SoybaseURL, ">", id,"</a></p>"))
    
    visNetwork(Nodes, Edges) %>%
        visEdges(arrows = "to") %>%
        visHierarchicalLayout(direction = "UD", 
                              sortMethod = "directed", 
                              levelSeparation = 150, 
                              edgeMinimization = FALSE) -> p
    return(p)
}

PedigreeVisnetwork <- function(graph = AllCrosses_igraph, cultivar = "NC-Roy", MaxDepth = 5){
    
    PedigreeList <- GetPedigree_fromGraph(graph = graph, 
                                          cultivar = cultivar, 
                                          MaxDepth = MaxDepth) %>% 
        .$FullGraph %>%
        Pedigree_asString()
    
    PedNetwork <- pedigree_VisNetwork_fromDF(PedigreeList$PedigreeDF)
    
    return(list(PedigreeData = PedigreeList, NetworkPlot = PedNetwork))
}

load("./Data/AllCrossesGraph.RData")
GraphDegree <- degree(AllCrosses_igraph)
AllCultivars <- GraphDegree[which(GraphDegree > 1)] %>% names()
AllCultivars <- AllCultivars[!grepl("\\(", AllCultivars)] %>% sort()

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    updateSelectizeInput(session, 
                         "CultivarSelection", 
                         choices = AllCultivars, 
                         server = TRUE, 
                         selected = "NC-Dunphy")
    
    NetworkData <- reactive({
        PedigreeVisnetwork(graph = AllCrosses_igraph, 
                           cultivar = input$CultivarSelection, 
                           MaxDepth = input$generations)
    })

    output$network <- renderVisNetwork({
        
        NetworkData()$NetworkPlot

    })
    
    output$crossString <- renderText({NetworkData()$PedigreeData$PedigreeString})

})
