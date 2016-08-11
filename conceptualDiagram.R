

renderDiagrammeR({
  mermaid("
          graph LR
          q(SES)-->id2(Overweight/Obesity)
          w(Brestfeeding)-->id2(Overweight/Obesity)
          e(Sleep Duration)-->id2(Overweight/Obesity)
          r(Breakfast)-->id2(Overweight/Obesity)
          t(Maternal BMI)-->id2(Overweight/Obesity)
          y(Cesarean Delivery)-->id2(Overweight/Obesity)
          u(Smoke In Pregnancy)-->id2(Overweight/Obesity)
          ")
})

renderDiagrammeR({
  grViz("
        digraph boxes_and_circles {
        
        # a 'graph' statement
        graph [overlap = true, fontsize = 10, rankdir = LR]
        
        # several 'node' statements
        node [shape = box,
        fontname = Helvetica]
        SES; Gender; ParentalWarmth; Depression; AlcoholAbuse; ADHD
        
        node [shape = circle,
        fixedsize = true,
        width = 0.9] // sets as circles
        1; 2; 3; 4; 5; 6; 7; 8
        
        # several 'edge' statements
        SES-> ParentalWarmth ParentalWarmth-> Depression ParentalWarmth-> AlcoholAbuse
        
        }
        ")
    })
    
    renderDiagrammeR({
      grViz("
            digraph dot  {
            
            # a 'graph' statement
            graph [layout = dot, overlap = true, fontsize = 10, rankdir = LR]
            
            # several 'node' statements
            node [shape = oval,  fontname = Helvetica,  style = filled, 
            color = grey]
            
            node [fillcolor = red]
            Depression
            
            node [fillcolor = blue]
            SES; Gender
            
            
            node [fillcolor = orange]
            ParentalWarmth; AlcoholAbuse; ADHD; Obesity;
            ParentalDepression; ParentalAlcoholAbuse; BeingBullied; Maltreatment;
            
            # several 'edge' statements
            edge[color = grey]
            SES-> {ParentalWarmth Obesity AlcoholAbuse Depression}
            Maltreatment-> {AlcoholAbuse Depression} Obesity->Depression
            Gender-> {ADHD AlcoholAbuse Depression}
            ADHD->AlcoholAbuse 
            ParentalAlcoholAbuse->AlcoholAbuse 
            ParentalDepression->Depression 
            BeingBullied->Depression 
            ParentalWarmth->Depression 
            ParentalWarmth->AlcoholAbuse
            
            }
            ")
    })


require(shiny)
require(visNetwork)

server <- function(input, output) {
  output$network <- renderVisNetwork({
    # minimal example
    nodes <- data.frame(id = 1:3)
    edges <- data.frame(from = c(1,2), to = c(1,3))
    
    visNetwork(nodes, edges)
  })
}

ui <- fluidPage(
  visNetworkOutput("network")
)

shinyApp(ui = ui, server = server)




mermaid("
 graph LR
 id1(Start)-->id2(Stop)
    style id1 fill:#f9f,stroke:#333,stroke-width:4px;
        style id2 fill:#ccf,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5;
        ")

nodes <-
  create_nodes(
    nodes = LETTERS,
    label = TRUE,
    type = "letter",
    shape = sample(c("circle", "square"),
                   length(LETTERS),
                   replace = TRUE),
    fillcolor = sample(c("aqua", "orange",
                         "pink", "lightgreen",
                         "black", "yellow"),
                       length(LETTERS),
                       replace = TRUE))

# Create an edge data frame (edf)
edges <-
  create_edges(
    from = sample(LETTERS, replace = TRUE),
    to = sample(LETTERS, replace = TRUE),
    rel = "letter_to_letter")

# Create a graph object using the ndf and edf, and,
# add a few attributes for the graph appearance
graph <-
  create_graph(
    nodes_df = nodes,
    edges_df = edges,
    graph_attrs = "layout = twopi",
    node_attrs = c("fontname = Helvetica",
                   "style = filled"),
    edge_attrs = c("color = gray20",
                   "arrowsize = 0.5"))

# Render the graph using Graphviz
render_graph(graph)


G <- graph( c(1,2,1,3,1,4,3,4,3,5,5,6,6,7,7,8,8,9,3,8,5,8), directed = TRUE )

plot(G, layout = layout.fruchterman.reingold,
     vertex.size = 25,
     vertex.color="red",
     vertex.frame.color= "white",
     vertex.label.color = "white",
     vertex.label.family = "sans",
     edge.width=2,  
     edge.color="black")

library(DiagrammeR)

nodes <-
  create_nodes(
    nodes = 1:4,
    data = c(9.7, 8.5,
             2.2, 6.0))

graph <- create_graph(nodes_df = nodes)

# Select nodes where the `data` attribute
# has a value greater than 7.0 (it's the
# first 2 nodes)
graph <-
  select_nodes(
    graph = graph,
    node_attr = "data",
    search = ">7.0")


library(networkD3)


# Create fake data
src <- c("A", "A", "A", "A",
         "B", "B", "C", "C", "D")
target <- c("B", "C", "D", "J",
            "E", "F", "G", "H", "I")
networkData <- data.frame(src, target)

# Plot
simpleNetwork(networkData)



# Load data
data(MisLinks)
data(MisNodes)

# Plot
forceNetwork(Links = MisLinks, Nodes = MisNodes,
             Source = "source", Target = "target",
             Value = "value", NodeID = "name",
             Group = "group", opacity = 0.8)


CanadaPC <- list(name = "Canada", children = list(list(name = "Newfoundland",
                                                       children = list(list(name = "St. John's"))),
                                                  list(name = "PEI",
                                                       children = list(list(name = "Charlottetown"))),
                                                  list(name = "Nova Scotia",
                                                       children = list(list(name = "Halifax"))),
                                                  list(name = "New Brunswick",
                                                       children = list(list(name = "Fredericton"))),
                                                  list(name = "Quebec",
                                                       children = list(list(name = "Montreal"),
                                                                       list(name = "Quebec City"))),
                                                  list(name = "Ontario",
                                                       children = list(list(name = "Toronto"),
                                                                       list(name = "Ottawa"))),
                                                  list(name = "Manitoba",
                                                       children = list(list(name = "Winnipeg"))),
                                                  list(name = "Saskatchewan",
                                                       children = list(list(name = "Regina"))),
                                                  list(name = "Nunavuet",
                                                       children = list(list(name = "Iqaluit"))),
                                                  list(name = "NWT",
                                                       children = list(list(name = "Yellowknife"))),
                                                  list(name = "Alberta",
                                                       children = list(list(name = "Edmonton"))),
                                                  list(name = "British Columbia",
                                                       children = list(list(name = "Victoria"),
                                                                       list(name = "Vancouver"))),
                                                  list(name = "Yukon",
                                                       children = list(list(name = "Whitehorse")))
))

diagonalNetwork(List = CanadaPC, fontSize = 10)


forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",  Nodesize = "size",
             Target = "target", Value = "value", NodeID = "name", fontSize = 20,
             Group = "group",  zoom = TRUE, legend = TRUE)




MyClickScript <- 'alert("You clicked " + d.name + " which is in row " +
       (d.index + 1) +  " of your original R data frame");'

forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 1, zoom = FALSE,
             bounded = TRUE, clickAction = MyClickScript)











