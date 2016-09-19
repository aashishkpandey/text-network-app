#################################################
#     Text Network App    #
#################################################

shinyServer(function(input, output,session) {
  
  library("igraph")
  library("tm")  
  #---------------------------------------------
  text.clean1 = function(x)                          # text data
  { 
    # x = gsub("[^[:alnum:]['-][\\s]", "-", x)        # replace intra-word space with dash
    x  =  gsub("<.*?>", " ", x)                  # regex for removing HTML tags
    x  =  iconv(x, "latin1", "ASCII", sub=" ")   # Keep only ASCII characters
    x  =  gsub("[^[:alnum:]['-]", " ", x)       # keep apostrophe 
    x  =  tolower(x)                          # convert to lower case characters
    x  =  removeNumbers(x)                    # removing numbers
    x  =  stripWhitespace(x)                  # removing white space
    x  =  gsub("^\\s+|\\s+$", " ", x)          # remove leading and trailing white space
    return(x)
  }
  
  plot.one.mode <- function(mat, network.name, cutoff,cex,cex2){
    
    mat.network = mat %*% t(mat)
    
    mat.network[upper.tri(mat.network, diag = T)] = NA
    
    s = quantile(setdiff(as.vector(mat.network*lower.tri(mat.network)),NA),cutoff)
    mat.network[is.na(mat.network)] = 0
    
    mat.adj = mat.network
    mat.adj[mat.adj < s] = 0
    mat.adj[mat.adj > 0] = 1
    
    
    graph1e = graph.adjacency(mat.adj, mode = "undirected")
    
    E(graph1e)$weight <- count.multiple(graph1e)
    graph1e <- simplify(graph1e)
    
    # Set vertex attributes
    V(graph1e)$label = V(graph1e)$name
    V(graph1e)$label.color = rgb(0,0,.2,.8)
    V(graph1e)$label.cex = cex
    V(graph1e)$size = cex2
    V(graph1e)$frame.color = NA
    V(graph1e)$color = rgb(0,0,1,.5)
    
    # Set edge gamma according to edge weight
    egam = (log(E(graph1e)$weight)+.3)/max(log(E(graph1e)$weight)+.3)
    E(graph1e)$color = rgb(.5,.5,0,egam)
    
    plot(graph1e, main = network.name, layout=layout.kamada.kawai)
    # plot(graph1e, main = "layout.fruchterman.reingold", layout=layout.fruchterman.reingold)
  }
  distill.cog <- function(dtm1, s, k1, network.name){
    # s = 5  # no. of seed nodes
    # k1 = 7   # max no. of connections
    # n1 = 100 # restrict to the top n1 words
    
    mat = as.matrix((dtm1))  # input dtm here
    mat1 = mat %*% t(mat)    # build 1 mode term term matrix
    
    a = colSums(mat1)  # collect colsums into a vector obj a
    b = order(-a)     # nice syntax for ordering vector in decr order  
    mat2 = mat1[b,b]  # 
    diag(mat2) =  0
    
    ## +++ go row by row and find top k adjacencies +++ ##
    
    wc = NULL
    for (i1 in 1:s){ 
      thresh1 = mat2[i1,][order(-mat2[i1, ])[k1]]
      mat2[i1, mat2[i1,] < thresh1] = 0   # wow. didn't need 2 use () in the subset here.
      mat2[i1, mat2[i1,] > 0 ] = 1
      word = names(mat2[i1, mat2[i1,] > 0])
      mat2[(i1+1):nrow(mat2), match(word,colnames(mat2))] = 0
      wc = c(wc,word)
    } # i1 loop ends
    mat3 = mat2[match(wc, colnames(mat2)), match(wc, colnames(mat2))]
    ord = colnames(mat2)[which(!is.na(match(colnames(mat2), colnames(mat3))))]  # removed any NAs from the list
    mat4 = mat3[match(ord, colnames(mat3)), match(ord, colnames(mat3))]
    graph <- graph.adjacency(mat4, mode = "undirected", weighted=T)    # Create Network object
    graph = simplify(graph)  
    
    V(graph)$color[1:s] = "gray"
    V(graph)$color[s+1:length(V(graph))] = adjustcolor("white", alpha.f = 0.7)
    plot(graph, vertex.label.cex = 1.1, main = network.name, layout=layout.kamada.kawai)
  } # func ends
  #---------------------------------------------
  
  Dataset <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      Dataset <- read.csv(input$file$datapath ,header=TRUE, sep = ",", stringsAsFactors = F)
#      row.names(Dataset) = Dataset[,1]
#      Dataset = Dataset[,2:ncol(Dataset)]
      return(Dataset)
    }
  })
  #---------------------------------------------  
    dtm = reactive({
      
      text = text.clean1(Dataset()[,2])
      t = Corpus(VectorSource(text))
      tdm = TermDocumentMatrix(t) 
      
      Brands_DTM <- t(as.matrix(tdm))
      
      rownames(Brands_DTM) <- Dataset()[,1]
      return(Brands_DTM)
      })
    #---------------------------------------------  
    
    dtm1 <- reactive({
      
      
      dtm1 = dtm()[order(rowSums(dtm()),decreasing = T),]
      if (input$npoint > nrow(dtm1)){
        n = nrow(dtm1)
      } else {
        n = input$npoint
      }
      
      dtm1 = dtm1[1:n,]
      return(dtm1)
      })
    #-------------------------------------------
    dtm2 <- reactive({
      
      
      dtm2 = dtm()[,order(colSums(dtm()),decreasing = T)]
      if (input$npoint > ncol(dtm2)){
        n = ncol(dtm2)
      } else {
        n = input$npoint
      }
      
      dtm2 = dtm2[,1:n]
      return(dtm2)
    })
    #-------------------------------------------
  output$graph1 <- renderPlot({
  plot.one.mode(dtm1(), "Doc-Doc", input$cutoff,input$cex,input$cex2)
  })
  output$graph2 <- renderPlot({
  plot.one.mode(t(dtm2()), "Term-Term",input$cutoff,input$cex,input$cex2)
  })
  output$graph3 <- renderPlot({
  distill.cog(dtm(), input$nodes, input$connection, "Doc-Doc")
  })
  output$graph4 <- renderPlot({
  distill.cog(t(dtm()),input$nodes, input$connection, "Term-Term")
  })
    
  })

