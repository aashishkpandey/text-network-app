#################################################
#     Text Network App    #
#################################################

shinyServer(function(input, output,session) {
  library("shiny")
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
  distill.cog <- function(dtm1, s, k1, network.name,cex,cex2){
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
    
    V(graph)$color[1:s] = "darkgoldenrod2"
    V(graph)$color[s+1:length(V(graph))] = adjustcolor("cyan3", alpha.f = 0.7)
    plot(graph,
         vertex.label.cex = cex, 
         vertex.label.color='black',		#the color of the name labels
         vertex.size = cex2,     # size of the vertex
         main = network.name, 
         layout=layout.kamada.kawai)
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
      
      stp_word1 = stopwords('english')
      stp_word2 = readLines("data/stopwords.txt")
      comn  = unique(c(stp_word1, stp_word2))
      stp_word = unique(c(gsub("'","",comn),comn))
      sto = unique(c(stp_word)) #,unlist(strsplit(input$stopw,","))
      
      text = text.clean1(Dataset()[,2])
      myCorpus = tm_map(Corpus(VectorSource(text)), removeWords,c(sto))
      # myCorpus = tm_map(myCorpus, stripWhitespace)   # removes white space
      # myCorpus = as.character(unlist(myCorpus))
      # x1 = myCorpus
      # 
      # ngram <- function(x1) NGramTokenizer(x1, Weka_control(min = 2, max = 2))  
      # 
      # tdm0 <- TermDocumentMatrix(x1, control = list(tokenize = ngram,
      #                                               tolower = TRUE, 
      #                                               removePunctuation = TRUE,
      #                                               removeNumbers = TRUE,
      #                                               stopwords = TRUE ))
      # tdm = tdm0; rm('tdm0')
      # a1 = apply(tdm, 1, sum)  
      # a2 = ((a1 > 3))
      # tdm.new = tdm[a2, ]
      # rm('a1','a2','tdm')
      # 
      # dim(tdm.new)    # reduced tdm
      # x1mat = t(tdm.new)    # don't do tfidf, not mentioned anywhere for topic modeling.
      # dim(x1mat);    # store[i1, 5] = ncol(x2mat);
      # 
      # test = colnames(x1mat); 
      # test1 = gsub(" ",".", test);  # replace spaces with dots
      # colnames(x1mat) = test1
      # 
      # a11 = apply(x1mat, 2, sum)
      # a12 = order(a11, decreasing = T)
      # a13 = as.matrix(a11[a12])
      # 
      # #x1 = tm_map(x1, stripWhitespace)
      # x1 = unlist(lapply(x1, content)) 
      # for (i in 1:nrow(a13)){    
      #   focal.term = gsub("\\.", " ", rownames(a13)[i])
      #   replacement.term = gsub(" ", "-", focal.term)
      #   replacement.term=paste("",replacement.term,"")
      #   x1 = gsub(focal.term, replacement.term, x1)  
      #   
      # }	# now, our x corpus has the top 400 bigrams encoded as unigrams
      
      # progress$set(message = 'TDM creation in progress',
      #              detail = 'This may take a while...')
      # progress$set(value = 4)

      # x1 = Corpus(VectorSource(x1))    # Constructs a source for a vector as input
      tdm = TermDocumentMatrix(myCorpus)
      
      #tdm = TermDocumentMatrix(t) 
      
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
    if (is.null(input$file)) { return(NULL) }
    else{
  plot.one.mode(dtm1(), "Doc-Doc", input$cutoff,input$cex,input$cex2)
    }
  })
  output$graph2 <- renderPlot({
    if (is.null(input$file)) { return(NULL) }
    else{
  plot.one.mode(t(dtm2()), "Term-Term",input$cutoff,input$cex,input$cex2)
    }
  })
  output$graph3 <- renderPlot({
    if (is.null(input$file)) { return(NULL) }
    else{
  distill.cog(dtm(), input$nodes, input$connection, "Doc-Doc",input$cex,input$cex2)
    }
  })
  output$graph4 <- renderPlot({     if (is.null(input$file)) { return(NULL) }
    else{
  distill.cog(t(dtm()),input$nodes, input$connection, "Term-Term",input$cex,input$cex2)
    }
  })
    
  })

