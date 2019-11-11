library(structOfSearch)
library(igraph)
#Instalar la estructura de datos necesaria de los siguiente paquete de 
#github de elaboración propia https://github.com/JonnHenry/StructOfSearch.git

#Para utilzar se debe de instalar desde github con la siquiente linea de comando
#devtools::install_github("JonnHenry/StructOfSearch")


ifelse(require(igraph) == T, "Cargado", "igraph no cargado")
ifelse(require(structOfSearch) == T, "Cargado", "structOfSearch no cargado acceder al siguiente enlace para descargar https://github.com/JonnHenry/StructOfSearch.git")


#####################################################

grafica = function(vertice,data,nodoFinal){
  if(nodoFinal){
    V(data)[vertice]$color <- "blue"
  }else{
    V(data)[vertice]$color <- "red"
  }
  plot(data,layout=layout.reingold.tilford(data, root=1), edge.arrow.size=0.5,vertex.color=V(data)$color)
  legend(x = "topright",cex=0.7,bty = "n" ,pt.cex=1,legend = c("No visitado", "Visitado","Encontrado"), fill = c("yellow", "red","blue"), title = "Estados de un nodo")
  Sys.sleep(1)
  return(data)
}

nodoGene <- setRefClass("Nodo", fields = list(
  adyacentes = "list",
  id = "character"),
  methods = list(
    agregarAdyacente = function(nodoAdyacente, peso,heuristica) {
      adyacentes[[nodoAdyacente]] <<- list(id = nodoAdyacente, peso = peso, heuristica=heuristica)
    },
    obtenerAdyacentes = function() {
      return(adyacentes)
    }
  )
)

#Grafo de una sola dirección para todos los metodos excepto el bidireccional
grafoGene <- setRefClass("Grafo", fields = list(
  listNodos = "list",
  nombreArchivo = "character",
  listNodosVisitados = "list",
  listNombreNodos = "list",
  heuristicaList="list"),
  methods = list(
    agregarNodo = function(id) {
      if (is.null(listNombreNodos[[id]])) {
        listNombreNodos[[id]] <<- id
        listNodos[[id]] <<- nodoGene(id = id)
      }
    },
    getNodoAristas = function(id) {
      return(listNodos[[id]])
    },
    #Inicializa el grafo el parametro indica el orden si es FALSE entoces es ascendente 1,2,3 ...5 si es TRUE es descente 5,4,3,...1
    initGrafo = function(ordDescendente) {
      #inicializa el grafo
      heuristicaList<<-list()
      dataFrameGrafo <- read.csv(nombreArchivo, sep = ",", header = TRUE, stringsAsFactors = FALSE)
      dataPrueba <- dataFrameGrafo[order(as.character(dataFrameGrafo[[1]]),as.character(dataFrameGrafo[[2]]),na.last = TRUE,decreasing = ordDescendente),]
      
      for (j in 1:length(dataPrueba[[1]])) {
        heuristicaList[[as.character(dataPrueba[[1]][[j]])]]<<-dataPrueba[[4]][[j]]
      }
      
      dataFrameGrafo<-dataFrameGrafo[dataFrameGrafo[[2]]!="",]
      dataPrueba<-dataPrueba[dataPrueba[[2]]!="",]
      
      for (k in 1:length(dataPrueba[[1]])) {
        agregarArista(as.character(dataPrueba[[1]][[k]]), as.character(dataPrueba[[2]][[k]]), dataPrueba[[3]][[k]],heuristicaList[[as.character(dataPrueba[[2]][[k]])]])
      }
      
      return(graph_from_data_frame(dataFrameGrafo, directed = TRUE))
    },
    
    getNombreNodos = function() {
      return(listNombreNodos)
    },
    
    agregarArista = function(inicio, fin, peso,heuristica) {
      #Me agrega una arista para poder conectar los nodos
      
      agregarNodo(inicio)
      agregarNodo(fin)
      listNodos[[inicio]]$agregarAdyacente(fin, peso,heuristica)
      #Con la agregación de esta linea me ayuda a que los grafos esten doblemente conectado
      #y no sean grafos dirigidos
      #listNodos[[fin]]$agregarAdyacente(listNodos[[inicio]], peso)
    },
    
    getNodos = function() {
      return(listNombreNodos)
    },
    
    #Algoritmo para la busqueda en amplitud, nodoInicio por donde comienza la busqueda, lista de nodos a buscar, datos de Igraph para el grafico
    busquedaAmplitud = function(nodoInicio, listNodosBuscar,dataIgraph) {
      if (length(listNodosBuscar)==0){
        listNodosBuscar[[1]]<-""
      }
      listNodosVisitados <<- list()
      #Se ingresa una lista para buscar todos los elementos de la lista dada
      #El nodo inicio es de tipo character
      cola <- queue()
      names(listNodosBuscar) <- listNodosBuscar
      extracciones <- "--"
      cola$push(listNodos[[nodoInicio]]$id)
      while (TRUE) {
        if (!is.null(listNodosBuscar[[extracciones]])) {
          valor <- listNodosBuscar[[extracciones]]
          listNodosBuscar[[extracciones]] <- NULL
          
          if (length(listNodosBuscar) == 0) {
            cola$clean()
          }
          cat("\n ")
          cat(paste("--->El nodo encontrado es: ", valor, sep = " "))
          cat("\n ")
          dataIgraph<-grafica(extracciones,dataIgraph,TRUE)
        }
        cat("\n ")
        cat(paste("La extracción es: ", extracciones, sep = " "))
        cat("\n ")
        cat("La cola actual es: ", cola$look())
        cat("\n ")
        cat("********************************************************************")
        cat("\n ")
        
        if (is.null(cola$look()) | length(listNodosBuscar) == 0) {
          break
        }
        extracciones <- cola$pop()
        dataIgraph<-grafica(extracciones,dataIgraph,FALSE)
        
        while(!is.null(listNodosVisitados[[extracciones]])){
          cat("\n ")
          cat(paste("<El nodo", extracciones, "ya ha sido visitado, no se visitara!>", sep = " "))
          cat("\n ")
          cat("La cola actual es: ", cola$look())
          cat("\n ")
          extracciones <- cola$pop()
          dataIgraph<-grafica(extracciones,dataIgraph,FALSE)
          if (is.null(extracciones)) {
            break
          }
        }
        
        if (is.null(extracciones)) {
          break
        }
        nodosAdyacentes <- getNodoAristas(extracciones)$adyacentes
        for (nodo in nodosAdyacentes) {
          cola$push(nodo$id)
        }
        listNodosVisitados[[extracciones]] <<- "1"
      }
      if (length(listNodosBuscar) != 0 & !is.null(listNodosBuscar)) {
        if (listNodosBuscar[[1]]!=""){
          cat("\n ")
          cat("No se ha encontrado todos los nodos")
        }
        
      }
    },
    
    #Algoritmo de busqueda en profundidad usando una cola
    busquedaProfundidad = function(nodoInicio, listNodosBuscar,dataIgraph) {
      if (length(listNodosBuscar)==0){
        listNodosBuscar[[1]]<-""
      }
      listNodosVisitados <<- list()
      #Se ingresa una lista para buscar todos los elementos de la lista dada
      #El nodo inicio es de tipo character
      pila <- stackOwn()
      names(listNodosBuscar) <- listNodosBuscar
      extracciones <- "--"
      pila$push(listNodos[[nodoInicio]]$id)
      while (TRUE) {
        if (!is.null(listNodosBuscar[[extracciones]])) {
          valor <- listNodosBuscar[[extracciones]]
          listNodosBuscar[[extracciones]] <- NULL
          
          if (length(listNodosBuscar) == 0) {
            pila$clean()
          }
          cat("\n ")
          cat(paste("--->El nodo encontrado es: ", valor, sep = " "))
          cat("\n ")
          dataIgraph<-grafica(extracciones,dataIgraph,TRUE)
        }
        cat("\n ")
        cat(paste("La extracción es: ", extracciones, sep = " "))
        cat("\n ")
        cat("La pila actual es: ", pila$look())
        cat("\n ")
        cat("********************************************************************")
        cat("\n ")
        
        if (is.null(pila$look()) | length(listNodosBuscar) == 0) {
          break
        }
        extracciones <- pila$pop()
        dataIgraph<-grafica(extracciones,dataIgraph,FALSE)
        
        while(!is.null(listNodosVisitados[[extracciones]])){
          cat("\n ")
          cat(paste("<El nodo", extracciones, "ya ha sido visitado, no se visitara!>", sep = " "))
          cat("\n ")
          cat("La pila actual es: ", pila$look())
          cat("\n ")
          extracciones <- pila$pop()
          dataIgraph<-grafica(extracciones,dataIgraph,FALSE)
          if (is.null(extracciones)) {
            break
          }
        }
        
        if (is.null(extracciones)) {
          break
        }
        nodosAdyacentes <- getNodoAristas(extracciones)$adyacentes
        for (nodo in nodosAdyacentes) {
          pila$push(nodo$id)
        }
        listNodosVisitados[[extracciones]] <<- "1"
      }
      if (length(listNodosBuscar) != 0 & !is.null(listNodosBuscar)) {
        if (listNodosBuscar[[1]]!=""){
          cat("\n ")
          cat("No se ha encontrado todos los nodos")
        }
        
      }
    },
    
    
    busquedaProfundidadIterativa = function(nodoInicio, listNodosBuscar, nivelBusqueda,dataIgraph) {
      if (length(listNodosBuscar)==0){
        listNodosBuscar[[1]]<-""
      }
      nivel<-0
      nivelBusqd<-list()
      listNodosVisitados <<- list()
      #Se ingresa una lista para buscar todos los elementos de la lista dada
      #El nodo inicio es de tipo character
      pila <- stackOwn()
      names(listNodosBuscar) <- listNodosBuscar
      extracciones <- "--"
      nivelBusqd[[nodoInicio]]<-list(id=nodoInicio, nivel=0)
      pila$push(listNodos[[nodoInicio]]$id)
      while (TRUE) {
        if (!is.null(listNodosBuscar[[extracciones]])) {
          valor <- listNodosBuscar[[extracciones]]
          listNodosBuscar[[extracciones]] <- NULL
          
          if (length(listNodosBuscar) == 0) {
            pila$clean()
          }
          cat("\n ")
          cat(paste("--->El nodo encontrado es: ", valor, sep = " "))
          cat("\n ")
          dataIgraph<-grafica(extracciones,dataIgraph,TRUE)
        }
        cat("\n ")
        cat(paste("La extracción es: ", extracciones, sep = " "))
        cat("\n ")
        cat("La pila actual es: ", pila$look())
        cat("\n ")
        cat(paste("El nodo de la extracción se encuentra en el nivel:",nivelBusqd[[extracciones]]$nivel, sep = " "))
        cat("\n ")
        cat("********************************************************************")
        cat("\n ")
        
        if (is.null(pila$look()) | length(listNodosBuscar) == 0) {
          break
        }
        
        
        extracciones <- pila$pop()
        dataIgraph<-grafica(extracciones,dataIgraph,FALSE)
        if(nivelBusqd[[extracciones]]$nivel< nivelBusqueda){
          
          while(!is.null(listNodosVisitados[[extracciones]])){
            cat("\n ")
            cat(paste("<El nodo", extracciones, "ya ha sido visitado, no se visitara!>", sep = " "))
            cat("\n ")
            cat("La pila actual es: ", pila$look())
            cat("\n ")
            extracciones <- pila$pop()
            dataIgraph<-grafica(extracciones,dataIgraph,FALSE)
            if (is.null(extracciones)) {
              break
            }
          }
          
          if (is.null(extracciones)) {
            break
          }
          
          nodosAdyacentes <- getNodoAristas(extracciones)$adyacentes
          for (nodo in nodosAdyacentes) {
            pila$push(nodo$id)
            nivelBusqd[[nodo$id]]<-list(id=nodo$id, nivel=nivelBusqd[[extracciones]]$nivel+1)
            
          }
          listNodosVisitados[[extracciones]] <<- "1"
          nivel<-nivel+1
        }
      }
      if (length(listNodosBuscar) != 0 & !is.null(listNodosBuscar)) {
        if (listNodosBuscar[[1]]!=""){
          cat("\n ")
          cat("No se ha encontrado todos los nodos")
        }
        
      }
    },
    
    #Algoritmo para la busqueda en costo uniforme
    busquedaCostoUniforme = function(nodoInicio, listNodosBuscar,dataIgraph) {
      #Se ingresa una lista para buscar todos los elementos de la lista dada
      #El nodo inicio es de tipo character
      if (length(listNodosBuscar)==0){
        listNodosBuscar[[1]]<-""
      }
      listNodosVisitados <<- list()
      
      #Se asigna un nodo basura para que los nombres del dataframe y sus propiedades no se pierdan cuando
      #no haya elementos
      cola<-queueSorted(queue=data.frame(nodo=character(),peso=numeric(),costo=numeric(),stringsAsFactors = FALSE))
      names(listNodosBuscar) <- listNodosBuscar
      cola$push(c("--",0,0))
      extracciones<-cola$pop()
      cola$push(c(nodoInicio,0,0))
      
      while (TRUE) {
        if (!is.null(listNodosBuscar[[extracciones$nodo]])) {
          valor <- listNodosBuscar[[extracciones$nodo]]
          listNodosBuscar[[extracciones$nodo]] <- NULL
          
          if (length(listNodosBuscar) == 0) {
            cola$clean()
          }
          cat("\n ")
          cat(paste("--->El nodo encontrado es: ", valor, sep = " "))
          cat("\n ")
          dataIgraph<-grafica(extracciones$nodo,dataIgraph,TRUE)
        }
        cat("\n ")
        cat(paste("La extracción es:", extracciones$nodo,"con el costo de:",extracciones$costo, sep = " "))
        cat("\n ")
        cat("La cola actual es: ")
        cat("\n ")
        print(cola$look(),row.names = FALSE)
        cat("\n ")
        cat("********************************************************************")
        cat("\n ")
        
        if (is.null(cola$look()) | length(listNodosBuscar) == 0) {
          break
        }
        
        extracciones <- cola$pop()
        dataIgraph<-grafica(extracciones$nodo,dataIgraph,FALSE)
        
        while(!is.null(listNodosVisitados[[extracciones$nodo]])){
          cat(paste("<El nodo", extracciones$nodo, "ya ha sido visitado, no se visitara!>", sep = " "))
          cat("\n ")
          cat("La cola actual es:")
          cat("\n ")
          print(cola$look(),row.names = FALSE)
          extracciones <- cola$pop()
          cat("\n ")
          dataIgraph<-grafica(extracciones$nodo,dataIgraph,FALSE)
          if (is.null(extracciones)) {
            break
          }
        }
        
        if (is.null(extracciones)) {
          break
        }
        
        nodosAdyacentes <- getNodoAristas(extracciones$nodo)$adyacentes
        numExtracciones<-as.numeric(extracciones$peso)
        for (nodo in nodosAdyacentes) {
          numNodo<-as.numeric(nodo$peso)
          cola$push(c(nodo$id,numNodo,(numExtracciones+numNodo)))
        }
        cola$sort("costo")
        listNodosVisitados[[extracciones$nodo]] <<- "1"
      }
      if (length(listNodosBuscar) != 0 & !is.null(listNodosBuscar)) {
        if (listNodosBuscar[[1]]!=""){
          cat("\n ")
          cat("No se ha encontrado todos los nodos")
        }
        
      }
    },
    
    #*************Algortimos Heuristicos****************
    metodoGradiente = function(nodoInicio, listNodosBuscar,dataIgraph) {
      #Se ingresa una lista para buscar todos los elementos de la lista dada
      #El nodo inicio es de tipo character
      if (length(listNodosBuscar)==0){
        listNodosBuscar[[1]]<-""
      }
      listNodosVisitados <<- list()
      
      #Se asigna un nodo basura para que los nombres del dataframe y sus propiedades no se pierdan cuando
      #no haya elementos
      cola<-queueSorted(queue=data.frame(nodo=character(),heuristica=numeric(),stringsAsFactors = FALSE))
      names(listNodosBuscar) <- listNodosBuscar
      cola$push(c("--",0))
      extracciones<-cola$pop()
      cola$push(c(nodoInicio,heuristicaList[[nodoInicio]]))
      
      while (TRUE) {
        if (!is.null(listNodosBuscar[[extracciones$nodo]])) {
          valor <- listNodosBuscar[[extracciones$nodo]]
          listNodosBuscar[[extracciones$nodo]] <- NULL
          
          if (length(listNodosBuscar) == 0) {
            cola$clean()
          }
          cat("\n ")
          cat(paste("--->El nodo encontrado es: ", valor, sep = " "))
          cat("\n ")
          dataIgraph<-grafica(extracciones$nodo,dataIgraph,TRUE)
        }
        cat("\n ")
        cat(paste("La extracción es:", extracciones$nodo,"con la heuristica de:",extracciones$heuristica, sep = " "))
        cat("\n ")
        cat("La cola actual es:")
        cat("\n ")
        print(cola$look(),row.names = FALSE)
        cat("\n ")
        cat("********************************************************************")
        cat("\n ")
        
        if (is.null(cola$look()) | length(listNodosBuscar) == 0) {
          break
        }
        
        extracciones <- cola$pop()
        dataIgraph<-grafica(extracciones$nodo,dataIgraph,FALSE)
        cola$clean()
        
        while(!is.null(listNodosVisitados[[extracciones$nodo]])){
          cat(paste("<El nodo", extracciones$nodo, "ya ha sido visitado, no se visitara!>", sep = " "))
          cat("\n ")
          cat("La cola actual es:")
          cat("\n ")
          print(cola$look(),row.names = FALSE)
          extracciones <- cola$pop()
          cat("\n ")
          dataIgraph<-grafica(extracciones$nodo,dataIgraph,FALSE)
          if (is.null(extracciones)) {
            break
          }
        }
        
        if (is.null(extracciones)) {
          break
        }
        
        nodosAdyacentes <- getNodoAristas(extracciones$nodo)$adyacentes
        
        for (nodo in nodosAdyacentes) {
          numNodo<-as.numeric(nodo$heuristica)
          cola$push(c(nodo$id,(numNodo)))
        }
        cola$sort("heuristica")
        listNodosVisitados[[extracciones$nodo]] <<- "1"
      }
      if (length(listNodosBuscar) != 0 & !is.null(listNodosBuscar)) {
        if (listNodosBuscar[[1]]!=""){
          cat("\n ")
          cat("No se ha encontrado todos los nodos")
        }
        
      }
    },
    
    #Algoritmo primero el mejor
    primeroMejor = function(nodoInicio, listNodosBuscar,dataIgraph) {
      #Se ingresa una lista para buscar todos los elementos de la lista dada
      #El nodo inicio es de tipo character
      if (length(listNodosBuscar)==0){
        listNodosBuscar[[1]]<-""
      }
      listNodosVisitados <<- list()
      
      #Se asigna un nodo basura para que los nombres del dataframe y sus propiedades no se pierdan cuando
      #no haya elementos
      cola<-queueSorted(queue=data.frame(nodo=character(),heuristica=numeric(),stringsAsFactors = FALSE))
      names(listNodosBuscar) <- listNodosBuscar
      cola$push(c("--",0))
      extracciones<-cola$pop()
      cola$push(c(nodoInicio,heuristicaList[[nodoInicio]]))
      
      while (TRUE) {
        if (!is.null(listNodosBuscar[[extracciones$nodo]])) {
          valor <- listNodosBuscar[[extracciones$nodo]]
          listNodosBuscar[[extracciones$nodo]] <- NULL
          
          if (length(listNodosBuscar) == 0) {
            cola$clean()
          }
          cat("\n ")
          cat(paste("--->El nodo encontrado es: ", valor, sep = " "))
          cat("\n ")
          dataIgraph<-grafica(extracciones$nodo,dataIgraph,TRUE)
        }
        cat("\n ")
        cat(paste("La extracción es:", extracciones$nodo,"con la heuristica de:",extracciones$heuristica, sep = " "))
        cat("\n ")
        cat("La cola actual es:")
        cat("\n ")
        print(cola$look(),row.names = FALSE)
        cat("\n ")
        cat("********************************************************************")
        cat("\n ")
        
        if (is.null(cola$look()) | length(listNodosBuscar) == 0) {
          break
        }
        
        extracciones <- cola$pop()
        dataIgraph<-grafica(extracciones$nodo,dataIgraph,FALSE)
        
        while(!is.null(listNodosVisitados[[extracciones$nodo]])){
          cat(paste("<El nodo", extracciones$nodo, "ya ha sido visitado, no se visitara!>", sep = " "))
          cat("\n ")
          cat("La cola actual es:")
          cat("\n ")
          print(cola$look(),row.names = FALSE)
          cat("\n ")
          extracciones <- cola$pop()
          dataIgraph<-grafica(extracciones$nodo,dataIgraph,FALSE)
          if (is.null(extracciones)) {
            break
          }
        }
        
        if (is.null(extracciones)) {
          break
        }
        
        nodosAdyacentes <- getNodoAristas(extracciones$nodo)$adyacentes
        
        for (nodo in nodosAdyacentes) {
          numNodo<-as.numeric(nodo$heuristica)
          cola$push(c(nodo$id,(numNodo)))
        }
        cola$sort("heuristica")
        listNodosVisitados[[extracciones$nodo]] <<- "1"
      }
      if (length(listNodosBuscar) != 0 & !is.null(listNodosBuscar)) {
        if (listNodosBuscar[[1]]!=""){
          cat("\n ")
          print("Paso")
          cat("No se ha encontrado todos los nodos")
        }
        
      }
    },
    
    aEstrella = function(nodoInicio, listNodosBuscar,dataIgraph) {
      #Se ingresa una lista para buscar todos los elementos de la lista dada
      #El nodo inicio es de tipo character
      if (length(listNodosBuscar)==0){
        listNodosBuscar[[1]]<-""
      }
      listNodosVisitados <<- list()
      
      #Se asigna un nodo basura para que los nombres del dataframe y sus propiedades no se pierdan cuando
      #no haya elementos
      cola<-queueSorted(queue=data.frame(nodo=character(),peso=numeric(),heuristica=numeric(),suma=numeric(),stringsAsFactors = FALSE))
      names(listNodosBuscar) <- listNodosBuscar
      cola$push(c("--",0,0,0))
      extracciones<-cola$pop()
      cola$push(c(nodoInicio,0,heuristicaList[[nodoInicio]],heuristicaList[[nodoInicio]]))
      
      while (TRUE) {
        if (!is.null(listNodosBuscar[[extracciones$nodo]])) {
          valor <- listNodosBuscar[[extracciones$nodo]]
          listNodosBuscar[[extracciones$nodo]] <- NULL
          
          if (length(listNodosBuscar) == 0) {
            cola$clean()
          }
          cat("\n ")
          cat(paste("--->El nodo encontrado es: ", valor, sep = " "))
          cat("\n ")
          dataIgraph<-grafica(extracciones$nodo,dataIgraph,TRUE)
        }
        cat("\n ")
        cat(paste("La extracción es:", extracciones$nodo,"con la suma de:",extracciones$suma, sep = " "))
        cat("\n ")
        cat("La cola actual es:")
        cat("\n ")
        print(cola$look(),row.names = FALSE)
        cat("\n ")
        cat("********************************************************************")
        cat("\n ")
        
        if (is.null(cola$look()) | length(listNodosBuscar) == 0) {
          break
        }
        
        extracciones <- cola$pop()
        dataIgraph<-grafica(extracciones$nodo,dataIgraph,FALSE)
          while(!is.null(listNodosVisitados[[extracciones$nodo]])){
            cat(paste("<El nodo", extracciones$nodo, "ya ha sido visitado, no se visitara!>", sep = " "))
            cat("\n ")
            cat("La cola actual es:")
            cat("\n ")
            print(cola$look(),row.names = FALSE)
            extracciones <- cola$pop()
            cat("\n ")
            dataIgraph<-grafica(extracciones$nodo,dataIgraph,FALSE)
            if (is.null(extracciones)) {
              break
            }
          }
          
        if (is.null(extracciones)) {
          break
        }
        
        nodosAdyacentes <- getNodoAristas(extracciones$nodo)$adyacentes
        #print(nodosAdyacentes)
        
        for (nodo in nodosAdyacentes) {
          numHeuristica<-as.numeric(nodo$heuristica)
          numPeso<-as.numeric(nodo$peso)
          cola$push(c(nodo$id,numPeso,numHeuristica,(numHeuristica+numPeso)))
        }
        cola$sort("suma")
        listNodosVisitados[[extracciones$nodo]] <<- "1"
        
        
      }
      if (length(listNodosBuscar) != 0 & !is.null(listNodosBuscar)) {
        if (listNodosBuscar[[1]]!=""){
          cat("\n ")
          cat("No se ha encontrado todos los nodos")
        }
        
        
      }
    }
  )
)

grafo <- grafoGene(nombreArchivo = "prueba.csv")
datosIgraph<-grafo$initGrafo(FALSE)
V(datosIgraph)$color <- "yellow"
grafo$busquedaProfundidadIterativa("A",list(),2,datosIgraph)
