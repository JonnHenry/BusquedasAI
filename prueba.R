library(structOfSearch)
library(igraph)
#Instalar la estructura de datos necesaria de los siguiente paquete de 
#github que es de elaboración propia https://github.com/JonnHenry/StructOfSearch.git

#Para utilzar se debe de instalar desde github con la siquiente linea de comando
#devtools::install_github("JonnHenry/StructOfSearch")

ifelse(require(igraph) == T, "Cargado", "igraph no cargado")
ifelse(require(structOfSearch) == T, "Cargado", "structOfSearch no cargado acceder al siguiente enlace para descargar https://github.com/JonnHenry/StructOfSearch.git")

#####################################################
ploteo = function(vertice,data,nodoFinal){
  if(nodoFinal){
    V(data)[vertice]$color <- "blue"
  }else{
    V(data)[vertice]$color <- "red"
  }
  plot(data, layout=layout.reingold.tilford(data, root=1), 
       vertex.color=V(data)$color)
  legend(x = "topright",cex=0.7,bty = "n" ,pt.cex=1,legend = c("No visitado", "Visitado","Encontrado"), fill = c("yellow", "red","blue"), 
         title = "Estados de un nodo")
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
grafoGene <- setRefClass("Grafo",fields = list(
  cantNodos = "numeric",
  listNodos = "list",
  heuristicaList="list",
  nombreArchivo = "character",
  listNodosVisitados = "list",
  listNombreNodos = "list"),
  methods = list(
    agregarNodo = function(id) {
      cantNodos <<- cantNodos + 1
      if (is.null(listNombreNodos[[id]])) {
        nodoNuevo <- nodoGene(id = id)
        listNombreNodos[[id]] <<- id
        listNodos[[id]] <<- nodoNuevo
      }
    },
    
    getNodoAristas = function(id) {
      return(listNodos[[id]])
    },
    
    initGrafo = function() {
      #inicializa el grafo
      heuristicaList<<-list()
      dataFrameGrafo <- read.csv(nombreArchivo, sep = ",", header = TRUE, stringsAsFactors = FALSE)
      dataFrameGrafo<-dataFrameGrafo[order(dataFrameGrafo$nodoInicio,dataFrameGrafo$nodoFin,na.last = TRUE,decreasing = FALSE),]
      
      for (j in 1:length(dataFrameGrafo[[1]])) {
        heuristicaList[[as.character(dataFrameGrafo[[1]][[j]])]]<<-dataFrameGrafo[[4]][[j]]
      }
      
      dataFrameGrafo<-dataFrameGrafo[dataFrameGrafo[[2]]!="",]

      for (k in 1:length(dataFrameGrafo[[1]])) {
          agregarArista(as.character(dataFrameGrafo[[1]][[k]]), as.character(dataFrameGrafo[[2]][[k]]), dataFrameGrafo[[3]][[k]],heuristicaList[[as.character(dataFrameGrafo[[1]][[k]])]])
      }
      
      return(graph_from_data_frame(dataFrameGrafo, directed = TRUE))
    },
    
    agregarArista = function(inicio, fin, peso,heuristica) {
      #Me agrega una arista para poder conectar los nodos
      
      agregarNodo(inicio)
      agregarNodo(fin)
      listNodos[[inicio]]$agregarAdyacente(fin, peso,heuristica)
    },
    
    #Algoritmo de busqueda en profundidad usando una cola
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
          dataIgraph<-ploteo(extracciones,dataIgraph,TRUE)
        }
        cat("\n ")
        cat(paste("La extracción es: ", extracciones, sep = " "))
        cat("\n ")
        cat("La pila actual es: ", pila$look())
        cat("\n ")
        cat(paste("El nodo de la extracción se encuentre en el nivel:",nivelBusqd[[extracciones]]$nivel, sep = " "))
        cat("\n ")
        cat("********************************************************************")
        cat("\n ")
        
        if (is.null(pila$look()) | length(listNodosBuscar) == 0) {
          break
        }
        
        
        extracciones <- pila$pop()
        dataIgraph<-ploteo(extracciones,dataIgraph,FALSE)
        
        if(nivelBusqd[[extracciones]]$nivel< nivelBusqueda){
          
          if (!is.null(listNodosVisitados[[extracciones]])) {
            print(paste("El nodo : ", extracciones, " ya ha sido visitado no se visitara!", sep = " "))
            extracciones <- pila$pop()
          }
          nodosAdyacentes <- getNodoAristas(extracciones)$adyacentes
          for (nodo in nodosAdyacentes) {
            pila$push(nodo$id)
            nivelBusqd[[nodo$id]]<-list(id=nodo$id, nivel=nivelBusqd[[extracciones]]$nivel+1)
            
          }
          listNodosVisitados[[extracciones]] <<- "1"
          nivel<-nivel+1
        }else{
          
        }
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

grafo <- grafoGene(cantNodos = 0, nombreArchivo = "prueba.csv")
datosIgraph<-grafo$initGrafo()
V(datosIgraph)$color <- "yellow"
grafo$busquedaProfundidadIterativa("A",list("A"),1,datosIgraph)
