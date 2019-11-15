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
    #Este parametro me sirve para ver si el archivo tiene cabeceras puede ser TRUE o FALSE
    initGrafo = function(ordDescendente,cabecerasEnArchivo) {
      #inicializa el grafo
      heuristicaList<<-list()
      dataFrameGrafo <- read.csv(nombreArchivo, sep = ",", header = cabecerasEnArchivo, stringsAsFactors = FALSE)
      dataAux <- dataFrameGrafo[order(as.character(dataFrameGrafo[[1]]),as.character(dataFrameGrafo[[2]]),na.last = TRUE,decreasing = ordDescendente),]
      #Mando a limpiar los datos que no me sirve, las filas que solo contienen la heuristica del nodo
      
      for (j in 1:length(dataAux[[1]])) {
        heuristicaList[[as.character(dataAux[[1]][[j]])]]<<-dataAux[[4]][[j]]
      }
      dataAux<-dataAux[dataAux[[2]]!="",]
      dataFrameGrafo<-dataFrameGrafo[dataFrameGrafo[[2]]!="",]
      
      for (k in 1:length(dataAux[[1]])) {
        agregarArista(as.character(dataAux[[1]][[k]]), as.character(dataAux[[2]][[k]]), dataAux[[3]][[k]],heuristicaList[[as.character(dataAux[[2]][[k]])]])
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
        
        
        dataIgraph<-grafica(extracciones,dataIgraph,FALSE)
        if(nivelBusqd[[extracciones]]$nivel< nivelBusqueda){
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
    }
   
  )
)

grafo <- grafoGene(nombreArchivo = "grafo1.csv")
datosIgraph<-grafo$initGrafo(FALSE,FALSE)
V(datosIgraph)$color <- "yellow"
grafo$busquedaProfundidadIterativa("s",list("z","a"),1,datosIgraph)

