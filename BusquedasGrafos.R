library(structOfSearch)
library(igraph)
#Instalar la estructura de datos necesaria de los siguiente paquete de 
#github de elaboración propia https://github.com/JonnHenry/StructOfSearch.git

#Para utilzar se debe de instalar desde github con la siquiente linea de comando
#devtools::install_github("JonnHenry/StructOfSearch")


ifelse(require(igraph) == T, "Cargado", "igraph no cargado")
ifelse(require(structOfSearch) == T, "Cargado", "structOfSearch no cargado acceder al siguiente enlace para descargar https://github.com/JonnHenry/StructOfSearch.git")

#####################################################
#Para el calculo de la profundidad de todos los nodos a buscar
profudidadMeta=function(dataframeIgraph,nodosBuscar,nodoInicial){
  if (length(nodosBuscar)==0){
    nodosBuscar<-nodoInicial
  }
  distancias <- distances(dataframeIgraph, nodoInicial)
  costo<-0
  for (nodo  in nodosBuscar){
    costo<-costo+distancias[, nodo]
  }
  return(round(costo/length(nodosBuscar)))
}

#Para poder graficar cada ves lo que se esta haciendo con el grafo
#El parametro pausas es para que se detenga en instantes o todo de una si es TRUE se ve deteniendo poco a poco 
#Si es falso el mismo no se detiens y va de corrido
grafica = function(vertice,data,nodoFinal,pausas){
  if(nodoFinal){
    V(data)[vertice]$color <- "blue"
  }else{
    V(data)[vertice]$color <- "red"
  }
  plot(data,layout=layout.reingold.tilford(data, root=1), edge.arrow.size=0.6,vertex.color=V(data)$color)
  legend(x = "topright",cex=0.7,bty = "n" ,pt.cex=1,legend = c("No visitado", "Visitado","Encontrado"), fill = c("yellow", "red","blue"), title = "Estados de un nodo")
  if (pausas){
    Sys.sleep(2)
  }
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
      listNodos[[inicio]]$agregarAdyacente(fin,peso,heuristica)
      #Con la agregación de esta linea me ayuda a que los grafos esten doblemente conectado
      #y no sean grafos dirigidos
      #listNodos[[fin]]$agregarAdyacente(listNodos[[inicio]], peso)
    },
    
    getNodos = function() {
      return(listNombreNodos)
    },
    
    #Algoritmo para la busqueda en amplitud, nodoInicio por donde comienza la busqueda, lista de nodos a buscar, datos de Igraph para el grafico
    busquedaAmplitud = function(nodoInicio, listNodosBuscar,dataIgraph,pausas) {
      t <- proc.time()
      maxHijos<-max(degree(dataIgraph,mode="out"))
      profundidadGrafo <- max(distances(dataIgraph, nodoInicio))
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
          dataIgraph<-grafica(extracciones,dataIgraph,TRUE,pausas)
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
        
        
        while(!is.null(listNodosVisitados[[extracciones]])){
          cat("\n ")
          cat(paste("<El nodo", extracciones, "ya ha sido visitado, no se visitara!>", sep = " "))
          cat("\n ")
          cat("La cola actual es: ", cola$look())
          cat("\n ")
          extracciones <- cola$pop()
          if (is.null(extracciones)) {
            break
          }
          if (is.null(listNodosVisitados[[extracciones]])){
            dataIgraph<-grafica(extracciones,dataIgraph,FALSE,pausas)
          }
        }
        
        if (is.null(extracciones)) {
          break
        }
        dataIgraph<-grafica(extracciones,dataIgraph,FALSE,pausas)
        nodosAdyacentes <- getNodoAristas(extracciones)$adyacentes
        for (nodo in nodosAdyacentes) {
          cola$push(nodo$id)
        }
        listNodosVisitados[[extracciones]] <<- "1"
      }
      if (length(listNodosBuscar) != 0 & !is.null(listNodosBuscar)) {
        if (listNodosBuscar[[1]]!=""){
          cat("\n ")
          message("No se ha encontrado todos los nodos en el algoritmo de amplitud")
          cat("\n ")
        }
      }
      tiempo<-proc.time() - t
      complejidad=as.character(maxHijos^profundidadGrafo)
      resultados<-data.frame("Algoritmo"=c("Amplitud"),"Temporal"=c(complejidad),"Espacial"=c(complejidad),"Tiempo computacional"=c(tiempo[["elapsed"]]),"Tipo"=c("Ciegas"))
      return(resultados)
    },
    
    #Algoritmo de busqueda en profundidad usando una cola
    busquedaProfundidad = function(nodoInicio, listNodosBuscar,dataIgraph,pausas) {
      t <- proc.time()
      maxHijos<-max(degree(dataIgraph,mode="out"))
      profundidadGrafo <- max(distances(dataIgraph, nodoInicio))
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
          dataIgraph<-grafica(extracciones,dataIgraph,TRUE,pausas)
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
        
        
        while(!is.null(listNodosVisitados[[extracciones]])){
          cat("\n ")
          cat(paste("<El nodo", extracciones, "ya ha sido visitado, no se visitara!>", sep = " "))
          cat("\n ")
          cat("La pila actual es: ", pila$look())
          cat("\n ")
          extracciones <- pila$pop()
          if (is.null(extracciones)) {
            break
          }
          if (is.null(listNodosVisitados[[extracciones]])){
            dataIgraph<-grafica(extracciones,dataIgraph,FALSE,pausas)
          }
          
        }
        
        if (is.null(extracciones)) {
          break
        }
        
        dataIgraph<-grafica(extracciones,dataIgraph,FALSE,pausas)
        nodosAdyacentes <- getNodoAristas(extracciones)$adyacentes
        for (nodo in nodosAdyacentes) {
          pila$push(nodo$id)
        }
        listNodosVisitados[[extracciones]] <<- "1"
      }
      if (length(listNodosBuscar) != 0 & !is.null(listNodosBuscar)) {
        if (listNodosBuscar[[1]]!=""){
          cat("\n ")
          message("No se ha encontrado todos los nodos en el algoritmo de profundidad")
          cat("\n ")
        }
        
      }
      
      tiempo<-proc.time() - t
      return(data.frame("Algoritmo"=c("Profundidad"),"Temporal"=c(as.character(maxHijos^profundidadGrafo)),"Espacial"=c(as.character(maxHijos*profundidadGrafo)),"Tiempo computacional"=c(tiempo[["elapsed"]]),"Tipo"=c("Ciegas")))
      
    },
    
    
    busquedaProfundidadIterativa = function(nodoInicio, listNodosBuscar, nivelBusqueda,dataIgraph,pausas) {
      t <- proc.time()
      maxHijos<-max(degree(dataIgraph,mode="out"))
      profundidadGrafo <- max(distances(dataIgraph, nodoInicio))
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
          dataIgraph<-grafica(extracciones,dataIgraph,TRUE,pausas)
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
        #Es para poder obviar los que ya estan repetidos y no extraer de nuevo
        while(!is.null(listNodosVisitados[[extracciones]])){
          cat("\n ")
          cat(paste("<El nodo", extracciones, "ya ha sido visitado, no se visitara!>", sep = " "))
          cat("\n ")
          cat("La pila actual es: ", pila$look())
          cat("\n ")
          extracciones <- pila$pop()
          if (is.null(extracciones)) {
            break
          }
          if (is.null(listNodosVisitados[[extracciones]])){
            dataIgraph<-grafica(extracciones,dataIgraph,FALSE,pausas)
          }
          
        }
        
        #Puede darse el caso de que la cola este vacia
        if (is.null(extracciones)) {
          break
        }
        
        
        dataIgraph<-grafica(extracciones,dataIgraph,FALSE,pausas)
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
          message("No se ha encontrado todos los nodos en el profundidad iterativa")
          cat("\n ")
        }
        
      }
      
      tiempo<-proc.time() - t
      
      complejidad=as.character(maxHijos^profundidadGrafo)
      return(data.frame("Algoritmo"=c("Profundidad iterativa"),"Temporal"=c(complejidad),"Espacial"=c(as.character(maxHijos)),"Tiempo computacional"=c(tiempo[["elapsed"]]),"Tipo"=c("Ciegas")))

    },
    
    #Algoritmo para la busqueda en costo uniforme
    busquedaCostoUniforme = function(nodoInicio, listNodosBuscar,dataIgraph,pausas) {
      #Se ingresa una lista para buscar todos los elementos de la lista dada
      #El nodo inicio es de tipo character
      t <- proc.time()
      maxHijos<-max(degree(dataIgraph,mode="out"))
      profundidadGrafo <- max(distances(dataIgraph, nodoInicio))
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
          dataIgraph<-grafica(extracciones$nodo,dataIgraph,TRUE,pausas)
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
        
        
        while(!is.null(listNodosVisitados[[extracciones$nodo]])){
          cat(paste("<El nodo", extracciones$nodo, "ya ha sido visitado, no se visitara!>", sep = " "))
          cat("\n ")
          cat("La cola actual es:")
          cat("\n ")
          print(cola$look(),row.names = FALSE)
          extracciones <- cola$pop()
          cat("\n ")
          if (is.null(extracciones)) {
            break
          }
          if (is.null(listNodosVisitados[[extracciones$nodo]])){
            dataIgraph<-grafica(extracciones$nodo,dataIgraph,FALSE,pausas)
          }
          
        }
        
        if (is.null(extracciones)) {
          break
        }
        dataIgraph<-grafica(extracciones$nodo,dataIgraph,FALSE,pausas)
        
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
          message("No se ha encontrado todos los nodos en la busqueda costo uniforme")
          cat("\n ")
        }
        
      }
      
      tiempo<-proc.time() - t
      complejidad=as.character(maxHijos^profundidadGrafo)
      return(data.frame("Algoritmo"=c("Costo uniforme"),"Temporal"=c(complejidad),"Espacial"=c(complejidad),"Tiempo computacional"=c(tiempo[["elapsed"]]),"Tipo"=c("Ciegas")))
    },
    
    #*************Algortimos Heuristicos****************
    metodoGradiente = function(nodoInicio, listNodosBuscar,dataIgraph,pausas) {
      #Se ingresa una lista para buscar todos los elementos de la lista dada
      #El nodo inicio es de tipo character
      t <- proc.time()
      grados<-degree(dataIgraph,mode="out")
      numNodos<-length(grados)
      profundidadGrafo <- max(distances(dataIgraph, nodoInicio))
      
      
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
          dataIgraph<-grafica(extracciones$nodo,dataIgraph,TRUE,pausas)
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
        
        cola$clean()
        
        while(!is.null(listNodosVisitados[[extracciones$nodo]])){
          cat(paste("<El nodo", extracciones$nodo, "ya ha sido visitado, no se visitara!>", sep = " "))
          cat("\n ")
          cat("La cola actual es:")
          cat("\n ")
          print(cola$look(),row.names = FALSE)
          extracciones <- cola$pop()
          cat("\n ")
          if (is.null(extracciones)) {
            break
          }
          if (is.null(listNodosVisitados[[extracciones$nodo]])){
            dataIgraph<-grafica(extracciones$nodo,dataIgraph,FALSE,pausas)
          }
          
        }
        
        if (is.null(extracciones)) {
          break
        }
        dataIgraph<-grafica(extracciones$nodo,dataIgraph,FALSE,pausas)
        
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
          message("No se ha encontrado todos los nodos en el gradiente")
          cat("\n ")
        }
        
      }
      
      tiempo<-proc.time() - t
      return(data.frame("Algoritmo"=c("Gradiente"),"Temporal"=c(paste("Mejor:",round(log(numNodos,10),2),"Peor:", profundidadGrafo,sep = " ")),"Espacial"=c(paste("Mejor:",1,"Peor:", profundidadGrafo,sep = " ")),"Tiempo computacional"=c(tiempo[["elapsed"]]),"Tipo"=c("Heuristico")))
      
    },
    
    #Algoritmo primero el mejor
    primeroMejor = function(nodoInicio, listNodosBuscar,dataIgraph,pausas) {
      #Se ingresa una lista para buscar todos los elementos de la lista dada
      #El nodo inicio es de tipo character
      t <- proc.time()
      grados<-degree(dataIgraph,mode="out")
      numNodos<-length(grados)
      profundidadGrafo <- max(distances(dataIgraph, nodoInicio))
      hijosPromedio<-round(mean(grados[grados>0]), digits = 0)
      
      
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
          dataIgraph<-grafica(extracciones$nodo,dataIgraph,TRUE,pausas)
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
       
        
        while(!is.null(listNodosVisitados[[extracciones$nodo]])){
          cat(paste("<El nodo", extracciones$nodo, "ya ha sido visitado, no se visitara!>", sep = " "))
          cat("\n ")
          cat("La cola actual es:")
          cat("\n ")
          print(cola$look(),row.names = FALSE)
          cat("\n ")
          extracciones <- cola$pop()
          if (is.null(extracciones)) {
            break
          }
          if (is.null(listNodosVisitados[[extracciones$nodo]])){
            dataIgraph<-grafica(extracciones$nodo,dataIgraph,FALSE,pausas)
          }
          
        }
        
        if (is.null(extracciones)) {
          break
        }
        dataIgraph<-grafica(extracciones$nodo,dataIgraph,FALSE,pausas)
        
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
          message("No se ha encontrado todos los nodos en el primero el mejor")
          cat("\n ")
        }
        
      }
      
      tiempo<-proc.time() - t
      return(data.frame("Algoritmo"=c("Primero Mejor"),"Temporal"=c(paste("Mejor:",round(log(numNodos,10),2),"Peor:", round(hijosPromedio^profundidadGrafo,2),sep = " ")),"Espacial"=c(paste("Mejor:",1,"Peor:", round(hijosPromedio^profundidadGrafo,2),sep = " ")),"Tiempo computacional"=c(tiempo[["elapsed"]]),"Tipo"=c("Heuristico")))
      
      
    },
    
    aEstrella = function(nodoInicio, listNodosBuscar,dataIgraph,pausas) {
      #Se ingresa una lista para buscar todos los elementos de la lista dada
      #El nodo inicio es de tipo character
      t <- proc.time()
      grados<-degree(dataIgraph,mode="out")
      profundidadGrafo <- max(distances(dataIgraph, nodoInicio))
      hijosPromedio<-round(mean(grados[grados>0]), digits = 0)
      listAux<-list()
      for (l in listNodosBuscar){
        if (!is.null(listNombreNodos[[l]])){
          listAux[[l]]<-l 
        }
      }
      
      profundidadMetaPromedio<-profudidadMeta(dataIgraph,listAux,nodoInicio)
      
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
          dataIgraph<-grafica(extracciones$nodo,dataIgraph,TRUE,pausas)
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
        
          while(!is.null(listNodosVisitados[[extracciones$nodo]])){
            cat(paste("<El nodo", extracciones$nodo, "ya ha sido visitado, no se visitara!>", sep = " "))
            cat("\n ")
            cat("La cola actual es:")
            cat("\n ")
            print(cola$look(),row.names = FALSE)
            extracciones <- cola$pop()
            cat("\n ")
            if (is.null(extracciones)) {
              break
            }
            if (is.null(listNodosVisitados[[extracciones$nodo]])){
              dataIgraph<-grafica(extracciones$nodo,dataIgraph,FALSE,pausas)
            }
          }
        
          
        if (is.null(extracciones)) {
          break
        }
        
          dataIgraph<-grafica(extracciones$nodo,dataIgraph,FALSE,pausas)
        
        
        nodosAdyacentes <- getNodoAristas(extracciones$nodo)$adyacentes
        
        
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
          message("No se ha encontrado todos los nodos en el A*")
          cat("\n ")
        }
      }
      
      tiempo<-proc.time() - t
      return(data.frame("Algoritmo"=c("A*"),"Temporal"=c(paste("Mejor:",hijosPromedio^profundidadMetaPromedio,"Peor:", round(hijosPromedio^profundidadGrafo,2),sep = " ")),"Espacial"=c(paste("Mejor:",1,"Peor:", round(hijosPromedio^profundidadGrafo,2),sep = " ")),"Tiempo computacional"=c(tiempo[["elapsed"]]),"Tipo"=c("Heuristico")))
      
    }
  )
)

# grafo <- grafoGene(nombreArchivo = "prueba.csv")
# datosIgraph<-grafo$initGrafo(FALSE,TRUE)
# V(datosIgraph)$color <- "yellow"
# print(grafo$busquedaProfundidadIterativa("A",list("I","J"),100,datosIgraph,TRUE))


