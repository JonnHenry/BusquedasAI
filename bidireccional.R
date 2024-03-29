library(structOfSearch)
library(igraph)
#Instalar la estructura de datos necesaria de los siguiente paquete de 
#github de elaboración propia https://github.com/JonnHenry/StructOfSearch.git

#Para utilzar se debe de instalar desde github con la siquiente linea de comando
#devtools::install_github("JonnHenry/StructOfSearch")


ifelse(require(igraph) == T, "Cargado", "igraph no cargado")
ifelse(require(structOfSearch) == T, "Cargado", "structOfSearch no cargado acceder al siguiente enlace para descargar https://github.com/JonnHenry/StructOfSearch.git")


#####################################################

graficaBidi = function(vertice,data,nodoFinal,pausas){
  if(nodoFinal){
    V(data)[vertice]$color <- "blue"
  }else{
    V(data)[vertice]$color <- "red"
  }
  plot(data,layout=layout.reingold.tilford(data, root=1), edge.arrow.size=0.5,vertex.color=V(data)$color)
  legend(x = "topright",cex=0.7,bty = "n" ,pt.cex=1,legend = c("Nodo intermedio","Nodo objetivo"), fill = c("yellow","blue"), title = "Estados de un nodo")
  if (pausas){
    Sys.sleep(1)
  }
  return(data)
}

nodoGeneBidi <- setRefClass("NodoBidi", fields = list(
  adyacentes = "list",
  id = "character"),
  methods = list(
    agregaAdyacente = function(nodoAdyacente, peso) {
      adyacentes[[nodoAdyacente]] <<- list(id = nodoAdyacente, peso = peso)
    }
  )
)


#Construcción de un grafo bidireccional para realizar la busqueda
grafoGeneBidi <- setRefClass("GrafoBidireccional", fields = list(
  listNodos = "list",
  nombreArchivo = "character",
  listNombreNodos = "list"),
  methods = list(
    agregarNodo = function(id) {
      if (is.null(listNombreNodos[[id]])) {
        nodoNuevo <- nodoGeneBidi(id = id)
        listNombreNodos[[id]] <<- id
        listNodos[[id]] <<- nodoNuevo
      }
    },
    getNodoAristas = function(id) {
      return(listNodos[[id]])
    },
    #Inicializa el grafo el parametro indica el orden si es FALSE entoces es ascendente 1,2,3 ...5 si es TRUE es descente 5,4,3,...1
    initGrafo = function(ordDescendente,cabecerasEnArchivo) {
      #inicializa el grafo
      dataFrameGrafo <- read.csv(nombreArchivo, sep = ",", header = cabecerasEnArchivo, stringsAsFactors = FALSE)
      dataAux <- dataFrameGrafo[order(as.character(dataFrameGrafo[[1]]),as.character(dataFrameGrafo[[2]]),na.last = TRUE,decreasing = ordDescendente),]
    
      dataAux<-dataAux[dataAux[[2]]!="",]
      dataFrameGrafo<-dataFrameGrafo[dataFrameGrafo[[2]]!="",]
      
      for (k in 1:length(dataAux[[1]])) {
        agregarArista(as.character(dataAux[[1]][[k]]), as.character(dataAux[[2]][[k]]), dataAux[[3]][[k]])
        agregarArista(as.character(dataAux[[2]][[k]]), as.character(dataAux[[1]][[k]]), dataAux[[3]][[k]])
      
      }
      return(graph_from_data_frame(dataFrameGrafo, directed = FALSE))
    },
    
    getNombreNodos = function() {
      return(listNombreNodos)
    },
    
    agregarArista = function(inicio, fin, peso) {
      #Me agrega una arista para poder conectar los nodos
      
      agregarNodo(inicio)
      agregarNodo(fin)
      listNodos[[inicio]]$agregaAdyacente(fin, peso)
      #Con la agregación de esta linea me ayuda a que los grafos esten doblemente conectado
      #y no sean grafos dirigidos
      listNodos[[fin]]$agregaAdyacente(inicio, peso)
    },
    
    getNodos = function() {
      return(listNombreNodos)
    },
    
    busquedaBidireccional=function(nodoInicio,nodosObj,cola,dataIgraph,pausas){
      t <- proc.time()
      maxHijos<-max(degree(dataIgraph,mode="all"))
      profundidadGrafo <- (max(distances(dataIgraph, nodoInicio))/2)
      if(cola){
        cat("******Se esta usando una cola para realizar la busqueda******")
      }else{
        cat("******Se esta usando una pila para realizar la busqueda******")
      }
      cat("\n ")
      estructura<-list()
      noSalir=TRUE
      listNodosVisitados <- list()
      extracciones <- list()
      finalizarBusqdNodo<-FALSE
      names(nodosObj) <- nodosObj
      #No se puede buscar el nodo origen y el nodo destino xq habria un choque en las colas
      if (length(nodosObj)==0 |length(intersect(nodoInicio,nodosObj))!=0){
        cat("No se ha ingresado nodos o el nodo inicial y el final es el mismo, no se puede realizar la busqueda!")
      } else{
        nodosObj[[nodoInicio]]<-nodoInicio
        if (cola){
          for (i in nodosObj){
            estructura[[i]]<-queue()
            estructura[[i]]$push(i)
            listNodosVisitados[[i]]<-list()
            extracciones[[i]]<-'--'
          }
        }else {
          for (i in nodosObj){
            estructura[[i]]<-stackOwn()
            estructura[[i]]$push(i)
            listNodosVisitados[[i]]<-list()
            extracciones[[i]]<-'--'
          }
        }
        cont<-1
        while (noSalir) {
          cat("\n ")
          cat("<-------------------------------------------------------------------------------->")
          cat("\n ")
          cat(cat("La iteración:", cont, sep = " "))
          cat("\n ")
          
          for (nodo in nodosObj){
            if(nodo==nodoInicio){
              cat("\n ")
              cat(paste("**Analizando el nodo Inicial:",nodo,sep = " "))
              
            }else{
              cat("\n ")
              cat(paste("**Analizando el nodo:",nodo,sep = " "))
            }
            
            #Busca un camino desde comun con las 2 pilas
            cat("\n ")
            cat(paste("La extracción es:",extracciones[[nodo]],sep = " "))
            cat("\n ")
            
            cat(paste("Estructura actual del nodo",nodo,"es: ", sep = " "))
            cat(estructura[[nodo]]$look(),sep = " ")
            cat("\n ")
            
            valor<-intersect(estructura[[nodo]]$look(),estructura[[nodoInicio]]$look())
            if (length(valor)!=0 & nodoInicio!=nodo ) {
              cat("\n ")
              cat(paste("La estructura actual del nodo inicio",nodoInicio,": ",sep = " "))
              cat(paste("",estructura[[nodoInicio]]$look()),sep = " ")
              cat("\n ")
              cat(paste("La estructura actual del nodo",nodo,": ",sep = " "))
              cat(paste("",estructura[[nodo]]$look()),sep = " ")
              cat("\n ")
              cat(paste("--->Existe un camino entre",nodo,"y",nodoInicio,"los elementos que coinciden son:",sep = " "))
              cat(paste(valor,sep = " "))
              cat("\n ")
              
              if (nodo!=nodoInicio){
                nodosObj[[nodo]]<-NULL
              }
              
              dataIgraph<-graficaBidi(nodo,dataIgraph,TRUE,pausas)
              dataIgraph<-graficaBidi(nodoInicio,dataIgraph,TRUE,pausas)
              finalizarBusqdNodo<-TRUE
              next
            }
            
            
            if (is.null(estructura[[nodo]]$look())) 
            {
              nodosObj[[nodo]]<-NULL
            }
            
            if (length(nodosObj) == 1) 
            {
              noSalir=FALSE
              break
            }
            extracciones[[nodo]] <- estructura[[nodo]]$pop()
            while(!is.null(listNodosVisitados[[nodo]][[extracciones[[nodo]]]])){
               cat(paste("<El nodo", extracciones[[nodo]], "ya ha sido visitado, no se visitara!>", sep = " "))
               cat("\n ")
               extracciones[[nodo]] <- estructura[[nodo]]$pop()
               cat(paste("Estructura actual del nodo",nodo,"es:", sep = " "))
               cat("\n ")
               if (is.null(extracciones)) {
                 break
               }
            }
            if (is.null(extracciones)) {
              break
            }
            
            nodosAdyacentes <- getNodoAristas(extracciones[[nodo]])$adyacentes
            for (nodoAd in nodosAdyacentes){
              estructura[[nodo]]$push(nodoAd$id)
            }
            listNodosVisitados[[nodo]][[extracciones[[nodo]]]] <- "1"
            finalizarBusqdNodo<-FALSE
          }
          cont<-cont+1
        }
        
        tiempo<-proc.time() - t
        complejidad=as.character(round(maxHijos^profundidadGrafo,2))
        return(data.frame("Algoritmo"=c("Bidireccional"),"Temporal"=c(complejidad),"Espacial"=c(complejidad),"Tiempo computacional"=c(tiempo[["elapsed"]]),"Tipo"=c("Ciegas")))
      }
    }
  )
)

# grafo <- grafoGeneBidi(nombreArchivo = "grafo1.csv")
# datosIgraph<-grafo$initGrafo(FALSE,FALSE)
# V(datosIgraph)$color <- "yellow"
# print(grafo$busquedaBidireccional("z",list("s","h","e"),TRUE,datosIgraph,TRUE))
