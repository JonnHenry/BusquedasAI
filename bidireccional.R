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
  legend(x = "topright",cex=0.7,bty = "n" ,pt.cex=1,legend = c("Nodo intermedio","Nodo objetivo"), fill = c("yellow","blue"), title = "Estados de un nodo")
  Sys.sleep(1)
  return(data)
}

nodoGene <- setRefClass("Nodo", fields = list(
  adyacentes = "list",
  id = "character",
  heuristica="numeric"),
  methods = list(
    agregarAdyacente = function(nodoAdyacente, peso) {
      adyacentes[[nodoAdyacente]] <<- list(id = nodoAdyacente, peso = peso)
    }
  )
)


#Construcción de un grafo bidireccional para realizar la busqueda
grafoBidiGene <- setRefClass("GrafoBidireccional", fields = list(
  listNodos = "list",
  nombreArchivo = "character",
  listNombreNodos = "list"),
  methods = list(
    agregarNodo = function(id) {
      if (is.null(listNombreNodos[[id]])) {
        nodoNuevo <- nodoGene(id = id)
        listNombreNodos[[id]] <<- id
        listNodos[[id]] <<- nodoNuevo
      }
    },
    getNodoAristas = function(id) {
      return(listNodos[[id]])
    },
    #Inicializa el grafo el parametro indica el orden si es FALSE entoces es ascendente 1,2,3 ...5 si es TRUE es descente 5,4,3,...1
    initGrafo = function(decresiente) {
      #inicializa el grafo
      dataFrameGrafo <- read.csv(nombreArchivo, sep = ",", header = TRUE, stringsAsFactors = FALSE)
      dataFrameGrafo<-dataFrameGrafo[order(dataFrameGrafo$nodoInicio,dataFrameGrafo$nodoFin,na.last = TRUE,decreasing = decresiente),]
      
      dataFrameGrafo<-dataFrameGrafo[dataFrameGrafo[[2]]!="",]
      
      for (k in 1:length(dataFrameGrafo[[1]])) {
        agregarArista(as.character(dataFrameGrafo[[1]][[k]]), as.character(dataFrameGrafo[[2]][[k]]), dataFrameGrafo[[3]][[k]])
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
      listNodos[[inicio]]$agregarAdyacente(fin, peso)
      #Con la agregación de esta linea me ayuda a que los grafos esten doblemente conectado
      #y no sean grafos dirigidos
      listNodos[[fin]]$agregarAdyacente(inicio, peso)
    },
    
    getNodos = function() {
      return(listNombreNodos)
    },
    busquedaBidiriccional=function(nodoInicio,nodosObj,cola,dataIgraph){
      if(cola){
        cat("Se esta usando una cola para realizar la busqueda")
      }else{
        cat("Se esta usando una pila para realizar la busqueda")
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
              
              
              dataIgraph<-grafica(nodo,dataIgraph,TRUE)
              dataIgraph<-grafica(nodoInicio,dataIgraph,TRUE)
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
      }
    }
  )
)

grafo <- grafoBidiGene(nombreArchivo = "pruebaRepetidos.csv")
datosIgraph<-grafo$initGrafo(FALSE)
V(datosIgraph)$color <- "yellow"
grafo$busquedaBidiriccional("0",list("1","2","3"),TRUE,datosIgraph)
