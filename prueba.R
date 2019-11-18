library(igraph)
dataFrameGrafo <- read.csv("pruebaRepetidos.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

#dataFrameGrafo<-dataFrameGrafo[dataFrameGrafo[[2]]!="",]

dataIgraph<-graph_from_data_frame(dataFrameGrafo, directed = TRUE)

V(datosIgraph)$color <- "yellow"

plot(dataIgraph,layout=layout.reingold.tilford(dataIgraph, root=1), edge.arrow.size=0.5,vertex.color=V(dataIgraph)$color)

distances(dataIgraph, "0")

profudidadMeta=function(dataframeIgraph,nodosBuscar,nodoInicial){
  if (length(nodosBuscar)==0){
    nodosBuscar<-nodoInicial
  }
  distancias <- distances(dataframeIgraph, "")
  costo<-0
  for (nodo  in nodosBuscar){
    costo<-costo+distancias[, nodo]
  }
  return(round(costo/length(nodosBuscar)))
}

t <- proc.time()

##Maximos sucesores de un nodo
maxHijos<-max(degree(dataIgraph,mode="out"))

#Hijos promedio
grados<-degree(dataIgraph,mode="out")
hijosPromedio<-round(mean(grados[grados>0]), digits = 0)

t <- proc.time()

tiempo<-proc.time() - t

print(tiempo)

print(tiempo[["elapsed"]])

#Nodos de un grafo
numNodos<-length(degree(dataIgraph,mode="out"))


# Se ingresa el nodo inicial a buscar "A" y el primer parametro es el grafo de igraph
distancias <- distances(dataIgraph, "A")
nivelNodo<-distancias[, "J"] #Se extrae solo la columna en R

#Profundidad de un grafo en R
distancias <- distances(dataIgraph, "A")
profundidadGrafo<-max(distancias)

profudidadMeta(dataIgraph,list("E","F","B"),"A")

























