library(structOfSearch)
library(igraph)
source('ingresoDatos.R',local = TRUE,encoding="utf-8")
source('BusquedasGrafos.R',local = TRUE,encoding="utf-8")
source('bidireccional.R',local = TRUE,encoding="utf-8")

# Grafo direccional
# grafo <- grafoGene(nombreArchivo = "prueba.csv")
# datosIgraph<-grafo$initGrafo(FALSE,TRUE)
# V(datosIgraph)$color <- "yellow"
# grafo$aEstrella("A",list("J"),datosIgraph,FALSE)

#Grafo bidireccional
# grafo <- grafoBidiGene(nombreArchivo = "grafo1.csv")
# datosIgraph<-grafo$initGrafo(FALSE,FALSE)
# V(datosIgraph)$color <- "yellow"
# print(grafo$busquedaBidireccional("z",list("s","h","e"),TRUE,datosIgraph,TRUE))

#readInteger("Mesaje a imprimir")
#readCaracter("Mensaje a imprimir")
archivoValido<-FALSE
nombreArchivo<-""
grafoDireccional<-NULL
grafoBidireccional<-NULL
cabecerasArchivo<-FALSE
ordenadoDescendentemente<-FALSE
datosIgraphDireccional<-NULL
datosIgraphDBidireccional<-NULL
listNodos<-list()
numNodos<-0

while(TRUE){
  shell("cls")
  cat("\n ")
  cat("***********************************************************************")
  cat("\n ")
  message("                            Bienvenido")
  cat("Seleccione la opción que desea ejecutar en el programa\n ")
  cat("1. Ingreso del archivo para su analisis\n ")
  cat("2. Busqueda en Amplitud\n ")
  cat("3. Busqueda en Profundidad\n ")
  cat("4. Busqueda Bidireccional\n ")
  cat("5. Busqueda Profundidad Iterativa\n ")
  cat("6. Costo uniforme\n ")
  cat("7. Metódo gradiente\n ")
  cat("8. Busqueda primero el mejor\n ")
  cat("9. Algoritmo A*\n ")
  cat("10. Ejecutar todos los algoritmos\n ")
  cat("11. Salir del programa\n ")
  opcion<-readCaracter("Ingrese la opción a elegir:")
  switch(opcion, 
         '1'={
           archivoValido<-FALSE
           nombreArchivo<-readCaracter("Ingrese el nombre del archivo:")
           if(file.exists(nombreArchivo)) {
             grafoBidireccional <- grafoGeneBidi(nombreArchivo = nombreArchivo)
             grafoDireccional <- grafoGene(nombreArchivo = nombreArchivo)
             cat("El archivo tiene cabeceras\n ")
             cat("1. Si\n ")
             cat("2. No\n")
             cabeceras<-readCaracter("Respuesta:")
             cat("El agregar los nodos ordenado de manera\n ")
             cat("1. Ascendentemente 1...n\n ")
             cat("2. Descendentemente n...1\n")
             orden<-readCaracter("Respuesta:")
             
             switch (cabeceras,
               "1"={
                 cabecerasArchivo<-TRUE
               },
               "2"={
                 cabecerasArchivo<-FALSE
               })
             
             switch (orden,
                "1"={
                  ordenadoDescendentemente<-FALSE
                },
                "2"={
                  ordenadoDescendentemente<-TRUE
                })
             datosIgraphDireccional<-grafoDireccional$initGrafo(ordenadoDescendentemente,cabecerasArchivo)
             V(datosIgraphDireccional)$color <- "yellow"
             plot(datosIgraphDireccional,layout=layout.reingold.tilford(datosIgraphDireccional, root=1), edge.arrow.size=0.6,vertex.color=V(datosIgraphDireccional)$color)
             datosIgraphDBidireccional<-grafoBidireccional$initGrafo(ordenadoDescendentemente,cabecerasArchivo)
             V(datosIgraphDBidireccional)$color <- "yellow"
             listNodos<-grafoDireccional$getNombreNodos()
             archivoValido<-TRUE
           }else{
             archivoValido<-FALSE
             shell("cls")
             message("El archivo no existe o no es valido!")
             Sys.sleep(2)
          }
         },
         '2'={
           if (archivoValido){
             shell("cls")
             listNodosBuscar<-list()
             cat("\nLos nodos disponibles son:")
             cat(unlist(listNodos, use.names = FALSE))
             cat("\n")
             while(TRUE){
               nodoInicial<-readCaracter("Ingrese el nodo inicio:")
               if (!is.null(listNodos[[nodoInicial]])) {
                 break
               }else{
                 message("El nodo no se encuentra en el grafo")
               }
             }
             
             numNodos<-readInteger("Ingrese el número de nodos a buscar:")
             cont<-0
             while(cont<numNodos&!is.null(listNodos[[nodoInicial]])){
               nodo<-readCaracter(paste("Ingrese el nodo", cont+1,"a buscar:",sep = " "))
               if (!is.null(listNodos[[nodo]])) {
                 listNodosBuscar[[nodo]]<-nodo
                 cont<-cont+1
               }else{
                 message("El nodo no se encuentra en el grafo")
               }
             }
            
             resultados<-grafoDireccional$busquedaAmplitud(nodoInicial, listNodosBuscar,datosIgraphDireccional,TRUE)
             cat("\nResultados de la ejecucción\n")
             print(resultados,row.names = FALSE)
             Sys.sleep(15)
             
           }else {
             shell("cls")
             message("No se ha ingresado ningun archivo o no es válido")
             Sys.sleep(2)
           }
         },
         '3'={
           if (archivoValido){
             shell("cls")
             listNodosBuscar<-list()
             cat("\nLos nodos disponibles son:")
             cat(unlist(listNodos, use.names = FALSE))
             cat("\n")
             while(TRUE){
               nodoInicial<-readCaracter("Ingrese el nodo inicio:")
               if (!is.null(listNodos[[nodoInicial]])) {
                 break
               }else{
                 message("El nodo no se encuentra en el grafo")
               }
             }
             
             numNodos<-readInteger("Ingrese el número de nodos a buscar:")
             cont<-0
             while(cont<numNodos&!is.null(listNodos[[nodoInicial]])){
               nodo<-readCaracter(paste("Ingrese el nodo", cont+1,"a buscar:",sep = " "))
               if (!is.null(listNodos[[nodo]])) {
                 listNodosBuscar[[nodo]]<-nodo
                 cont<-cont+1
               }else{
                 message("El nodo no se encuentra en el grafo")
               }
             }
             
             resultados<-grafoDireccional$busquedaProfundidad(nodoInicial, listNodosBuscar,datosIgraphDireccional,TRUE)
             cat("\nResultados de la ejecucción\n")
             print(resultados,row.names = FALSE)
             Sys.sleep(15)
           }else {
             shell("cls")
             message("No se ha ingresado ningun archivo o no es válido")
             Sys.sleep(2)
           }
         },
         '4'={
           if (archivoValido){
             shell("cls")
             usaCola<-TRUE
             listNodosBuscar<-list()
             cat("\nLos nodos disponibles son:")
             cat(unlist(listNodos, use.names = FALSE))
             cat("\n")
             while(TRUE){
               nodoInicial<-readCaracter("Ingrese el nodo inicio:")
               if (!is.null(listNodos[[nodoInicial]])) {
                 break
               }else{
                 message("El nodo no se encuentra en el grafo")
               }
             }
             cat("Ingrese la manera de realizar la busqueda \n ")
             cat("1. Usando una cola\n ")
             cat("2. Usando una pila\n")
             estructUsar<-readCaracter("Respuesta:")
             switch (estructUsar,
                     "1"={
                       usaCola<-TRUE
                     },
                     "2"={
                       usaCola<-FALSE
                     })
             
             numNodos<-readInteger("Ingrese el número de nodos a buscar:")
             cont<-0
             while(cont<numNodos&!is.null(listNodos[[nodoInicial]])){
               nodo<-readCaracter(paste("Ingrese el nodo", cont+1,"a buscar:",sep = " "))
               if (!is.null(listNodos[[nodo]])) {
                 listNodosBuscar[[nodo]]<-nodo
                 cont<-cont+1
               }else{
                 message("El nodo no se encuentra en el grafo")
               }
             }
             
             resultados<-grafoBidireccional$busquedaBidireccional(nodoInicial, listNodosBuscar,usaCola,datosIgraphDBidireccional,TRUE)
             cat("\nResultados de la ejecucción\n")
             print(resultados,row.names = FALSE)
             Sys.sleep(15)
           }else {
             shell("cls")
             message("No se ha ingresado ningun archivo o no es válido")
             Sys.sleep(2)
           }    
         },
         '5'={
           if (archivoValido){
             shell("cls")
             listNodosBuscar<-list()
             cat("\nLos nodos disponibles son:")
             cat(unlist(listNodos, use.names = FALSE))
             cat("\n")
             nivelBusqueda<-readInteger("Ingrese hasta el nivel de la busqueda:")
             while(TRUE){
               nodoInicial<-readCaracter("Ingrese el nodo inicio:")
               if (!is.null(listNodos[[nodoInicial]])) {
                 break
               }else{
                 message("El nodo no se encuentra en el grafo")
               }
             }
             numNodos<-readInteger("Ingrese el número de nodos a buscar:")
             cont<-0
             while(cont<numNodos&!is.null(listNodos[[nodoInicial]])){
               nodo<-readCaracter(paste("Ingrese el nodo", cont+1,"a buscar:",sep = " "))
               if (!is.null(listNodos[[nodo]])) {
                 listNodosBuscar[[nodo]]<-nodo
                 cont<-cont+1
               }else{
                 message("El nodo no se encuentra en el grafo")
               }
             }
             
             resultados<-grafoDireccional$busquedaProfundidadIterativa(nodoInicial, listNodosBuscar,nivelBusqueda,datosIgraphDireccional,TRUE)
             cat("\nResultados de la ejecucción\n")
             print(resultados,row.names = FALSE)
             Sys.sleep(15)
           }else {
             shell("cls")
             message("No se ha ingresado ningun archivo o no es válido")
             Sys.sleep(2)
           }  
         },
         '6'={
           if (archivoValido){
             shell("cls")
             listNodosBuscar<-list()
             cat("\nLos nodos disponibles son:")
             cat(unlist(listNodos, use.names = FALSE))
             cat("\n")
             while(TRUE){
               nodoInicial<-readCaracter("Ingrese el nodo inicio:")
               if (!is.null(listNodos[[nodoInicial]])) {
                 break
               }else{
                 message("El nodo no se encuentra en el grafo")
               }
             }
             numNodos<-readInteger("Ingrese el número de nodos a buscar:")
             cont<-0
             while(cont<numNodos&!is.null(listNodos[[nodoInicial]])){
               nodo<-readCaracter(paste("Ingrese el nodo", cont+1,"a buscar:",sep = " "))
               if (!is.null(listNodos[[nodo]])) {
                 listNodosBuscar[[nodo]]<-nodo
                 cont<-cont+1
               }else{
                 message("El nodo no se encuentra en el grafo")
               }
             }
             
             resultados<-grafoDireccional$busquedaCostoUniforme(nodoInicial, listNodosBuscar,datosIgraphDireccional,TRUE)
             cat("\nResultados de la ejecucción\n")
             print(resultados,row.names = FALSE)
             Sys.sleep(15)
           }else {
             shell("cls")
             message("No se ha ingresado ningun archivo o no es válido")
             Sys.sleep(2)
           }    
         },
         '7'={
           if (archivoValido){
             shell("cls")
             listNodosBuscar<-list()
             cat("\nLos nodos disponibles son:")
             cat(unlist(listNodos, use.names = FALSE))
             cat("\n")
             while(TRUE){
               nodoInicial<-readCaracter("Ingrese el nodo inicio:")
               if (!is.null(listNodos[[nodoInicial]])) {
                 break
               }else{
                 message("El nodo no se encuentra en el grafo")
               }
             }
             numNodos<-readInteger("Ingrese el número de nodos a buscar:")
             cont<-0
             while(cont<numNodos&!is.null(listNodos[[nodoInicial]])){
               nodo<-readCaracter(paste("Ingrese el nodo", cont+1,"a buscar:",sep = " "))
               if (!is.null(listNodos[[nodo]])) {
                 listNodosBuscar[[nodo]]<-nodo
                 cont<-cont+1
               }else{
                 message("El nodo no se encuentra en el grafo")
               }
             }
             
             resultados<-grafoDireccional$metodoGradiente(nodoInicial, listNodosBuscar,datosIgraphDireccional,TRUE)
             cat("\nResultados de la ejecucción\n")
             print(resultados,row.names = FALSE)
             Sys.sleep(15)
           }else {
             shell("cls")
             message("No se ha ingresado ningun archivo o no es válido")
             Sys.sleep(2)
           }   
         },
         '8'={
           if (archivoValido){
             shell("cls")
             listNodosBuscar<-list()
             cat("\nLos nodos disponibles son:")
             cat(unlist(listNodos, use.names = FALSE))
             cat("\n")
             while(TRUE){
               nodoInicial<-readCaracter("Ingrese el nodo inicio:")
               if (!is.null(listNodos[[nodoInicial]])) {
                 break
               }else{
                 message("El nodo no se encuentra en el grafo")
               }
             }
             numNodos<-readInteger("Ingrese el número de nodos a buscar:")
             cont<-0
             while(cont<numNodos&!is.null(listNodos[[nodoInicial]])){
               nodo<-readCaracter(paste("Ingrese el nodo", cont+1,"a buscar:",sep = " "))
               if (!is.null(listNodos[[nodo]])) {
                 listNodosBuscar[[nodo]]<-nodo
                 cont<-cont+1
               }else{
                 message("El nodo no se encuentra en el grafo")
               }
             }
             
             resultados<-grafoDireccional$primeroMejor(nodoInicial, listNodosBuscar,datosIgraphDireccional,TRUE)
             cat("\nResultados de la ejecucción\n")
             print(resultados,row.names = FALSE)
             Sys.sleep(15)
           }else {
             shell("cls")
             message("No se ha ingresado ningun archivo o no es válido")
             Sys.sleep(2)
           }    
         },
         '9'={
           if (archivoValido){
             shell("cls")
             listNodosBuscar<-list()
             cat("\nLos nodos disponibles son:")
             cat(unlist(listNodos, use.names = FALSE))
             cat("\n")
             while(TRUE){
               nodoInicial<-readCaracter("Ingrese el nodo inicio:")
               if (!is.null(listNodos[[nodoInicial]])) {
                 break
               }else{
                 message("El nodo no se encuentra en el grafo")
               }
             }
             numNodos<-readInteger("Ingrese el número de nodos a buscar:")
             cont<-0
             while(cont<numNodos&!is.null(listNodos[[nodoInicial]])){
               nodo<-readCaracter(paste("Ingrese el nodo", cont+1,"a buscar:",sep = " "))
               if (!is.null(listNodos[[nodo]])) {
                 listNodosBuscar[[nodo]]<-nodo
                 cont<-cont+1
               }else{
                 message("El nodo no se encuentra en el grafo")
               }
             }
             
             resultados<-grafoDireccional$aEstrella(nodoInicial, listNodosBuscar,datosIgraphDireccional,TRUE)
             cat("\nResultados de la ejecucción\n")
             print(resultados,row.names = FALSE)
             Sys.sleep(15)
           }else {
             shell("cls")
             message("No se ha ingresado ningun archivo o no es válido")
             Sys.sleep(2)
           }
         },
         '10'={
           if (archivoValido){
             shell("cls")
             listNodosBuscar<-list()
             cat("\nLos nodos disponibles son:")
             cat(unlist(listNodos, use.names = FALSE))
             cat("\n")
             while(TRUE){
               nodoInicial<-readCaracter("Ingrese el nodo inicio:")
               if (!is.null(listNodos[[nodoInicial]])) {
                 break
               }else{
                 message("El nodo no se encuentra en el grafo")
               }
             }
             numNodos<-readInteger("Ingrese el número de nodos a buscar:")
             if (numNodos>=1){
               cont<-0
               while(cont<numNodos&!is.null(listNodos[[nodoInicial]])){
                 nodo<-readCaracter(paste("Ingrese el nodo", cont+1,"a buscar:",sep = " "))
                 if (!is.null(listNodos[[nodo]])) {
                   listNodosBuscar[[nodo]]<-nodo
                   cont<-cont+1
                 }else{
                   message("El nodo no se encuentra en el grafo")
                 }
               }
               
               
               resultados<-grafoDireccional$busquedaAmplitud(nodoInicial, listNodosBuscar,datosIgraphDireccional,FALSE)
               resultados<-rbind(resultados,grafoDireccional$busquedaProfundidad(nodoInicial, listNodosBuscar,datosIgraphDireccional,FALSE))
               resultados<-rbind(resultados,grafoDireccional$busquedaProfundidadIterativa(nodoInicial,listNodosBuscar,1000,datosIgraphDireccional,FALSE))
               resultados<-rbind(resultados,grafoBidireccional$busquedaBidireccional(nodoInicial, listNodosBuscar,TRUE,datosIgraphDBidireccional,FALSE))
               resultados<-rbind(resultados,grafoDireccional$busquedaCostoUniforme(nodoInicial, listNodosBuscar,datosIgraphDireccional,FALSE))
               resultados<-rbind(resultados,grafoDireccional$metodoGradiente(nodoInicial, listNodosBuscar,datosIgraphDireccional,FALSE))
               resultados<-rbind(resultados,grafoDireccional$primeroMejor(nodoInicial, listNodosBuscar,datosIgraphDireccional,FALSE))
               resultados<-rbind(resultados,grafoDireccional$aEstrella(nodoInicial, listNodosBuscar,datosIgraphDireccional,FALSE))
               
               cat("\nResultados de la ejecucción\n")
               print(resultados,row.names = FALSE)
               Sys.sleep(20)
             }else {
               shell("cls")
               message("Se tiene que enviar como mínimo 1 nodo debido algoritmo bidireccional!")
               Sys.sleep(15)
             }
             
           }else {
             
             message("No se ha ingresado ningun archivo o no es válido")
             Sys.sleep(2)
           }    
         },
         '11'={
           break
         },
         {
           shell("cls")
           message("          -->>La opción ingresada no es valida<<--")
           Sys.sleep(2)
           
         })
}


