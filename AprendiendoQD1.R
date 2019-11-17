library("igraph")
nodos=list()
nodos2ways=list()
opcionmenu<- 0
arnod<-read.csv("ArchivoPrubas.csv",header = TRUE, sep = ",",stringsAsFactors = FALSE)
dataf=data.frame(arnod)
datap<-dataf[!((dataf$fin)==""),c(1,2,3)]
node<-sort(union(unique(datap$Inicio),unique(datap$fin)),decreasing = FALSE)
Migrafo<-Grafo$new()


Migrafo$set_matriz_aristapeso(length(node),c(node))
Migrafo$get_matrix()

limpiar<-function(){
  for (variable in 1:length(nodos)) {
      nodos[[variable]]$set_visitado(FALSE)
      nodos[[variable]]$set_added(FALSE)
      nodos[[variable]]$set_hasnivel(FALSE)
      #cat("el valor de el nodo visitado  ",variable,"",nodos[[variable]]$get_visited(),"\n")
      #cat("el valor de el nodo aderido ",variable,"",nodos[[variable]]$get_added(),"\n")
  }
  
}

#Busquedas A ciegas
BFS<-function(Raiz,Meta){
  #print(str(Raiz))
  cola<- Queue$new()
  
  cola$add(Raiz)
  cat("Recorrido  : ->  ",Raiz$get_dato(),"\n")
  #cat("el nuevo valor e raiz es  : ->  ",Raiz$get_dato(),"\n")
  #cat("el valor de la cola al entrar es : ->  ",Raiz$get_dato(),"\n")
  while (cola$vacia()!=TRUE) {
    #print("entras \n")
    if(Raiz$get_dato()==Meta$get_dato()){
      cola$remove()
     
      break
    }else{
      
      Raiz$set_visitado(TRUE)
      #cat("el valor visiatdo  de la Raiz es : ->  ",Raiz$get_visited(),"\n")
      cola$remove()
      #cat("el valor qque se remueve es   es : ->  ",r,"\n")
      if (length(Raiz$get_lista())!=0 ) {
        #cat("el valor de el tam   es : ->  ",length(Raiz$get_lista()),"\n")
        for (variable in 1:length(Raiz$get_lista())) {
          #cat("el valor de el contador   es : ->  ",variable,"\n")
          Nodohijo=Raiz$get_lista()[[variable]]
          #cat("el valor de un hijo de un nodo   es : ->  ",Nodohijo$get_dato(),"\n")
          if (Nodohijo$get_visited() == FALSE && Nodohijo$get_added()==FALSE ){
            Nodohijo$set_added(TRUE)
            cola$add(Nodohijo)
            if(cola$vacia()!=TRUE){
              #cat("el valor  en la cola    es : ->  ",cola$frente()$get_dato(),"\n")
            }
            
            
          }
        }
      }
      if(cola$vacia()!=TRUE){
        Raiz<-cola$peek();
        cat("Recorrido  : ->  ",Raiz$get_dato(),"\n")
        
      }
      
      
    }
    
  }
  #print("sale del while")
  return(0)
  
}##fin bfs
DFS<-function(Raiz,Meta){
  #print(str(Raiz))
  pila<- Pila$new()
  
  pila$add(Raiz)
  #cat("el nuevo valor e raiz es  : ->  ",Raiz$get_dato(),"\n")
  #cat("el valor de la cola al entrar es : ->  ",Raiz$get_dato(),"\n")
  cat("Recorrido  : ->  ",Raiz$get_dato(),"\n")
  while (pila$vacia()!=TRUE) {
    #print("entras \n")
    if(Raiz$get_dato()==Meta$get_dato()){
      pila$remove()
      
      break
    }else{
      
      Raiz$set_visitado(TRUE)
      #cat("el valor visiatdo  de la Raiz es : ->  ",Raiz$get_visited(),"\n")
      pila$remove()
      #cat("el valor qque se remueve es   es : ->  ",r,"\n")
      if (length(Raiz$get_lista())!=0 ) {
        #cat("el valor de el contador   es : ->  ",variable,"\n")
        for (variable in 1:length(Raiz$get_lista())) {
          #cat("el valor de el contador   es : ->  ",variable,"\n")
          Nodohijo=Raiz$get_lista()[[variable]]
          #cat("el valor de un hijo de un nodo   es : ->  ",Nodohijo$get_dato(),"\n")
          if (Nodohijo$get_visited() == FALSE && Nodohijo$get_added()==FALSE ){
            Nodohijo$set_added(TRUE)
            pila$add(Nodohijo)
            if(pila$vacia()!=TRUE){
              #cat("el valor  en la pila    es : ->  ",pila$frente()$get_dato(),"\n")
            }
            
            
          }
        }#fin for
      }
      if(pila$vacia()!=TRUE){
        Raiz<-pila$peek();
        cat("Recorrido  : ->  ",Raiz$get_dato(),"\n")
        
      }
      
      
    }
    
  }
  #print("sale del while")
  return(0)
  
}##fin dfs
BCU<-function(Raiz,Meta){
  cola<-c()
  colavisiatados<-c()
  root<-Migrafo$matriz[Raiz,Raiz]
  cat("--------","\n")
  names(root)<-c(as.character(Raiz))
  print(root)
  cat("--------","\n")
  cat("--------","\n")
  cola<-c(cola,root)
  print(cola)
  cat("--------","\n")
  while (length(cola)!=0) {
    if(names(root)==Meta){
      print("encontrado")
      break
    }else{
      cat("--------Tiene hijos la raiz ","\n")
      cat("--------","\n")
      print(names(root))
      cat("--------","\n")
      x<-Migrafo$matriz[names(root),]
      print(x)
      cat("-----fin ---","\n")
      cat("----Tiene hijos la raiz----","\n")
      cat("--------Inicia el valor de peso del padre root ","\n")
      dator<-as.numeric(cola[1])
      print(dator)
      cat("--------Fin el valor de peso del padre root  ","\n")
      cat("--------Inicia el valor de cola visitados -----","\n")
      colavisiatados<-c(colavisiatados,cola[1])
      print(colavisiatados)
      cat("--------finaliza el valor de colas visitados----","\n")
      cola<-cola[-1]
      verx<-all(x==0)
      if (verx!=TRUE) {
        cat("--Inicia ingresea a ordx-----" ,"\n")
        ordx<-c(sort(x[x>0]))
        print(ordx)
        cat("-- finaliza ingresea a ordx-----","\n")
        for (variable in 1:length(ordx)) {
          ordx[variable]<-ordx[variable]+dator
          cola<-c(cola,ordx[variable])
        }
        cat("--Inicia ingresea a cola-----" ,"\n")
        cola<-sort(cola)
        print(cola)
        cat("--fin ingresea a cola -----" ,"\n")
        cat("-- Inicia el valor nuevo de root-----" ,"\n")
        root<-cola[1]
        print(root)
        cat("-----finaliza el valor nuevo de root-----" ,"\n")
        while (any((names(cola[1])==names(colavisiatados)))==TRUE) {
          cat("--Inicia ingresea a cola cuando enceuntra visitados en la cola[1] -----" ,"\n")
          cat("--antes-----" ,"\n")
          print(cola)
          cat("--antes-----" ,"\n")
          cola<-cola[-1]
          cat("--dspues-----" ,"\n")
          print(root)
          cat("--despues-----" ,"\n")
          root<-cola[1]
          print(cola)
          cat("--finaliza ingresea a cola cuando enceuntra visitados en la cola[1] -----" ,"\n")
        }
        
      }else{
        cat("--Inicia ingresea a cola cuando la fila es 000000 -----" ,"\n")
        print(cola)
        #cola<-cola[-1]
        root<-cola[1]
        cat("nueva cola ","\n")
        print(cola)
        cat("--finaliza  ingresea a cola cuando fila es 00000 -----" ,"\n")
      }#finif
    }#finif
    #fin
  }
}##fin BCU
IDS<-function(Raiz,Meta,Nivel){
  #print(str(Raiz))
  cola<- Queue$new()
  
  cola$add(Raiz)
  cat("Recorrido  : ->  ",Raiz$get_dato(),"\n")
  #cat("el nuevo valor e raiz es  : ->  ",Raiz$get_dato(),"\n")
  #cat("el valor de la cola al entrar es : ->  ",Raiz$get_dato(),"\n")
  while (Raiz$==tam) {
    #print("entras \n")
    if(Raiz$get_dato()==Meta$get_dato()){
      cola$remove()
      
      break
    }else{
      
      Raiz$set_visitado(TRUE)
      #cat("el valor visiatdo  de la Raiz es : ->  ",Raiz$get_visited(),"\n")
      cola$remove()
      #cat("el valor qque se remueve es   es : ->  ",r,"\n")
      if (length(Raiz$get_lista())!=0 ) {
        #cat("el valor de el tam   es : ->  ",length(Raiz$get_lista()),"\n")
        for (variable in 1:length(Raiz$get_lista())) {
          #cat("el valor de el contador   es : ->  ",variable,"\n")
          Nodohijo=Raiz$get_lista()[[variable]]
          #cat("el valor de un hijo de un nodo   es : ->  ",Nodohijo$get_dato(),"\n")
          if (Nodohijo$get_visited() == FALSE && Nodohijo$get_added()==FALSE ){
            Nodohijo$set_added(TRUE)
            cola$add(Nodohijo)
            if(cola$vacia()!=TRUE){
              #cat("el valor  en la cola    es : ->  ",cola$frente()$get_dato(),"\n")
            }
            
            
          }
        }
      }
      if(cola$vacia()!=TRUE){
        Raiz<-cola$peek();
        cat("Recorrido  : ->  ",Raiz$get_dato(),"\n")
        
      }
      
      
    }
    
  }
  #print("sale del while")
  return(0)
  
}##fin IDS
#Busquedas Heuristicas

HCS<-function(Raiz,Meta){
  
  cola<- colapr$new()
  
  cola$add(Raiz)
  cat("Recorrido  : ->  ",Raiz$get_dato(),"\n")
  #cat("el nuevo valor e raiz es  : ->  ",Raiz$get_dato(),"\n")
  #cat("el valor de la cola al entrar es : ->  ",Raiz$get_dato(),"\n")
  while (cola$vacia()!=TRUE) {
    #print("entras \n")
    if(Raiz$get_dato()==Meta$get_dato()){
      cola$remove()
      
      break
    }else{
      
      Raiz$set_visitado(TRUE)
      #cat("el valor visiatdo  de la Raiz es : ->  ",Raiz$get_visited(),"\n")
      #cola$remove()
      cola$vaciar
      #cat("el valor qque se remueve es   es : ->  ",r,"\n")
      if (length(Raiz$get_lista())!=0 ) {
        #cat("el valor de el tam   es : ->  ",length(Raiz$get_lista()),"\n")
        for (variable in 1:length(Raiz$get_lista())) {
          #cat("el valor de el contador   es : ->  ",variable,"\n")
          Nodohijo=Raiz$get_lista()[[variable]]
          #cat("el valor de un hijo de un nodo   es : ->  ",Nodohijo$get_dato(),"\n")
          if (Nodohijo$get_visited() == FALSE && Nodohijo$get_added()==FALSE ){
            Nodohijo$set_added(TRUE)
            cola$add(Nodohijo)
            if(cola$vacia()!=TRUE){
              #cat("el valor  en la cola    es : ->  ",cola$frente()$get_dato(),"\n")
            }
            
            
          }
        }
      }
      if(cola$vacia()!=TRUE){
        Raiz<-cola$peek();
        cat("Recorrido  : ->  ",Raiz$get_dato(),"\n")
        
      }
      
      
    }
    
  }
  #print("sale del while")
  return(0)
  
  
}
FBS<-function(Raiz,Meta){
  
  cola<- colapr$new()
  
  cola$add(Raiz)
  cat("Recorrido  : ->  ",Raiz$get_dato(),"\n")
  #cat("el nuevo valor e raiz es  : ->  ",Raiz$get_dato(),"\n")
  #cat("el valor de la cola al entrar es : ->  ",Raiz$get_dato(),"\n")
  while (cola$vacia()!=TRUE) {
    #print("entras \n")
    if(Raiz$get_dato()==Meta$get_dato()){
      cola$remove()
      
      break
    }else{
      
      Raiz$set_visitado(TRUE)
      #cat("el valor visiatdo  de la Raiz es : ->  ",Raiz$get_visited(),"\n")
      #cola$remove()
      cola$vaciar
      #cat("el valor qque se remueve es   es : ->  ",r,"\n")
      if (length(Raiz$get_lista())!=0 ) {
        #cat("el valor de el tam   es : ->  ",length(Raiz$get_lista()),"\n")
        for (variable in 1:length(Raiz$get_lista())) {
          #cat("el valor de el contador   es : ->  ",variable,"\n")
          Nodohijo=Raiz$get_lista()[[variable]]
          #cat("el valor de un hijo de un nodo   es : ->  ",Nodohijo$get_dato(),"\n")
          if (Nodohijo$get_visited() == FALSE && Nodohijo$get_added()==FALSE ){
            Nodohijo$set_added(TRUE)
            cola$add(Nodohijo)
            if(cola$vacia()!=TRUE){
              #cat("el valor  en la cola    es : ->  ",cola$frente()$get_dato(),"\n")
            }
            
            
          }
        }
      }
      if(cola$vacia()!=TRUE){
        Raiz<-cola$peek();
        cat("Recorrido  : ->  ",Raiz$get_dato(),"\n")
        
      }
      
      
    }
    
  }
  #print("sale del while")
  return(0)
  
  
}
BDD<-function(Raiz,Meta){
  
  cola1<-c()
  cola2<-c()
  if(Raiz$get_dato()==Meta$get_dato()){
    print("encontrado")
  }
  cola1<-c(cola1,Raiz)
  cola2<-c(cola2,Meta)
  
  while (algo!=TRUE) {
    cola1aux<-c()
    cola2aux<-c()
    for (variable in cola1) {
      if (variable == cola2aux) {
        print("encontrado")
      }
      cola<-c(cola,Raiz)
    }
   
    for (variable in cola2) {
      if (variable == cola1aux) {
        print("encontrado")
      }
    }
   
   
  }#fin while
  #print("sale del while")
  return(0)
  
  
}#fin BDD



while (opcionmenu != 3 ) {
  cat("Menu de Ingreso \n")
  cat("1)cargar Nodos Desde Archivo \n")
  cat("2)cargar Aristas  Desde Archivo \n")
  cat("3)Ejecutar Metodos De Busqueda \n")
  cat("4)Salir \n")
  opcionmenu<- readline("Cual elige  ? ")
  opciones<-paste0("opcion",opcionmenu)
  switch(opciones,
         opcion1={
          
           
           data<-unique(dataf[,c(1,4)])
           n<-nrow(data)
           
           for (variable in 1:n) {
             nombre_nodo<-data[variable,1]
             heuristica_nodo<-data[variable,2]
             objNodo<-Nodo$new()
             objNodo$set_dato(nombre_nodo)
             objNodo$set_heuristica(heuristica_nodo)
             nodos[[nombre_nodo]]<-objNodo
             #nodos2ways[[variable]]<-objNodo
           }
           
           for (variable in 1:n) {
             nombre_nodo<-data[variable,1]
             heuristica_nodo<-data[variable,2]
             objNodo<-Nodo$new()
             objNodo$set_dato(nombre_nodo)
             objNodo$set_heuristica(heuristica_nodo)
             
             nodos2ways[[nombre_nodo]]<-objNodo
           }
          
           
         },
         opcion2={
           
           dataemptyh<-dataf[!((dataf$fin)==""),c(1,2,4)]
           numerofilas<-nrow(dataemptyh)
           solonodos<-dataemptyh[,c(1,2)]
           solonodos
           for (variable in 1:numerofilas) {
             father<-solonodos[variable,1]
             son<-solonodos[variable,2]
             decendiente<-nodos[[son]]
             nodos[[father]]$set_hijos(decendiente)
             
             adecendiente<-nodos2ways[[father]]
             nodos2ways[[son]]$set_hijos(adecendiente)
            
           }
           
           dataemptyp<-dataf[!((dataf$fin)==""),c(1,2,3)]
           Migrafo$get_matrix()
           solonodos<-dataemptyp[,c(1,2,3)]
           for (variable in 1:numerofilas) {
             i<-solonodos[variable,1]
             j<-solonodos[variable,2]
             p<-solonodos[variable,3]
             Migrafo$matriz[i,j]<-p
           }
           
           
         },
         opcion3={
           dataempty<-dataf[!((dataf$fin)==""),c(1,2)]
           vive <-matrix(c(dataempty$Inicio, dataempty$fin), ncol=2)
           head.matrix(vive)
           g<-graph_from_edgelist(vive)
           plot(g)
           cat("Ingrese la raiz ->  ")
           R<- readline("Ingrese el nodo Raiz->  : ")
           root<-R
           cat("\n")
           cat("Ingrese la Meta ->  ")
           M<- readline("Ingrese el nodo Meta->  : ")
           goal<-M 
           NodoR<-nodos[[R]]
           NodoM<-nodos[[M]]
           cat("Busqueda En Amplitud ->  \n")
           BFS(NodoR,NodoM)
           
           
           limpiar()
           cat("Busqueda En Profundidad ->  \n")
           DFS(NodoR,NodoM)
           limpiar()
           cat("Busqueda en costo uniforme ->  \n")
           BCU(NodoR$get_dato(),NodoM$get_dato())
           
         },opcion4={
           break()
         },
  )#finswitch
  
  
}# fin while

