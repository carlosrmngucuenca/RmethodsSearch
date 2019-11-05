nodos=list()
opcionmenu<- 0

limpiar<-function(){
  for (variable in 1:length(nodos)) {
      nodos[[variable]]$set_visitado(FALSE)
    
      nodos[[variable]]$set_added(FALSE)
      #cat("el valor de el nodo visitado  ",variable,"",nodos[[variable]]$get_visited(),"\n")
      #cat("el valor de el nodo aderido ",variable,"",nodos[[variable]]$get_added(),"\n")
  }
  
}

BFS<-function(Raiz,Meta){
  #print(str(Raiz))
  cola<- Queue$new()
  
  cola$add(Raiz)
  cat("el nuevo valor e raiz es  : ->  ",Raiz$get_dato(),"\n")
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
        #cat("el valor de el contador   es : ->  ",variable,"\n")
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
        cat("el nuevo valor e raiz es  : ->  ",Raiz$get_dato(),"\n")
        
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
  cat("el nuevo valor e raiz es  : ->  ",Raiz$get_dato(),"\n")
  #cat("el valor de la cola al entrar es : ->  ",Raiz$get_dato(),"\n")
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
        cat("el nuevo valor e raiz es  : ->  ",Raiz$get_dato(),"\n")
        
      }
      
      
    }
    
  }
  #print("sale del while")
  return(0)
  
}##fin dfs


while (opcionmenu != 3 ) {
  cat("Menu de Ingreso \n")
  cat("1)Ingresar Nodos \n")
  cat("2)Ingresar Aristas \n")
  cat("3)Salir \n")
  opcionmenu<- readline("Cual elige  ? ")
  opciones<-paste0("opcion",opcionmenu)
  switch(opciones,
         opcion1={
           cantidad<-readline("Cuantos Nodos Desea Ingresar ? ")
           n<-strtoi(cantidad)
           secuencia<-1:n
           
           for(i in 1:n ) { 
             
             x<-Nodo$new()
             x$set_dato(as.character(secuencia[i]))
             nodotag<-paste0("nodo",secuencia[i])
             nodos[[nodotag]] <-x
             
           }#fin for  
           cat("la lista de nodos creados son  : ->  ",secuencia,"\n")
         },
         opcion2={
             listahijos=list()
             info<- readline("Ingrese el Nodo Inicio ->  : ")
             nodotag<-paste0("nodo",info)
             infodatoInicio<-nodotag
             nodoInicio<-Nodo$new()
             nodoInicio<-nodos[[infodatoInicio]]
          
             cat("Ingrese los decendientes de : ->  ",infodatoInicio," hijos ")
             decendientes<- readline("Ingrese los enlaces a este nodo mediantes comas ej: 1,2 ->  : ")
             cat("aqui for \n")
             strsplit(decendientes, ",")
             n<-length(strsplit(decendientes, ",")[[1]])
             cat("aqui entra al n ",n,"\n")
             hijos<-strsplit(decendientes, ",")[[1]]
             nodoshijos<-list()
            
             for(i in 1:n ) { 
             infodatohijo<-hijos[i]
             nodotaghijos<-paste0("nodo",hijos[i])
             nodofin<-nodos[[nodotaghijos]]
             nodoshijos[[nodotaghijos]]<-nodofin
             cat("aqui entra al for ",n,"y",nodotaghijos,"i vale",i, "\n")
            
             
           }#fin for  
           nodoInicio$set_hijos(nodoshijos)
         },
         opcion3={
           cat("Ingrese la raiz ->  ")
           R<- readline("Ingrese el nodo Raiz->  : ")
           root<-strtoi(R)
           cat("\n")
           cat("Ingrese la Meta ->  ")
           M<- readline("Ingrese el nodo Meta->  : ")
           goal<-strtoi(M)
           NodoR<-nodos[[names(nodos)[root]]]
           NodoM<-nodos[[names(nodos)[goal]]]
           BFS(NodoR,NodoM)
           limpiar()
           DFS(NodoR,NodoM)
           
         }
  )#finswitch
  
  
}# fin while

