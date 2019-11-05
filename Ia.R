nodos=list()
vectornodos<-c()
opcionmenu<- 0
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
           cat("la lista de nodos creados son  : ->  ",secuencia,"\n")
           for(i in 1:n ) {
             valorn<-as.character(i)
             valorh<-sample(1:10,1,replace=F)
             hijos<-c()
             Nodol<-c(valorn,valorh,hijos)
             cat("el valor del nodo es :  ",valorn,"su Heuristica ",valorh,"  hijos ",hijos, "\n")
            
             vectornodos[i]<-list(Nodol)
           }#fin for
           ei<-1
           while (ei == 1 ) {
             eleccionhijos<-readline("Desea ingresar nodos hijos ?  1  (si) , 2 (no) :  ")
             ei<-strtoi(eleccionhijos)
             ninfo<- readline("Ingrese el Nodo Inicio ->  : ")
             dato<-strtoi(ninfo)
             cat("Ingrese los decendientes de : ->  ",dato," hijos ")
             a<- readline("Ingrese los enlaces a este nodo mediantes comas ej: 1,2 ->  : ")
             b<-as.character(a)
             vectornodos[[dato]][3]<-list(c(b))
           }
         },
         opcion2={
         },
         opcion3={
         
         }
  )#finswitch
  
  
}# fin while


