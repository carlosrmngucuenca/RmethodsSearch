######################################################################
# Create the Node class
#
# 
Nodo<- setClass(
  # Set the name for the class
  "Nodo",
  
  # Define the slots
  slots = c(
    valor = "numeric",
    visitado   = "logical",
    lista="list"
    #hijo="Nodo"
    
    
  )

)


# create a method to assign the value of the location
setGeneric(name="setvalor",
           def=function(theObject,valor)
           {
             standardGeneric("setvalor")
           }
)

setMethod(f="setvalor",
          signature="Nodo",
          definition=function(theObject,valor)
          {
            theObject@valor <- valor
            validObject(theObject)
            return(theObject)
          }
)

# create a method to get the value of the location
setGeneric(name="getvalor",
           def=function(theObject)
           {
             standardGeneric("getvalor")
           }
)

setMethod(f="getvalor",
          signature="Nodo",
          definition=function(theObject)
          {
            return(theObject@valor)
          }
)

# create a method to assign the value of the location
setGeneric(name="sethijos",
           def=function(theObject,hijo)
           {
             standardGeneric("sethijos")
           }
)

setMethod(f="sethijos",
          signature="Nodo",
          definition=function(theObject,hijo)
          {
            theObject@hijo <-hijo
            validObject(theObject)
            return(theObject)
          }
)

# create a method to assign the value of the location
setGeneric(name="setlistanodos",
           def=function(theObject,Nodo)
           {
             standardGeneric("setlistanodos")
           }
)

setMethod(f="setlistanodos",
          signature="Nodo",
          definition=function(theObject,Nodo)
          {
            pos<-paste(Nodo@valor)
            print(pos)
            theObject@lista[pos] <-list(Nodo)
            validObject(theObject)
            return(theObject)
          }
)


# create a method to assign the value of the location
setGeneric(name="metodoBusquedaAnchura",
           def=function(theObject,Nodo)
           {
             standardGeneric("metodoBusquedaAnchura")
           }
)

setMethod(f="metodoBusquedaAnchura",
          signature="Nodo",
          definition=function(Raiz,Meta)
          {
            
            
            pos<-paste(Nodo@valor)
            print(pos)
            theObject@lista[pos] <-list(Nodo)
            validObject(theObject)
            return(theObject)
          }
)



