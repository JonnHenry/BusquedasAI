readInteger = function(mensaje){ 
  out <- tryCatch(
    {
      n <- readline(prompt=mensaje)
      return(as.integer(n))
    },
    error=function(cond) {
      message("El valor ingresado no es un entero")
      return(NA)
    },
    warning=function(cond) {
      message("El valor ingresado no es un entero")
      return(NULL)
    }
  )    
  return(out)
}


readCaracter = function(mensaje){ 
  out <- tryCatch(
    {
      return(as.character(readline(prompt=mensaje)))
    },
    error=function(cond) {
      message("El valor ingresado no es un simbolo aceptado")
      return(NA)
    },
    warning=function(cond) {
      message("El valor ingresado no es un simbolo aceptado")
      return(NULL)
    }
  )    
  return(out)
}

