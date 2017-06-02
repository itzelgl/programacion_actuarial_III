getwd()
setwd("/Users/itzelrubigarcialopez/Downloads/Calidad de Hospitales - data/hospitales")

rankhospital <- function(estado, resultado, num) {
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  nl <- nrow(outcome)
  
  if (resultado == "infarto") {
    r <- 11
  } else if (resultado == "falla") {
    r <- 17
  } else if (resultado == "neumon??a") {
    r <- 23
  } else {
    r <- 2
  }
  
  x <- vector("numeric")
  y <- vector("numeric")
  
  if (r>10){
    xl <- 0
    for (i in 1:nl) {
      if (outcome[i,7] == estado) {
        xl <- length(x) + 1
        length(x) <- xl
        length(y) <- xl
        x[xl] <- outcome[i,2]
        y[xl] <- outcome[i,r]
      }
    }
    
    if (xl>0) {
      adver <- getOption("warn")
      options(warn = -1)
      c <- as(y,"numeric")
      options(warn = adver)
      a <- data.frame(x,c,stringsAsFactors = FALSE)
      b <- a[order(c,x),]
      
      if (num == "mejor") {
        b[1,1]
      } else if (num == "peor") {
        lpeor <- nrow(b[complete.cases(b),])
        b[lpeor,1]
      } else {
        b[num,1]
      }
    } else {
      "ESTADO INVALIDO"
    }
  } else {
    "RESULTADO INVALIDO"
  }
}


rankhospital("TX", "infarto", "peor")