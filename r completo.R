getwd()
setwd("/Users/itzelrubigarcialopez/Downloads/Calidad de Hospitales - data/hospitales")


rankingcompleto <- function(resultado, num) {
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  nl <- nrow(outcome)
  estado <- unique(outcome[,7])
  hospital <- vector("character", 54)
  
  if (resultado == "infarto") {
    r <- 11
  } else if (resultado == "falla") {
    r <- 17
  } else if (resultado == "neumon??a") {
    r <- 23
  } else {
    r <- 2
  }
  
  if (r>10) {
    for (i in 1:54) {
      es<- estado[i]
      x <- vector("numeric")
      y <- vector("numeric")
      xl <- 0
      
      for (k in 1:nl) {
        if(outcome[k,7] == es) {
          xl <- length(x) + 1
          length(x) <- xl
          length(y) <- xl
          x[xl] <- outcome[k,2]
          y[xl] <- outcome[k,r]
        }
      } 
      
      adver <- getOption("warn")
      options(warn = -1)
      c <- as(y,"numeric")
      options(warn = adver)
      a<-data.frame(x,c,stringsAsFactors = FALSE)
      b<- a[order(c,x),]
      
      if (num == "mejor") {
        hospital[i] <- b[1,1]
      } else if (num == "peor") {
        ficasos <- nrow(y[complete.cases(y),])
        hospital[i] <- b[ficasos,1]
      } else {
        hospital[i] <- b[num,1]
      }
    }
    m <- data.frame(hospital,estado,stringsAsFactors = FALSE)
    n <- m[order(estado,hospital),]
    n
  } else {
    "RESULTADO INVALIDO"
  }
}        

head(rankingcompleto("infarto",20), 10)