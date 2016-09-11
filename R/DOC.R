DOC <-
  function (ts, n = 7, w = 7, D1 = 0, D2 = 0, error = 1e-06, k=500,
            CEF = "MSE",type="Abbasov-Mamedova") 
  {
    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x - 
                                                                       round(x)) < tol
    if (!is.numeric(ts)) 
      stop("Error in 'ts'!")
    if (sum(is.na(ts) * 1) > 1) 
      stop("Time series contain 'NA value'!")
    if (length(n) > 1 | is.na(n) | !is.numeric(n) | n < 1 | !is.wholenumber(n)) 
      stop("Error in 'n'!")
    if (length(w) > 1 | is.null(w) | is.na(w) | !is.numeric(w) | 
        w < 2 | !is.wholenumber(w) | w > length(ts)) 
      stop("Error in 'w'!")
    if (length(D1) > 1 | is.na(D1) | !is.numeric(D1)) 
      stop("Error in 'D1'!")
    if (length(D2) > 1 | is.na(D2) | !is.numeric(D2)) 
      stop("Error in 'D2'!")
    
    if (length(k) > 1 | k <500 | is.na(k) | !is.numeric(k) | !is.wholenumber(k)) 
      stop("Error in 'k'!")
    if(type!="Abbasov-Mamedova" & type!="NFTS")stop("Error in 'type'!")
    
    if (CEF != "ME" & CEF != "MAE" & CEF != "MPE" & CEF != 
        "MAPE" & CEF != "MSE" & CEF != "RMSE") 
      stop("Error in 'CEF'!")
    
    
    
    Min <- 0
    Max <- 1
    Step <- (Max - Min)/k
    while (1) {
      C.table <- Compare.Cs(ts, n = n, w = w, D1 = D1, D2 = D2, 
                            Cs = seq(Min, Max, Step),type=type)
      C.value <- data.frame(C.table[, c("C values", CEF, paste(CEF,"xl", sep = ""))])
      if(Min==0 & Max==1)  C.value<-C.value[-c(1,k+1),]
      if(Min==0 & Max!=1)  C.value<-C.value[-c(1),]
      if(Min!=0 & Max==1)  C.value<-C.value[-c(k+1),]
      if(Min!=0 & Max!=1)  C.value<-C.value
      C.value<-data.frame(id=c(1:dim(C.value)[1]),C.value)
      
      locate <- C.value[C.value[, 3] == min(C.value[, 3]), 1]
      
      KQ0 <- c(C.value[locate[1], 2], C.value[locate[1],3])
      
      C.min <- min(C.value[, 2])
      C.max <- max(C.value[, 2])
      C.error <- C.max - C.min
      if (C.error <= error) 
        break
      C.center <- KQ0[1]
      
      Min <- C.center - Step
      Max <- C.center + Step
      
      Step <- (Max - Min)/k
    }
    
    
    names(KQ0) <- c("C value", paste(CEF, "value"))
    KQ <- KQ0
    
    
    KQ
  }
