Compare.Cs <-
function (ts, n = 7, w = 7, D1 = 0, D2 = 0, Cs = NULL, type="Abbasov-Mamedova",
complete=NULL) 
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
    if (sum(!is.numeric(Cs))*1>0) 
        stop("Error in 'Cs'!")
   if(type!="Abbasov-Mamedova" & type!="NFTS")stop("Error in 'type'!")

    if(type=="Abbasov-Mamedova")
{
    if(!is.null(complete)){
    if(complete[1]==0){
     time.tt<-"Calculating..."
}
}

    computeVt <- function(matrixVt, fuzzyset, w) {
        n <- (dim(matrixVt)[1] - w)
        cot <- dim(matrixVt)[2]
        Vt <- 1:dim(matrixVt)[1]
        Vt[1:w] <- NA
        for (i in 1:n) {
            O <- matrixVt[i:(w - 1 + (i - 1)), ]
            if (w == 2) 
                O <- t(as.matrix(O))
            K <- matrixVt[w + (i - 1), ]
            R <- O
            for (i1 in 1:cot) for (j1 in 1:(w - 1)) if (O[j1, 
                i1] > K[i1]) 
                R[j1, i1] <- K[i1]
            F <- 1:cot
            for (i2 in 1:cot) F[i2] <- max(R[, i2])
            Vt[w + i] <- sum(F * fuzzyset$Bw)/sum(F)
        }
        Vt
    }
    
    ts1 <- as.vector(diff(ts))
    min.x = min(ts1) - D1
    max.x = max(ts1) + D2
    h = (max.x - min.x)/n
    k <- 1:(n + 1)
    U <- 1:n
    for (i in 1:(n + 1)) {
        if (i == 1) 
            k[i] = min.x
        else {
            k[i] = min.x + (i - 1) * h
            U[i - 1] = paste("u", i - 1, sep = "")
        }
    }
    D <- data.frame(U, low = k[1:n], up = k[2:(n + 1)])
    D$Bw <- (1/2) * (D$low + D$up)
    table1 <- D
    ME <- 1:length(Cs)
    MAE <- 1:length(Cs)
    MPE <- 1:length(Cs)
    MAPE <- 1:length(Cs)
    MSE <- 1:length(Cs)
    RMSE <- 1:length(Cs)
#-------------------------------------------------------------------
if(!is.null(complete)){
if(complete[1]==0){
dev.new(width = 7, height = 3)
barplot(0,horiz=1,xlim=c(0,100),col="lightblue",
main=c("Complete: 0%",paste("Time remaining:",time.tt)),
xlab="Computing speed: Calculating...",cex.main=0.8)
}
dong<-"ok"}
#-------------------------------------------------------------------
time1<-Sys.time()

for (t in 1:length(Cs)) {
if(t==2) {
time2<-Sys.time()
time<-time2-time1
time<-substr(time,1,nchar(time))
time<-as.numeric(time)
if(time==0) time<-0.02
}
if(!is.null(complete)){
if(t>1){
time.re<-time*c(complete[2]-complete[1])

giay<-time.re
phut<-time.re/60
gio<-time.re/3600
if(as.numeric(gio)>1) time.tt<-paste("About",round2str(gio,2),"hours")
else
if(as.numeric(phut)>1) time.tt<-paste("About",round2str(phut,2),"minutes")
else
if(as.numeric(giay)>0) time.tt<-paste("About",round2str(giay,2),"seconds")
}
}
#-------------------------------------------------------------------

if(!is.null(complete) & t>1){

if((100*(complete[1]/complete[2]))<99){
complete[1]<-complete[1]+1
barplot(100*(complete[1]/complete[2]),horiz=1,xlim=c(0,100),col="lightblue",
main=c(paste("Complete: ",round2str(100*(complete[1]/complete[2]),2),"%",sep=""),
paste("Time remaining:",time.tt)),
xlab=paste("Computing speed:",round(1/time),"loops/second"),cex.main=0.8)
}
if(((100*(complete[1]/complete[2]))>=99) & dong=="ok"){
dev.off()
dong<-"no"
}
}
#-------------------------------------------------------------------
        C <- Cs[t]
        MATRIX <- matrix(1:(length(ts1) * n), ncol = n)
        for (i in 1:length(ts1)) {
            for (j in 1:n) {
                my.At <- 1/(1 + (C * (ts1[i] - table1$Bw[j]))^2)
                MATRIX[i, j] <- my.At
            }
        }
        V <- computeVt(MATRIX, table1, w)
        N <- c(NA, ts[-length(ts)] + V)
        danso <- N
        et <- na.omit(ts - danso)
        ne <- length(et)
        Yt <- ts[(length(ts) - ne + 1):length(ts)]
        ME[t] = sum(et)/ne
        MAE[t] = sum(abs(et))/ne
        MPE[t] = sum((et/Yt) * 100)/ne
        MAPE[t] = sum((abs(et)/Yt) * 100)/ne
        MSE[t] = sum(et * et)/ne
        RMSE[t] = sqrt(sum(et * et)/ne)
    }
    accuracy <- data.frame(ME = ME, MAE = MAE, MPE = MPE, MAPE = MAPE, 
        MSE = MSE, RMSE = RMSE)
    sort.accuracy <- accuracy
    for (xieploai in 1:6) {
        saiso <- accuracy[, xieploai]
        xl <- 1:length(saiso)
        saisoxl <- sort(saiso)
        for (k in 1:length(saiso)) {
            for (l in 1:length(xl)) {
                if (saiso[k] == saisoxl[l]) {
                  xl[k] <- l
                  break
                }
            }
        }
        sort.accuracy[, xieploai] <- xl
    }
    table2 <- data.frame(Cs, accuracy, sort.accuracy)
    names(table2) <- c("C values", "ME", "MAE", "MPE", "MAPE", 
        "MSE", "RMSE", "MExl", "MAExl", "MPExl", "MAPExl", "MSExl", 
        "RMSExl")
    answer <- table2
}




  if(type=="NFTS"){
  if(!is.null(complete)){
    if(complete[1]==0){
     time.tt<-"Calculating..."
}
}  
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x - 
        round(x)) < tol
    computeVt <- function(matrixVt, fuzzyset, w) {
        n <- (dim(matrixVt)[1] - w)
        cot <- dim(matrixVt)[2]
        Vt <- 1:dim(matrixVt)[1]
        Vt[1:w] <- NA
        for (i in 1:n) {
            O <- matrixVt[i:(w - 1 + (i - 1)), ]
            if (w == 2) 
                O <- t(as.matrix(O))
            K <- matrixVt[w + (i - 1), ]
            R <- O
            for (i1 in 1:cot) for (j1 in 1:(w - 1)) if (O[j1, 
                i1] > K[i1]) 
                R[j1, i1] <- K[i1]
            F <- 1:cot
            for (i2 in 1:cot) F[i2] <- mean(R[, i2])
            Vt[w + i] <- sum(F * fuzzyset$Bw)/sum(F)
        }
        Vt
    }
    computeVt2 <- function(matrixVt2, fuzzyset, w) {
        cot <- dim(matrixVt2)[2]
        dong <- (dim(matrixVt2)[1] - 1)
        O <- matrixVt2[1:dong, ]
        if (w == 2) 
            O <- t(as.matrix(O))
        K <- matrixVt2[(dong + 1), ]
        R <- O
        for (i1 in 1:cot) for (j1 in 1:(w - 1)) if (O[j1, i1] > 
            K[i1]) 
            R[j1, i1] <- K[i1]
        F <- 1:cot
        for (i2 in 1:cot) F[i2] <- mean(R[, i2])
        Vt2 <- sum(F * fuzzyset$Bw)/sum(F)
        Vt2
    }

   
    ts1 <- as.vector(diff(ts))
    min.x = min(ts1) - D1
    max.x = max(ts1) + D2
    h = (max.x - min.x)/n
    k <- 1:(n + 1)
    U <- 1:n
    for (i in 1:(n + 1)) {
        if (i == 1) 
            k[i] = min.x
        else {
            k[i] = min.x + (i - 1) * h
            U[i - 1] = paste("u", i - 1, sep = "")
        }
    }
    D <- data.frame(U, low = k[1:n], up = k[2:(n + 1)])
    D$Bw <- (1/2) * (D$low + D$up)
    table1 <- D
    ME <- 1:length(Cs)
    MAE <- 1:length(Cs)
    MPE <- 1:length(Cs)
    MAPE <- 1:length(Cs)
    MSE <- 1:length(Cs)
    RMSE <- 1:length(Cs)

#-------------------------------------------------------------------
if(!is.null(complete)){
if(complete[1]==0){
dev.new(width = 7, height = 3)
barplot(0,horiz=1,xlim=c(0,100),col="lightblue",
main=c("Complete: 0%",paste("Time remaining:",time.tt)),
xlab="Computing speed: Calculating...",cex.main=0.8)
}
dong<-"ok"
}
#-------------------------------------------------------------------
time1<-Sys.time()

    for (t in 1:length(Cs)) {

if(t==2) {
time2<-Sys.time()
time<-time2-time1
time<-substr(time,1,nchar(time))
time<-as.numeric(time)
if(time==0) time<-0.02
}
if(!is.null(complete)){
if(t>1){
time.re<-time*c(complete[2]-complete[1])

giay<-time.re
phut<-time.re/60
gio<-time.re/3600
if(as.numeric(gio)>1) time.tt<-paste("About",round2str(gio,2),"hours")
else
if(as.numeric(phut)>1) time.tt<-paste("About",round2str(phut,2),"minutes")
else
if(as.numeric(giay)>0) time.tt<-paste("About",round2str(giay,2),"seconds")
}
}
#-------------------------------------------------------------------
if(!is.null(complete) & t>1){
if((100*(complete[1]/complete[2]))<99){
complete[1]<-complete[1]+1
barplot(100*(complete[1]/complete[2]),horiz=1,xlim=c(0,100),col="lightblue",
main=c(paste("Complete: ",round2str(100*(complete[1]/complete[2]),2),"%",sep=""),
paste("Time remaining:",time.tt)),
xlab=paste("Computing speed:",round(1/time),"loops/second"),cex.main=0.8)
}
if(((100*(complete[1]/complete[2]))>=99) & dong=="ok"){
dev.off()
dong<-"no"
}
}
#-------------------------------------------------------------------
     
        C <- Cs[t]
        MATRIX <- matrix(1:(length(ts1) * n), ncol = n)
        for (i in 1:length(ts1)) {
            for (j in 1:n) {
                my.At <- 1/(1 + (C * (ts1[i] - table1$Bw[j]))^2)
                MATRIX[i, j] <- my.At
            }
        }
        V <- computeVt(MATRIX, table1, w)
        N <- ts[-length(ts)] + V
        MATRIX1 <- MATRIX[(dim(MATRIX)[1] - w + 1):dim(MATRIX)[1], 
            ]
        V1 <- computeVt2(MATRIX1, table1, w)
       
        N1 <- ts[length(ts)] + V1
    
        danso <- c(N, N1)
        et <- na.omit(ts - danso)
        ne <- length(et)
        Yt <- ts[(length(ts) - ne + 1):length(ts)]
        ME[t] = sum(et)/ne
        MAE[t] = sum(abs(et))/ne
        MPE[t] = sum((et/Yt) * 100)/ne
        MAPE[t] = sum((abs(et)/Yt) * 100)/ne
        MSE[t] = sum(et * et)/ne
        RMSE[t] = sqrt(sum(et * et)/ne)
    }
    accuracy <- data.frame(ME = ME, MAE = MAE, MPE = MPE, MAPE = MAPE, 
        MSE = MSE, RMSE = RMSE)
    sort.accuracy <- accuracy
    for (xieploai in 1:6) {
        saiso <- accuracy[, xieploai]
        xl <- 1:length(saiso)
        saisoxl <- sort(saiso)
        for (k in 1:length(saiso)) {
            for (l in 1:length(xl)) {
                if (saiso[k] == saisoxl[l]) {
                  xl[k] <- l
                  break
                }
            }
        }
        sort.accuracy[, xieploai] <- xl
    }
    table2 <- data.frame(Cs, accuracy, sort.accuracy)
    names(table2) <- c("C values", "ME", "MAE", "MPE", "MAPE", 
        "MSE", "RMSE", "MExl", "MAExl", "MPExl", "MAPExl", "MSExl", 
        "RMSExl")
    answer <- table2
  
}

  answer
}
