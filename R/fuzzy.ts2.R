fuzzy.ts2 <-
function (ts, n = 7, w = 7, D1 = 0, D2 = 0, C = NULL, forecast = 5, 
r=12,trace = FALSE, plot = FALSE,grid=FALSE,type="Abbasov-Mamedova") 
{

  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x - 
        round(x)) < tol
  
    namthang <- function(data.ts) {
        batdau <- start(data.ts)
        tanso <- frequency(data.ts)
        nam1 <- batdau[1]
        thang1 <- batdau[2]
        ketthuc <- end(data.ts)
        nam2 <- ketthuc[1]
        thang2 <- ketthuc[2]
        namkq <- 1:length(data.ts)
        thangkq <- 1:length(data.ts)
        index = 0
        for (nam in nam1:nam2) for (thang in 1:tanso) if (nam != 
            nam1 || thang >= thang1) {
            index = index + 1
            namkq[index] <- nam
            thangkq[index] <- thang
            if (nam == nam2 & thang == thang2) 
                break
        }
        if (tanso == 4) {
            thangkq[thangkq == 1] <- "Q1"
            thangkq[thangkq == 2] <- "Q1"
            thangkq[thangkq == 3] <- "Q1"
            thangkq[thangkq == 4] <- "Q1"
            print <- paste(namkq, thangkq, sep = " ")
        }
        else if (tanso == 12) {
            thangkq[thangkq == 1] <- "Jan"
            thangkq[thangkq == 2] <- "Feb"
            thangkq[thangkq == 3] <- "Mar"
            thangkq[thangkq == 4] <- "Apr"
            thangkq[thangkq == 5] <- "May"
            thangkq[thangkq == 6] <- "Jun"
            thangkq[thangkq == 7] <- "Jul"
            thangkq[thangkq == 8] <- "Aug"
            thangkq[thangkq == 9] <- "Sep"
            thangkq[thangkq == 10] <- "Oct"
            thangkq[thangkq == 11] <- "Nov"
            thangkq[thangkq == 12] <- "Dec"
            print <- paste(namkq, thangkq, sep = " ")
        }
        else if (tanso == 7) {
            thangkq[thangkq == 1] <- "Mon"
            thangkq[thangkq == 2] <- "Tue"
            thangkq[thangkq == 3] <- "Wed"
            thangkq[thangkq == 4] <- "Thu"
            thangkq[thangkq == 5] <- "Fri"
            thangkq[thangkq == 6] <- "Sat"
            thangkq[thangkq == 7] <- "Sun"
            print <- paste(namkq, thangkq, sep = " ")
        }
        else if (tanso != 1) 
            print <- paste("(", namkq, ",", thangkq, ")", sep = "")
        else print <- namkq
        print
    }



  if (!is.numeric(ts)) 
        stop("Error in 'ts'!")
    if (!is.ts(ts)) 
        stop("Error in 'ts'!")
    else if (!is.null(dim(ts))) 
        stop("Error in 'ts'!")
    kt <- 0
    for (i in 1:length(ts)) if (is.na(ts[i])) 
        kt = kt + 1
    if (kt > 0) 
        stop("'ts' contain NA!")
    if (is.na(n) || !is.numeric(n) || n < 1 || !is.wholenumber(n)) 
        stop("Error in 'n'!")
    if (is.null(w) || is.na(w) || !is.numeric(w) || w < 2 || 
        !is.wholenumber(w)) 
        stop("Error in 'w'!")
    if (is.null(C) || is.na(C) || !is.numeric(C)) 
        stop("Error in 'C'!")
    if (is.na(D1) || !is.numeric(D1)|| length(D1)>1) 
        stop("Error in 'D1'!")
    if (is.na(D2) || !is.numeric(D2)|| length(D2)>1) 
        stop("Error in 'D2'!")
    if (is.null(forecast) || is.na(forecast) || !is.numeric(forecast) || 
        forecast < 1 || !is.wholenumber(forecast)) 
        stop("Error in 'forecast'!")
    if (w >= length(ts)) 
        stop("Error in 'w'!")
 
    if (plot != 0 & plot != 1) 
        stop("Error in 'plot'!")
 if (is.na(r) || !is.numeric(r) || r < 0 || !is.wholenumber(r)) 
        stop("Error in 'r'!")
    if (trace != 0 & trace != 1) 
        stop("Error in 'trace'!")
 if(type!="Abbasov-Mamedova" & type!="NFTS")stop("Error in 'type'!")


if(type=="Abbasov-Mamedova")
{
  
    computeVi <- function(M, table, w) {
        n <- (dim(M)[1] - w)
        cot <- dim(M)[2]
        Vi <- 1:dim(M)[1]
        Vi[1:w] <- NA
        for (i in 1:n) {
            O <- M[i:(w - 1 + (i - 1)), ]
            if (w == 2) 
                O <- t(as.matrix(O))
            K <- M[w + (i - 1), ]
            R <- O
            for (i1 in 1:cot) for (j1 in 1:(w - 1)) if (O[j1, 
                i1] > K[i1]) 
                R[j1, i1] <- K[i1]
            F <- 1:cot
            for (i2 in 1:cot) F[i2] <- max(R[, i2])
            Vi[w + i] <- sum(F * table$Bw)/sum(F)
        }
        Vi
    }
    computeVi2 <- function(M, table, w) {
        cot <- dim(M)[2]
        dong <- (dim(M)[1] - 1)
        O <- M[1:dong, ]
        if (w == 2) 
            O <- t(as.matrix(O))
        K <- M[(dong + 1), ]
        R <- O
        for (i1 in 1:cot) for (j1 in 1:(w - 1)) if (O[j1, i1] > 
            K[i1]) 
            R[j1, i1] <- K[i1]
        F <- 1:cot
        for (i2 in 1:cot) F[i2] <- max(R[, i2])
        Vi <- sum(F * table$Bw)/sum(F)
        Vi
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
    D <- data.frame(set=U, low = k[1:n], up = k[2:(n + 1)])
    D$Bw <- (1/2) * (D$low + D$up)
    table1 <- D
    thoidiem <- namthang(ts)
    Ai <- 1:length(ts1)
    MATRIX <- matrix(1:(length(ts1) * n), ncol = n)
    for (i in 1:length(ts1)) {
        temp = ""
        for (j in 1:n) {
            my.At <- 1/(1 + (C * (ts1[i] - D$Bw[j]))^2)
            MATRIX[i, j] <- my.At
            At.j <- paste("(", round2str(my.At,r), "/u", j, sep = "", ")")
            if (j == 1) 
                temp <- paste(temp, At.j, sep = "")
            else temp <- paste(temp, At.j, sep = ",")
        }
        Ai[i] <- paste("A[", thoidiem[i + 1], "]={", temp, "}", 
            sep = "")
    }
    table2 <- data.frame(point = thoidiem, ts = ts, diff.ts = c(NA, 
        ts1))
    table3 <- c(NA, Ai)
    V <- computeVi(MATRIX, table1, w)
    N <- c(NA, (ts[-length(ts)] + V))
    danso1 <- N
    V <- c(NA, V)
    table4 <- data.frame(point = thoidiem, interpolate = N, diff.interpolate = V)
    accuracy <- av.res(Y = data.frame(ts), F = data.frame(Abbasov.Mamedova = table4[, 
        2]))
    table4 <- na.omit(table4)
    rownames(table4) <- c(1:dim(table4)[1])
    V <- 1:forecast
    N <- 0:forecast
    Ai <- 1:forecast
  
   N[1] <- ts[length(ts)]

    MT <- MATRIX[(dim(MATRIX)[1] - w + 1):dim(MATRIX)[1], ]
    temp <- c(as.vector(ts), 1:forecast)
    temp <- ts(temp, start = start(ts), frequency = frequency(ts))
    temp <- namthang(temp)
    temp <- temp[(length(ts) + 1):length(temp)]
    thoidiem <- temp
    for (i in 1:forecast) {
        V[i] <- computeVi2(MT, table1, w)
        N[i + 1] <- N[i] + V[i]
        for (chuyen in 1:(dim(MT)[1] - 1)) MT[chuyen, ] <- MT[(chuyen + 
            1), ]
        temp = ""
        for (j in 1:n) {
            my.At <- 1/(1 + (C * (V[i] - D$Bw[j]))^2)
            MT[dim(MT)[1], j] <- my.At
            At.j <- paste("(", round2str(my.At,r), "/u", j, sep = "", ")")
            if (j == 1) 
                temp <- paste(temp, At.j, sep = "")
            else temp <- paste(temp, At.j, sep = ",")
        }
        Ai[i] <- paste("A[", thoidiem[i], "]={", temp, "}", sep = "")
    }
    N <- N[-1]
    danso2 <- N
    table5 <- data.frame(point = thoidiem, forecast = N, diff.forecast = V)
    table6 <- Ai
    colnames(table1)<-c("set","dow","up","mid")
    KQ <- list(type = "Abbasov-Manedova model", table1 = table1, table2 = table2, 
        table3 = table3, table4 = table4, table5 = table5, table6 = table6, 
        accuracy = accuracy)
 Danso <- c(danso1, danso2)
        if (is.ts(ts)){ 
            Danso <- ts(Danso, start = start(ts), frequency = frequency(ts))
danso1<-ts(danso1, start = start(Danso), frequency = frequency(Danso))
danso2<-ts(danso2, end = end(Danso), frequency = frequency(Danso))
}    
    if (trace == TRUE) 
        MO <- KQ
    else if (trace == FALSE) {    
MO <- list(interpolate = danso1, forecast=danso2)
    }
    else 
MO <- c("'trace' must be 'TRUE' or 'FALSE'")
}



if(type=="NFTS"){
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
    thoidiem <- namthang(ts)
    Ai <- 1:length(ts1)
    MATRIX <- matrix(1:(length(ts1) * n), ncol = n)
    for (i in 1:length(ts1)) {
        temp = ""
        for (j in 1:n) {
            my.At <- 1/(1 + (C * (ts1[i] - D$Bw[j]))^2)
            MATRIX[i, j] <- my.At
            At.j <- paste("(", round2str(my.At,r), "/u", j, sep = "", ")")
            if (j == 1) 
                temp <- paste(temp, At.j, sep = "")
            else temp <- paste(temp, At.j, sep = ",")
        }
        Ai[i] <- paste("A[", thoidiem[i + 1], "]={", temp, "}", 
            sep = "")
    }
    table2 <- data.frame(point = thoidiem, ts = ts, diff.ts = c(NA, 
        ts1))
    table3 <- c(NA, Ai)
    V <- computeVt(MATRIX, table1, w)
    N <- ts[-length(ts)] + V
    V <- c(NA, V)
    N <- c(N, NA)
    table4 <- data.frame(point = thoidiem, interpolate = N, diff.interpolate = V)
    V.f <- 1:(forecast + 1)
    N.f <- 0:(forecast + 1)
    Ai.f <- 1:(forecast + 1)
   
   N.f[1] <- ts[length(ts)]

    MATRIX.f <- MATRIX[(dim(MATRIX)[1] - w + 1):dim(MATRIX)[1],]
    temp <- c(as.vector(ts), 1:(forecast + 1))
    temp <- ts(temp, start = start(ts), frequency = frequency(ts))
    temp <- namthang(temp)
    temp <- temp[(length(ts) + 1):length(temp)]
    thoidiem <- temp
    for (i in 1:(forecast + 1)) {
        V.f[i] <- computeVt2(MATRIX.f, table1, w)
        N.f[i + 1] <- N.f[i] + V.f[i]
        for (chuyen in 1:(dim(MATRIX.f)[1] - 1)) MATRIX.f[chuyen, 
            ] <- MATRIX.f[(chuyen + 1), ]
        temp = ""
        for (j in 1:n) {
            my.At <- 1/(1 + (C * (V.f[i] - D$Bw[j]))^2)
            MATRIX.f[dim(MATRIX.f)[1], j] <- my.At
            At.j <- paste("(", round2str(my.At,r), "/u", j, sep = "", ")")
            if (j == 1) 
                temp <- paste(temp, At.j, sep = "")
            else temp <- paste(temp, At.j, sep = ",")
        }
        Ai.f[i] <- paste("A[", thoidiem[i], "]={", temp, "}", 
            sep = "")
    }
    N.f <- N.f[-1]
    N[length(N)] <- N.f[1]
    N.f <- N.f[-1]
    table4$interpolate[dim(table4)[1]] <- N[length(N)]
    N.f.temp <- c(N.f, NA)
    table4 <- table4[(sum(is.na(table4$interpolate)) + 1):dim(table4)[1], 
        ]
    table5 <- data.frame(point = thoidiem, forecast = N.f.temp, 
        diff.forecast = V.f)
    table6 <- Ai.f


DS.noisuy<-N
DS.dubao<-N.f
    Danso <- c(N, N.f)
    if (is.ts(ts)){
    Danso <- ts(Danso, start = start(ts), frequency = frequency(ts))
DS.noisuy<-ts(DS.noisuy, start = start(Danso), frequency = frequency(Danso))
DS.dubao<-ts(DS.dubao, end = end(Danso), frequency = frequency(Danso))
    }




    accuracy<-av.res(Y=data.frame(ts),F=data.frame(NFTS=N))
    colnames(table1)<-c("set","dow","up","mid")
    KQ1 <- list(type = "NFTS model", table1 = table1, 
        table2 = table2, table3 = table3, table4 = table4, table5 = table5, 
        table6 = table6, accuracy = accuracy)

    KQ2 <- list(interpolate = DS.noisuy, forecast=DS.dubao)

    if (trace == 1) 
        MO <- KQ1
    else MO <- KQ2
}

#---------------
#Ve do thi {1}
if(plot==TRUE){
if(c(par()$mfrow)[1]>2 | c(par()$mfrow)[2]>2 )
warning("Graph only paint when: c(par()$mfrow)[1] < 3 & c(par()$mfrow)[1] < 3")
else{
goc<-ts
dubao<-Danso

n.dothi <- sum(par()$mfrow)
n.dothi<- n.dothi/2
if(n.dothi==1) n.dothi<-1/0.8
cex.legend<-n.dothi
main.plot<-c("Actual series vs forecated series by",paste(type,"model of", n, "fuzzy set"),
paste("with w =",w," and C =",C))



if (length(goc) < 50) {
tde1<-c(paste(main.plot[1],main.plot[2]),main.plot[3])
tde2<-main.plot

if(c(par()$mfrow)[2]==1)
gve<-list(col=c("blue","red"),cex.main=0.8,type="o",pch=c(15,17),
xlab = "point", ylab = "data",bty="l",main=tde1)

if(c(par()$mfrow)[2]==2)
gve<-list(col=c("blue","red"),cex.main=0.8,type="o",pch=c(15,17),
xlab = "point", ylab = "data",bty="l",main=tde2)

ts.plot(goc,dubao,gpars=gve)
legend("topleft", "(x,y)", c("Actual","Forecasted"), ncol=2,
col = c("blue", "red"), lty = c(1, 1), pch = c(15,17), cex = 1/cex.legend,box.lty=0)
}


if (length(goc) > 49) {
tde1<-c(paste(main.plot[1],main.plot[2]),main.plot[3])
tde2<-main.plot

if(c(par()$mfrow)[2]==1)
gve<-list(col=c("blue","red"),cex.main=0.8,type="l",
xlab = "point", ylab = "data",bty="l",main=tde1)

if(c(par()$mfrow)[2]==2)
gve<-list(col=c("blue","red"),cex.main=0.8,type="l",
xlab = "point", ylab = "data",bty="l",main=tde2)

ts.plot(goc,dubao,gpars=gve)
legend("topleft", "(x,y)", c("Actual","Forecasted"), ncol=2,
col = c("blue", "red"), lty = c(1, 1), cex = 1/cex.legend,box.lty=0)
}

if(grid==1)grid.on(v=0)
}
}
#Ve do thi {0}
#------------------------------

MO
}
