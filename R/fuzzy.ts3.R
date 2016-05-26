fuzzy.ts3 <-
function (ts, n = 7, w = 7, D1 = 0, D2 = 0, 
C = NULL,forecast = 5,fty=c("ts","f"),trace=FALSE,plot=FALSE){

fty<-match.arg(fty)

if(fty!="ts" & fty!="f")stop("Error in 'fty'!")

if(fty=="ts")
{
#is.wholenumber function
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x - round(x)) < tol
  
#computeVt function
computeVt <- function(matrixVt, fuzzyset, w) {

n <- (dim(matrixVt)[1] - w)
cot <- dim(matrixVt)[2]
Vt <- 1:dim(matrixVt)[1]
Vt[1:w] <- NA

for (i in 1:n) {
O <- matrixVt[i:(w - 1 + (i - 1)), ]
if (w == 2) O <- t(as.matrix(O))

K <- matrixVt[w + (i - 1), ]
R <- O
for (i1 in 1:cot) 
for (j1 in 1:(w - 1)) 
if (O[j1,i1] > K[i1]) 
R[j1, i1] <- K[i1]
            
F <- 1:cot
for (i2 in 1:cot)
F[i2] <- max(R[, i2])

Vt[w + i] <- sum(F * fuzzyset$Bw)/sum(F)
}

Vt
}

#computeVt2 function
computeVt2 <- function(matrixVt2, fuzzyset,w){
cot <- dim(matrixVt2)[2]
dong <- (dim(matrixVt2)[1] - 1)
O <- matrixVt2[1:dong, ]
if (w == 2) 
O <- t(as.matrix(O))
K <- matrixVt2[(dong + 1), ]
R <- O
for (i1 in 1:cot) 
for (j1 in 1:(w - 1)) 
if (O[j1, i1] > K[i1]) R[j1, i1] <- K[i1]

F <- 1:cot
for (i2 in 1:cot) F[i2] <- max(R[, i2])
        
Vt2 <- sum(F * fuzzyset$Bw)/sum(F)
Vt2
}

 
#test all variables
if(!is.numeric(ts)) stop("Error in 'ts'!")
if(sum(is.na(ts)*1)>1) stop("Time series contain 'NA value'!")
if (length(n)>1 | is.na(n) | !is.numeric(n) | n < 1 | !is.wholenumber(n)) stop("Error in 'n'!")
if (length(w)>1 | is.null(w) | is.na(w) | !is.numeric(w) | w < 2 | !is.wholenumber(w) | w > length(ts)) stop("Error in 'w'!")
if (length(D1)>1 | is.na(D1) | !is.numeric(D1)) stop("Error in 'D1'!")
if (length(D2)>1 | is.na(D2) | !is.numeric(D2)) stop("Error in 'D2'!")
if (!is.numeric(C)) stop("Error in 'C'!")

#abbasov fuzzy sets
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
    
#my.At matrix
MATRIX <- matrix(1:(length(ts1) * n), ncol = n)
for (i in 1:length(ts1)) {
for (j in 1:n) {
my.At <- 1/(1 + (C * (ts1[i] - D$Bw[j]))^2)
MATRIX[i, j] <- my.At
}
}

V <- computeVt(MATRIX, table1, w)#Toc do tang dan so, khong co nam cuoi
N <- ts[-length(ts)] + V #Dan so du bao, khong co nam cuoi

####----------------
#du bao nam cuoi va tuong lai
V.f <- 1:(forecast+1)
N.f <- 0:(forecast+1)

N.f[1] <- ts[length(ts)]
MATRIX.f <- MATRIX[(dim(MATRIX)[1] - w + 1):dim(MATRIX)[1], ]


for (i in 1:(forecast+1)) {
V.f[i] <- computeVt2(MATRIX.f, table1,w)
N.f[i + 1] <- N.f[i] + V.f[i]

for (chuyen in 1:(dim(MATRIX.f)[1] - 1)) 
MATRIX.f[chuyen, ] <- MATRIX.f[(chuyen + 1), ]

for (j in 1:n) {
my.At <- 1/(1 + (C * (V.f[i] - D$Bw[j]))^2)
MATRIX.f[dim(MATRIX.f)[1], j] <- my.At
}
}

N.f<-N.f[-1]

Danso<-c(N,N.f)

if(is.ts(ts)) Danso<-ts(Danso,start=start(ts),frequency=frequency(ts))

Danso<-na.omit(Danso)

Danso

#Cac chi tieu sai so
Yt<-ts
Ft<-c(N,N.f[1])
et<-na.omit(Yt-Ft)
n<-length(et)
k=2
Yt<-Yt[(length(Yt)-n+1):length(Yt)]

ME = sum(et)/n

MAE = sum(abs(et))/n

MPE = sum((et/Yt)*100)/n

MAPE = sum((abs(et)/Yt)*100)/n

MSE = sum(et*et)/n

RMSE = sqrt(sum(et*et)/n)

AIC = (sum(et*et)/n)*(exp((2*k)/n))

accuracy<-c(ME=ME,MAE=MAE,MPE=MPE,MAPE=MAPE,MSE=MSE,RMSE=RMSE,AIC=AIC)

kq<-list(timeseries=Danso,accuracy=accuracy)
}

if(fty=="f")
{
#is.wholenumber function
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x - round(x)) < tol
  
#computeVt function
computeVt <- function(matrixVt, fuzzyset, w) {

n <- (dim(matrixVt)[1] - w)
cot <- dim(matrixVt)[2]
Vt <- 1:dim(matrixVt)[1]
Vt[1:w] <- NA

for (i in 1:n) {
O <- matrixVt[i:(w - 1 + (i - 1)), ]
if (w == 2) O <- t(as.matrix(O))

K <- matrixVt[w + (i - 1), ]
R <- O
for (i1 in 1:cot) 
for (j1 in 1:(w - 1)) 
if (O[j1,i1] > K[i1]) 
R[j1, i1] <- K[i1]
            
F <- 1:cot
for (i2 in 1:cot)
F[i2] <- max(R[, i2])

Vt[w + i] <- sum(F * fuzzyset$Bw)/sum(F)
}

Vt
}

#computeVt2 function
computeVt2 <- function(matrixVt2, fuzzyset,w){
cot <- dim(matrixVt2)[2]
dong <- (dim(matrixVt2)[1] - 1)
O <- matrixVt2[1:dong, ]
if (w == 2) 
O <- t(as.matrix(O))
K <- matrixVt2[(dong + 1), ]
R <- O
for (i1 in 1:cot) 
for (j1 in 1:(w - 1)) 
if (O[j1, i1] > K[i1]) R[j1, i1] <- K[i1]

F <- 1:cot
for (i2 in 1:cot) F[i2] <- max(R[, i2])
        
Vt2 <- sum(F * fuzzyset$Bw)/sum(F)
Vt2
}

 
#test all variables
if(!is.numeric(ts)) stop("Error in 'ts'!")
if(sum(is.na(ts)*1)>1) stop("Time series contain 'NA value'!")
if (length(n)>1 | is.na(n) | !is.numeric(n) | n < 1 | !is.wholenumber(n)) stop("Error in 'n'!")
if (length(w)>1 | is.null(w) | is.na(w) | !is.numeric(w) | w < 2 | !is.wholenumber(w) | w > length(ts)) stop("Error in 'w'!")
if (length(D1)>1 | is.na(D1) | !is.numeric(D1)) stop("Error in 'D1'!")
if (length(D2)>1 | is.na(D2) | !is.numeric(D2)) stop("Error in 'D2'!")
if (!is.numeric(C)) stop("Error in 'C'!")

#abbasov fuzzy sets
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
    
#my.At matrix
MATRIX <- matrix(1:(length(ts1) * n), ncol = n)
for (i in 1:length(ts1)) {
for (j in 1:n) {
my.At <- 1/(1 + (C * (ts1[i] - D$Bw[j]))^2)
MATRIX[i, j] <- my.At
}
}

V <- computeVt(MATRIX, table1, w)#Toc do tang dan so, khong co nam cuoi
N <- ts[-length(ts)] + V #Dan so du bao, khong co nam cuoi

####----------------
#du bao nam cuoi va tuong lai
V.f <- 1:(forecast+1)
N.f <- 0:(forecast+1)

N.f[1] <- N[length(N)]
MATRIX.f <- MATRIX[(dim(MATRIX)[1] - w + 1):dim(MATRIX)[1], ]


for (i in 1:(forecast+1)) {
V.f[i] <- computeVt2(MATRIX.f, table1,w)
N.f[i + 1] <- N.f[i] + V.f[i]

for (chuyen in 1:(dim(MATRIX.f)[1] - 1)) 
MATRIX.f[chuyen, ] <- MATRIX.f[(chuyen + 1), ]

for (j in 1:n) {
my.At <- 1/(1 + (C * (V.f[i] - D$Bw[j]))^2)
MATRIX.f[dim(MATRIX.f)[1], j] <- my.At
}
}

N.f<-N.f[-1]

Danso<-c(N,N.f)

if(is.ts(ts)) Danso<-ts(Danso,start=start(ts),frequency=frequency(ts))

Danso<-na.omit(Danso)

Danso

#Cac chi tieu sai so
Yt<-ts
Ft<-c(N,N.f[1])
et<-na.omit(Yt-Ft)
n<-length(et)
Yt<-ts[(length(ts)-n+1):length(ts)]
k=2

ME = sum(et)/n

MAE = sum(abs(et))/n

MPE = sum((et/Yt)*100)/n

MAPE = sum((abs(et)/Yt)*100)/n

MSE = sum(et*et)/n

RMSE = sqrt(sum(et*et)/n)

AIC = (sum(et*et)/n)*(exp((2*k)/n))

accuracy<-c(ME=ME,MAE=MAE,MPE=MPE,MAPE=MAPE,MSE=MSE,RMSE=RMSE,AIC=AIC)

kq<-list(timeseries=Danso,accuracy=accuracy)
}

kq
}
