Abbasov.Cs2 <-
function(ts,n=7,w=7,D1=0,D2=0,Cs=NULL){

#is.integer function
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


#test all variables
if(!is.numeric(ts)) stop("Error in 'ts'!")
if(sum(is.na(ts)*1)>1) stop("Time series contain 'NA value'!")
if (length(n)>1 | is.na(n) | !is.numeric(n) | n < 1 | !is.wholenumber(n)) stop("Error in 'n'!")
if (length(w)>1 | is.null(w) | is.na(w) | !is.numeric(w) | w < 2 | !is.wholenumber(w) | w > length(ts)) stop("Error in 'w'!")
if (length(D1)>1 | is.na(D1) | !is.numeric(D1)) stop("Error in 'D1'!")
if (length(D2)>1 | is.na(D2) | !is.numeric(D2)) stop("Error in 'D2'!")
if (!is.numeric(Cs)) stop("Error in 'Cs'!")
  
#abbasov fuzzy sets
ts1 <- as.vector(diff(ts))
min.x = min(ts1) - D1
max.x = max(ts1) + D2
h = (max.x - min.x)/n
k <- 1:(n + 1)
U <- 1:n
for (i in 1:(n + 1)) {
if (i == 1)  k[i] = min.x
else 
{
k[i] = min.x + (i - 1) * h
U[i - 1] = paste("u", i - 1, sep = "")
}
}
D <- data.frame(U, low = k[1:n], up = k[2:(n + 1)])
D$Bw <- (1/2) * (D$low + D$up)
table1 <- D

MAE<-1:length(Cs)

for(t in 1:length(Cs)){
C<-Cs[t]

#my.At matrix
MATRIX <- matrix(1:(length(ts1) * n), ncol = n)
for (i in 1:length(ts1)) {
for (j in 1:n) {
my.At <- 1/(1 + (C * (ts1[i] - table1$Bw[j]))^2)
MATRIX[i, j] <- my.At
}
}
   
V <- computeVt(MATRIX, table1, w)#Toc do tang dan so
N <- c(NA,ts[-length(ts)] + V) #Dan so du bao

danso<-N

e<-na.omit(ts-danso)
ne<-length(e)
MAE[t]<-sum(abs(e))/ne
}

table2<-data.frame(Cs,MAE)
names(table2)<-c("C values","MAE values")

for(v in 1:dim(table2)[1]) if(table2[v,2]==min(table2[,2]))break
answer<-c("C value"=table2[v,1],"MAE value"=table2[v,2])

answer
}
