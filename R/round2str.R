round2str <-
function(x,r=12){

#function is.vector2 {1}
is.vector2<-function(a){
c(class(a)=="vector" | class(a)=="numeric") 
}
#function is.vector2 {0}


#function is.wholenumber {1}
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x - round(x)) < tol
#function is.wholenumber {0}

#function find.text{1}
find.text<-function(ch,str){
n<-nchar(str)
text<-"t"
for(i in 1:n){
text<-c(text,substring(str,i,i))
}
vt<-c(0:n)*(text==".")
vt<-vt[vt!=0]
vt
}
#function find.text{0}

#function round2 {1}
round2<-function(x,r){
#test parameters {1}
if(!is.numeric(x))stop("'x'consit non-numeric variable!")
#test  parameters {0}

if(is.null(x) | missing(x) | is.na(x)) kq<-x else{
x<-as.character(x)
vt.cham<-find.text(".",x)

if(length(vt.cham)!=0)
kq<-substring(x,1,vt.cham+r)

if(length(vt.cham)==0)kq<-x
}


kq
}
#function round2 {0}

#function round2s {1}
round2s<-function(x,r){
if(is.vector2(x) & length(x)==1) kq<-round2(x,r)

if(is.vector2(x) & length(x)>1){
n<-length(x)
kq<-rep(0,n)

for(i in 1:n) kq[i]<-round2(x[i],r)
}

if(is.matrix(x) | is.data.frame(x)){
m<-dim(x)[1]
n<-dim(x)[2]
kq<-matrix(rep(0,m*n),ncol=n)
for(j in 1:n)
for(i in 1:m)kq[i,j]<-round2(x[i,j],r)
}

kq
}
#function round2s {0}

if(!is.wholenumber(r)) stop("'r' must be a integer number!")
if(class(x)=="table"){
if(length(dim(x))==1)x<-as.vector(x)
if(length(dim(x))==2)x<-as.matrix(x)
}
if(is.vector2(x) | is.matrix(x) | is.data.frame(x)) 
kq<-round2s(x,r)
if(class(x)=="list") stop("'x' must not be a list!")
kq
}
