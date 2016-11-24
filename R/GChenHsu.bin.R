GChenHsu.bin <-
function(list,n.subset){
if(class(list)!="list")
stop("'list' must be result returning from Gfuzzy.ts1(,type=(...,\"Chen-Hsu\"),bin=\"NUL\") function!")

str.real<-substring(names(list)[1],1,17)
str.test<-"Chen-Hsu model of"
if(str.real!=str.test)
stop("'list' must be result returning from Gfuzzy.ts1(,type=(...,\"Chen-Hsu\"),bin=\"NUL\") function!")

if(class(n.subset)!="list") stop("'n.subset' must be a \"list\" object!")
if(length(list)!=length(n.subset))stop("Length of 'list' must be equal length of 'n.subset'!")

n<-length(list)
bin<-vector("list",n)
for(i in 1:n){
bin[[i]]<-ChenHsu.bin(list[[i]],n.subset=n.subset[[i]])
}

ten.bin<-rep(0,n)
for(j in 1:n)
ten.bin[j]<-paste("The bin value for Chen-Hsu model of",sum(n.subset[[j]]),"fuzzy set")

names(bin)<-ten.bin

bin
}
