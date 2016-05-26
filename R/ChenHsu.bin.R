ChenHsu.bin <-
function(table,ni){
if(length(ni)!=dim(table)[1]) stop("Error in ni!")

b<-1:(sum(ni)-1)
t=0

for(j in 1:length(ni)){
hi<-(table[j,3]-table[j,2])/ni[j]
for(i in 1:ni[j]){
t=t+1
b[t]<-table[j,2]+i*hi
}
}
b<-b[-length(b)]
b
}
