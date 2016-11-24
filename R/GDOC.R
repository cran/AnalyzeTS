GDOC <-
function (ts, n = 7, w = 7, D1 = 0, D2 = 0, error = 1e-06, k=500,r=13,
CEF = "MSE",type="Abbasov-Mamedova",show.complete=TRUE){

  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x - 
                                                                     round(x)) < tol
  
if (!is.numeric(ts)) stop("Error in 'ts'!")
if (sum(is.na(ts) * 1) > 1) stop("Time series contain 'NA value'!")
if (length(D1) > 1 | is.na(D1) | !is.numeric(D1)) stop("Error in 'D1'!")
if (length(D2) > 1 | is.na(D2) | !is.numeric(D2)) stop("Error in 'D2'!")
if (length(k) > 1 | k <500 | is.na(k) | !is.numeric(k) | !is.wholenumber(k)) 
stop("Error in 'k'!")
if ((sum(!is.vector(n))>0) | (sum(is.na(n))>0) | (sum(!is.numeric(n))>0) | (sum(n < 1)>0) | 
(sum(!is.wholenumber(n))>0)) stop("Error in 'n'!")
if ((sum(!is.vector(w))>0) | (sum(is.null(w))>0) | (sum(is.na(w))>0) | 
(sum(!is.numeric(w))>0) | (sum(w < 2)>0) | (sum(!is.wholenumber(w))>0) | 
(sum(w > length(ts))>0)) stop("Error in 'w'!")
if(sum(type!="Abbasov-Mamedova" & type!="NFTS")>0)stop("Error in 'type'!")
if (CEF != "ME" & CEF != "MAE" & CEF != "MPE" & CEF != "MAPE" & CEF != "MSE" &
CEF != "RMSE") stop("Error in 'CEF'!")
if(show.complete!=0 & show.complete!=1)stop("Error in 'show.complete'!")
#-----------------------------------------------

so.mohinh<-length(n)*length(w)*length(type)
thamso.n<-rep(0,so.mohinh)
thamso.w<-rep(0,so.mohinh)
thamso.type<-rep(0,so.mohinh)
thamso.D1<-rep(D1,so.mohinh/length(D1))
thamso.D2<-rep(D2,so.mohinh/length(D2))
thamso.error<-rep(error,so.mohinh/length(error))
thamso.k<-rep(k,so.mohinh/length(k))
thamso.r<-rep(r,so.mohinh/length(r))
thamso.CEF<-rep(CEF,so.mohinh/length(CEF))
giatri.C<-rep(0,so.mohinh)
giatri.CEF<-rep(0,so.mohinh)

thamso.mohinh<-data.frame(thamso.n,thamso.w,thamso.D1,thamso.D2,
thamso.error,thamso.k,thamso.r,thamso.CEF,thamso.type,giatri.C,
giatri.CEF)

id=0
for(t.n in 1:length(n))
for(t.w in 1:length(w))
for(t.type in 1:length(type)){
id=id+1
thamso.mohinh[id,1]<-n[t.n]
thamso.mohinh[id,2]<-w[t.w]
thamso.mohinh[id,9]<-type[t.type]
}
thamso.mohinh

#-----------------------------------------------
for(i in 1:so.mohinh){
tukhoa<-paste("Adidaphat",so.mohinh,"Adidaphat",i,"Adidaphat",sep="")
C.best<-DOC(ts,n=thamso.mohinh[i,1],w=thamso.mohinh[i,2],D1=thamso.mohinh[i,3],
D2=thamso.mohinh[i,4],error=thamso.mohinh[i,5],k=thamso.mohinh[i,6],
r=thamso.mohinh[i,7],CEF=as.character(thamso.mohinh[i,8]),
type=as.character(thamso.mohinh[i,9]),show.complete=show.complete,
keyword=tukhoa)
thamso.mohinh[i,10:11]<-C.best
}
#-------------------------------------------------

if(sum(thamso.mohinh[,9]=="Abbasov-Mamedova")>0){
n1<-thamso.mohinh[thamso.mohinh[,9]=="Abbasov-Mamedova",1]
w1<-thamso.mohinh[thamso.mohinh[,9]=="Abbasov-Mamedova",2]
C1<-thamso.mohinh[thamso.mohinh[,9]=="Abbasov-Mamedova",10]
CEF1<-thamso.mohinh[thamso.mohinh[,9]=="Abbasov-Mamedova",11]
datC.best1<-data.frame(n1,w1,C1,CEF1)
colnames(datC.best1)<-c("n parameter","w parameter","C value",paste(CEF,"value"))
}
if(sum(thamso.mohinh[,9]=="Abbasov-Mamedova")==0) datC.best1<-NULL

if(sum(thamso.mohinh[,9]=="NFTS")>0){
n2<-thamso.mohinh[thamso.mohinh[,9]=="NFTS",1]
w2<-thamso.mohinh[thamso.mohinh[,9]=="NFTS",2]
C2<-thamso.mohinh[thamso.mohinh[,9]=="NFTS",10]
CEF2<-thamso.mohinh[thamso.mohinh[,9]=="NFTS",11]
datC.best2<-data.frame(n2,w2,C2,CEF2)
colnames(datC.best2)<-c("n parameter","w parameter","C value",paste(CEF,"value"))

}
if(sum(thamso.mohinh[,9]=="NFTS")==0) datC.best2<-NULL

KQ<-list(datC.best1,datC.best2)
names(KQ)<-c("Abbasov-Mamedova model","NFTS model")
#--------------------------------------------------

KQ
}
