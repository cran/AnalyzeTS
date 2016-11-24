Gfuzzy.ts2 <-
function (ts,n=7,w=7,D1=0,D2=0,C=list(C1=NULL,C2=NULL),forecast=5, 
plot=FALSE,grid=FALSE,type="Abbasov-Mamedova") 
{

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x-round(x)) < tol

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

#---------------------------------------------------------

if (!is.numeric(ts)) stop("Error in 'ts'!")
if (!is.ts(ts)) stop("Error in 'ts'!")
else if (!is.null(dim(ts))) stop("Error in 'ts'!")
kt <- 0
for (i in 1:length(ts)) if (is.na(ts[i])) kt = kt + 1
if (kt > 0) stop("'ts' contain NA!")
if (is.na(D1) || !is.numeric(D1)|| length(D1)>1) stop("Error in 'D1'!")
if (is.na(D2) || !is.numeric(D2)|| length(D2)>1) stop("Error in 'D2'!")
if (is.null(forecast) || is.na(forecast) || !is.numeric(forecast) || 
forecast < 1 || !is.wholenumber(forecast)) 
stop("Error in 'forecast'!")
if (plot != 0 & plot != 1) stop("Error in 'plot'!")

if ((sum(!is.vector(n))>0) | (sum(is.na(n))>0) | (sum(!is.numeric(n))>0) | (sum(n < 1)>0) | 
(sum(!is.wholenumber(n))>0)) stop("Error in 'n'!")
if ((sum(!is.vector(w))>0) | (sum(is.null(w))>0) | (sum(is.na(w))>0) | 
(sum(!is.numeric(w))>0) | (sum(w < 2)>0) | (sum(!is.wholenumber(w))>0) | 
(sum(w > length(ts))>0)) stop("Error in 'w'!")
if(sum(type!="Abbasov-Mamedova" & type!="NFTS")>0)stop("Error in 'type'!")

if((class(C)!="list") | length(C)!=2) 
stop("'C' must be a list consiting 2 component C1 and C2 or a rusult object from GDOC function!")
if(is.null(C[[1]]) & is.null(C[[2]])) stop("'C' must not be NULL!")
tt.C<-c(0,0)
if(!is.null(C[[1]])) tt.C[1]<-1
if(!is.null(C[[2]])) tt.C[2]<-1
test.ten=0
#-------------------------------------------
#C is a rusult object from GDOC function {1}
if(sum(names(C)== c("Abbasov-Mamedova model","NFTS model"))==2){

if((tt.C[1]==1 & sum(type=="Abbasov-Mamedova")==0)|
(tt.C[1]==0 & sum(type=="Abbasov-Mamedova")==1)|
(tt.C[2]==1 & sum(type=="NFTS")==0)|
(tt.C[2]==0 & sum(type=="NFTS")==1))
stop("'type' in Gfuzzy.ts2 function must be equal 'type' in GDOC function!")

tt.nw<-vector("list",2)
if(tt.C[1]==1){
temp1<-C[[1]]
tt.nw[[1]]<-as.numeric(names(table(temp1[,1])))
tt.nw[[2]]<-as.numeric(names(table(temp1[,2])))
} 
else
if(tt.C[2]==1){
temp2<-C[[2]]
tt.nw[[1]]<-as.numeric(names(table(temp2[,1])))
tt.nw[[2]]<-as.numeric(names(table(temp2[,2])))
} 
n.C<-tt.nw[[1]]
w.C<-tt.nw[[2]]
if(length(n)!=length(n.C) | (sum(n!=n.C))>0)
stop("'n' in Gfuzzy.ts2 function must be equal 'n' in GDOC function!")
if(length(w)!=length(w.C) | (sum(w!=w.C))>0)
stop("'w' in Gfuzzy.ts2 function must be equal 'w' in GDOC function!")

C.AM<-NULL
C.NF<-NULL
if(tt.C[1]==1) C.AM<-as.numeric(as.vector(data.frame(C[[1]])$C.value))
if(tt.C[2]==1) C.NF<-as.numeric(as.vector(data.frame(C[[2]])$C.value))

test.ten=1
}
#C is a rusult object from GDOC function {0}
#-------------------------------------------
#C is a list consiting 2 component C1 and C2 {1}
if(sum(names(C)== c("C1","C2"))==2){

if((tt.C[1]==1 & sum(type=="Abbasov-Mamedova")==0)|
(tt.C[1]==0 & sum(type=="Abbasov-Mamedova")==1))
stop("C$C1 != NULL when 'type' containt \"Abbasov-Mamedova\"!")
if((tt.C[2]==1 & sum(type=="NFTS")==0)|
(tt.C[2]==0 & sum(type=="NFTS")==1))
stop("C$C2 != NULL when 'type' containt \"NFTS\"!")

C.AM<-NULL
C.NF<-NULL
if(tt.C[1]==1) C.AM<-C$C1
if(tt.C[2]==1) C.NF<-C$C2

test.ten=1
}
#C is a list consiting 2 component C1 and C2 {0}
#---------------------------------------------------------
#C is a list consiting 2 component !C1 and !C2 {0}
if(test.ten==0)stop("'C' must be a list consiting 2 component C1 and C2 or a rusult object from GDOC function!")
#---------------------------------------------------------
#Length of C is missing
if(sum(type=="Abbasov-Mamedova")==1 & length(C.AM)!=c(length(n)*length(w)))
stop("Length of C$C1 must be equal length(n)*length(w)!")
if(sum(type=="NFTS")==1 & length(C.NF)!=c(length(n)*length(w)))
stop("Length of C$C2 must be equal length(n)*length(w)!")
#---------------------------------------------------------

so.mohinh<-length(n)*length(w)*length(type)
thamso.n<-rep(0,so.mohinh)
thamso.w<-rep(0,so.mohinh)
thamso.C<-rep(0,so.mohinh)
thamso.type<-rep(0,so.mohinh)
thamso.D1<-rep(D1,so.mohinh/length(D1))
thamso.D2<-rep(D2,so.mohinh/length(D2))
thamso.mohinh<-data.frame(thamso.n,thamso.w,thamso.C,
thamso.D1,thamso.D2,thamso.type)

id=0
for(t.n in 1:length(n))
for(t.w in 1:length(w))
for(t.type in 1:length(type)){
id=id+1
thamso.mohinh[id,1]<-n[t.n]
thamso.mohinh[id,2]<-w[t.w]
thamso.mohinh[id,6]<-type[t.type]
}

thamso.C[thamso.mohinh[,6]=="Abbasov-Mamedova"]<-C.AM
thamso.C[thamso.mohinh[,6]=="NFTS"]<-C.NF
thamso.mohinh[,3]<-thamso.C

#-----------------------------------------------
#Analyze fuzzy time series by models {1}
ts.mh<-thamso.mohinh
KQ.fuzzy<-data.frame(ts)
KQ.forecast<-data.frame(rep(0,forecast))
for(mohinh in 1:so.mohinh){
mo<-fuzzy.ts2(ts,n=ts.mh[mohinh,1],w=ts.mh[mohinh,2],D1=ts.mh[mohinh,4],
D2=ts.mh[mohinh,5],C=ts.mh[mohinh,3],forecast=forecast,type=ts.mh[mohinh,6]) 
KQ.fuzzy<-data.frame(KQ.fuzzy,mo[[1]])
KQ.forecast<-data.frame(KQ.forecast,mo[[2]])
}
#Analyze fuzzy time series by models {1}
#-----------------------------------------------

KQ.fuzzy<-KQ.fuzzy[,-1]
dimnames(KQ.fuzzy)[[1]]<-namthang(ts)
KQ.forecast<-KQ.forecast[,-1]
dimnames(KQ.forecast)[[1]]<-namthang(KQ.forecast[,1])

ten.mh<-rep(0,so.mohinh)
for(ten in 1:so.mohinh) ten.mh[ten]<-paste("model",ten,sep="")
dimnames(KQ.fuzzy)[[2]]<-ten.mh
dimnames(KQ.forecast)[[2]]<-ten.mh

KQ.infor<-thamso.mohinh[,-c(4,5)]
dimnames(KQ.infor)[[1]]<-ten.mh
dimnames(KQ.infor)[[2]]<-c("n","w","C value","type")
 
KQ<-list(information=KQ.infor,interpolate=KQ.fuzzy,forecast=KQ.forecast)


#---------------
#Ve do thi {1}
if(plot==TRUE){
if(c(par()$mfrow)[1]>2 | c(par()$mfrow)[2]>2 )
warning("Graph only paint when: c(par()$mfrow)[1] < 3 & c(par()$mfrow)[1] < 3")
else{
#------------------------------------------------
KQ.gop<-data.frame(rep(0,length(ts)+forecast))
for(gop in 1:so.mohinh){
ts.gop<-ts(c(KQ.fuzzy[,gop],KQ.forecast[,gop]),
start=start(ts),frequency=frequency(ts))
KQ.gop<-data.frame(KQ.gop,ts.gop)
}
KQ.gop<-KQ.gop[,-1]
#------------------------------------------------
ts1<-ts(c(ts,rep(NA,forecast)),start=start(ts),frequency=frequency(ts))
#------------------------------------------------
goc<-ts1
dubao<-KQ.gop
#------------------------------------------------
n.dothi <- sum(par()$mfrow)
n.dothi<- n.dothi/2
if(n.dothi==1) n.dothi<-1/0.8
cex.legend<-n.dothi
main.plot<-c("Actual series vs forecated series by","fuzzy time series models")

kieuduong<-rep(1,100000)
mausac<-rep(c("burlywood4","darkgreen","deepskyblue3","chartreuse4","firebrick2",
"darkorchid","gold","darkslateblue","deeppink","burlywood2","lightsalmon2",
"mediumpurple","mediumseagreen","greenyellow","lightslateblue","mistyrose3",
"indianred1","indianred4","maroon","orange","plum2","sienna2","orange4",
"red1","slategray3"),10000)
kieudiem<-rep(c(15,17,19),100000)

ts.ve<-thamso.mohinh[,c(1,2,6)]
ts.ve<-data.frame(ts.ve,lty=rep(0,so.mohinh),col=rep(0,so.mohinh),pch=rep(0,so.mohinh))
id=0
for(t.lty in 1:length(type))
for(t.col in 1:length(n))
for(t.pch in 1:length(w)){
id=id+1
ts.ve[id,4]<-kieuduong[id]
ts.ve[id,5]<-mausac[id]
ts.ve[id,6]<-kieudiem[id]
}
ts.ve

mo.max<-max(max(KQ.gop,na.rm=1),ts)
mo.min<-min(min(KQ.gop,na.rm=1),ts)
y.range<-mo.max-mo.min
y.min<-mo.min-1/12*(y.range)
y.max<-mo.max
if((length(n)*length(w)*length(type)>4)&(length(n)*length(type)<8))y.max<-y.max+1/10*(y.range)
if((length(n)*length(w)*length(type)>8)&(length(n)*length(type)<16))y.max<-y.max+2/10*(y.range)
if((length(n)*length(w)*length(type)>16))y.max<-y.max+3/10*(y.range)

if((c(par()$mfrow)[1]==1)&(c(par()$mfrow)[2]==1))n.chuthich<-4
if((c(par()$mfrow)[1]==1)&(c(par()$mfrow)[2]==2))n.chuthich<-2
if((c(par()$mfrow)[1]==2)&(c(par()$mfrow)[2]==1))n.chuthich<-5
if((c(par()$mfrow)[1]==2)&(c(par()$mfrow)[2]==2))n.chuthich<-3

if (length(goc) < 50) {
tde1<-c(paste(main.plot[1],main.plot[2]))
tde2<-main.plot

if(c(par()$mfrow)[2]==1)
gve<-list(col=c("black",ts.ve$col),cex.main=0.8,type="o",pch=c(8,ts.ve$pch),ylim=c(y.min,y.max),
xlab = "point", ylab = "data",bty="l",main=tde1,lty=c(1,ts.ve$lty))

if(c(par()$mfrow)[2]==2)
gve<-list(col=c("black",ts.ve$col),cex.main=0.8,type="o",pch=c(8,ts.ve$pch),ylim=c(y.min,y.max),
xlab = "point", ylab = "data",bty="l",main=tde2,lty=c(1,ts.ve$lty))

ts.plot(goc,dubao,gpars=gve)
legend("topleft", "(x,y)", c("actual",ten.mh), ncol=n.chuthich,
col=c("black",ts.ve$col),lty=c(1,ts.ve$lty),pch=c(8,ts.ve$pch),
cex = 1/cex.legend,box.lty=0)
}


if (length(goc) > 49) {
tde1<-c(paste(main.plot[1],main.plot[2]))
tde2<-main.plot

if(c(par()$mfrow)[2]==1)
gve<-list(col=c("black",ts.ve$col),cex.main=0.8,type="l",ylim=c(y.min,y.max),
xlab = "point", ylab = "data",bty="l",main=tde1,lty=c(1,ts.ve$lty))

if(c(par()$mfrow)[2]==2)
gve<-list(col=c("black",ts.ve$col),cex.main=0.8,type="l",ylim=c(y.min,y.max),
xlab = "point", ylab = "data",bty="l",main=tde2,lty=c(1,ts.ve$lty))

ts.plot(goc,dubao,gpars=gve)
legend("topleft", "(x,y)", c("actual",ten.mh), ncol=n.chuthich,
col=c("black",ts.ve$col),lty=c(1,ts.ve$lty),cex = 1/cex.legend,box.lty=0)
}

if(grid==1)grid.on(v=0)
}
}
#Ve do thi {0}
#------------------------------

KQ
}
