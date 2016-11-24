forecast.Greg.ts <-
function(object,model="ALL",n.ahead=5,plot=FALSE){

#---ham con is.wholenumber {1}---
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x-round(x)) < tol
#---ham con is.wholenumber {0}---

#-------##-------##-------##-------##-------#

#---kiem tra dau vao {1}---
if(class(object)!="list") stop("The 'object' must be resul returning from Greg.ts() function!")
if(prod(names(object)==c("Models","Actual","Interpolate","Error"))==0)
stop("The 'object' must be resul returning from Greg.ts() function!")
if (is.na(n.ahead) || !is.numeric(n.ahead) || n.ahead < 1 || !is.wholenumber(n.ahead)) 
stop("The 'n.ahead' must be a integer numbaer and greater than 0!")
#---kiem tra dau vao {0}---

#-------##-------##-------##-------##-------#
ts=object$Actual
p.max=1
n<-length(ts)
t<-c(start(ts)[1]):c(end(ts)[1])
t1<-c((t[n]+1):(t[n]+n.ahead))
dubao<-data.frame(point=t1)

#-------##-------##-------##-------##-------#

#---hoi qui xu the {1}---
mh.hoiqui<-lm(ts~t)

db.hoiqui<-predict(mh.hoiqui,newdata=data.frame(t=t1))

dubao<-data.frame(dubao,db.hoiqui)
#---hoi qui xu the {0}---
#------------------------------------------
#---hoi qui luy thua {1}---
namlog=log(t)
thuctelog=log(ts)
mh.luythua=lm(thuctelog~namlog)

namlog1=log(t1)
db.luythua<-predict(mh.luythua,newdata=data.frame(namlog=namlog1))
db.luythua=exp(db.luythua)

dubao<-data.frame(dubao,db.luythua)
#---hoi qui luy thua {0}---
#------------------------------------------
#---hoi qui luong giac {1}---
x1=sin(t)
x2=cos(t)
mh.luonggiac=lm(ts~x1+x2)

x1=sin(t1)
x2=cos(t1)
db.luonggiac<-predict(mh.luonggiac,newdata=data.frame(x1,x2))

dubao<-data.frame(dubao,db.luonggiac)
#---hoi qui luong giac {0}---
#------------------------------------------
#---cap hyperbol {1}---
nam1=1/t
mh.hyperbol=lm(ts~nam1)

nam1=1/t1
db.hyperbol<-predict(mh.hyperbol,newdata=data.frame(nam1))

dubao<-data.frame(dubao,db.hyperbol)
#---cap hyperbol {0}---
#------------------------------------------
#---cap so cong {1}---
mh.cscong=(log(ts[n])-log(ts[1]))/(t[n]-t[1])

db.cscong=ts[n]*(1+mh.cscong*(t1-t[n]))

dubao<-data.frame(dubao,db.cscong)
#---cap so cong {0}---
#------------------------------------------
#---cap so nhan {1}---
mh.csnhan=((ts[n]/ts[1])^(1/(t[n]-t[1])))-1

db.csnhan=ts[n]*(1+mh.csnhan)^(t1-t[n])

dubao<-data.frame(dubao,db.csnhan)
#---cap so nhan {0}---

#-------##-------##-------##-------##-------#



dubao
}
