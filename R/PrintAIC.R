PrintAIC <-
function(DataTimeSeries,order=c(p,d=NULL,q=NULL),seas=list(order=c(P=NULL,D=NULL,Q=NULL),frequency=NULL),type=NULL,xreg=NULL){
    if(is.null(type))stop("Phai khai bao type cua mo hinh!")
    if(type!="ARMA" & type!="ARIMA" & type!="SARIMA" & type!="ARCH" & type!="GARCH" & type!="ARMAX" & type!="ARIMAX" & type!="SARIMAX")
      stop("Ham PrintAIC khong ho tro mo hinh nay!\nBan vui long lien he voi nguoi phat trien ham de viet
           code cho no, qua dia chi gmail <hongvietminh@gmail.com>")
    sona<-function(x){
      f<-x
      f[!is.na(f)]<-0
      f[is.na(f)]<-1
      s<-sum(f==1)
      s
    }
    
    
    is.wholenumber <-function(x, tol = .Machine$double.eps^0.5)  
      abs(x - round(x)) < tol
    
    #Kiem tra du lieu dau vao
    if (!is.numeric(DataTimeSeries)) stop("Chuoi khong hop le!")
    if(sona(DataTimeSeries)!=0) stop("Chuoi co gia tri NA!")
    DataTimeSeries <- as.ts(DataTimeSeries)
    
    if(type=="ARMA"){
      if(!is.null(seas$order)||!is.null(seas$frequency))stop("Mo hinh khong phu hop!")
      if (missing(order) ||is.null(order) || !is.numeric(order) || length(order) != 2L || any(order < 0)) stop("Loi o bac cua mo hinh!")
      
      #Ghi nhan cac he so
      p<-order[1]
      d<-0
      q<-order[2]
      
      #Kiem he so mot lan nua
      if (is.na(p)||!is.numeric(p) || p < 0 || !is.wholenumber(p))  stop("Loi o bac cua mo hinh!")
      if (is.na(q)||!is.numeric(q) || q < 0 || !is.wholenumber(q))  stop("Loi o bac cua mo hinh!")
      
      #To hop mo hinh, tinh aic
      anser<-gl(1,1,length=(p+1)*(q+1),labels=c("aic = ","aic ="))
      models<-list(p=1:((p+1)*(q+1)),d=1:((p+1)*(q+1)),q=1:((p+1)*(q+1)))
      aic<-1:((p+1)*(q+1))
      index=0
      for(i1 in 0:p){
        for(i2 in 0:q){
          index=index+1
          models$p[index]<-i1
          models$d[index]<-d
          models$q[index]<-i2
          aic[index]<-AIC(arima(DataTimeSeries,order=c(i1,d,i2)))
        }}
      
      #Xoa bo cac mo hinh 0 co y nghia
      xoa<-0
      for(i in 1:length(aic))
        if(models$p[i]==0 & models$q[i]==0) xoa<-cbind(xoa,i)
      models$p<-models$p[-xoa]
      models$d<-models$d[-xoa]
      models$q<-models$q[-xoa]
      aic<-aic[-xoa]
      
      #xep loai
      xl<-1:length(aic)
      aicxl<-sort(aic)
      for(k in 1:length(aic)){
        for(l in 1:length(aic)){
          if(aic[k]==aicxl[l]){
            if(l<10){xl[k]<-paste("..........",l); break;}
            else
              if(l<100){xl[k]<-paste(".........",l); break;}
          }}}
      
      #tim chi so mo hinh toi uu
      for(t in 1:length(aic)) if(aic[t]==min(aic)) break;
      id.best<-t
      
      #hop nhat mo hinh
      mohinh<-1:length(aic)
      giatri.AIC<-1:length(aic)
      for(i in 1:length(mohinh)){
        mohinh[i]<-paste("ARMA(", models$p[i],",", models$q[i], ")",sep="")
        giatri.AIC[i]<-paste("...AIC =",round(aic[i],2))
      }
      
      #xu ly ket qua xuat
      kq1<-data.frame(mohinh,giatri.AIC,xl)
      namescot1<-paste("Mo","hinh",sep=" ")
      namescot2<-paste("Gia","tri","AIC",sep=" ")
      namescot3<-paste("Xep","loai",sep=" ")
      colnames(kq1)<-c(namescot1,namescot2,namescot3)
      stt<-1:length(mohinh)
      for(j in 1:length(mohinh)) stt[j]<-paste("mo hinh",j)
      dimnames(kq1)[[1]]<-stt
      kq2<-data.frame("the.best.model"=paste(mohinh[id.best], giatri.AIC[id.best]))
      colnames(kq2)<-c(paste("Mo","hinh","toi","uu",sep=" "))
      dimnames(kq2)[[1]]<-" "
      print<-list(mohinh=kq1,best=kq2)
    }#finish ARMA
    
    if(type=="ARIMA"){
      if(!is.null(seas$order)||!is.null(seas$frequency))stop("Mo hinh khong phu hop!")
      if (missing(order) ||is.null(order) || !is.numeric(order) || length(order) != 3L || any(order < 0)) 
        stop("Loi o bac cua mo hinh!")
      
      #Ghi nhan cac he so
      p<-order[1]
      d<-order[2]
      q<-order[3]
      
      #Kiem he so mot lan nua
      if (is.na(p)||!is.numeric(p) || p < 0 || !is.wholenumber(p)) stop("Loi o bac cua mo hinh!")
      if (is.na(d)||!is.numeric(d) || d < 0 || !is.wholenumber(d)) stop("Loi o bac cua mo hinh!")
      if (is.na(q)||!is.numeric(q) || q < 0 || !is.wholenumber(q)) stop("Loi o bac cua mo hinh!")
      
      #To hop mo hinh, tinh aic
      anser<-gl(1,1,length=(p+1)*(q+1),labels=c("aic = ","aic ="))
      models<-list(p=1:((p+1)*(q+1)),d=1:((p+1)*(q+1)),q=1:((p+1)*(q+1)))
      aic<-1:((p+1)*(q+1))
      index=0
      for(i1 in 0:p){
        for(i2 in 0:q){
          index=index+1
          models$p[index]<-i1
          models$d[index]<-d
          models$q[index]<-i2
          aic[index]<-AIC(arima(DataTimeSeries,order=c(i1,d,i2)))
        }}
      
      #Xoa bo cac mo hinh 0 co y nghia
      xoa<-0
      for(i in 1:length(aic))
        if(models$p[i]==0 & models$q[i]==0) xoa<-cbind(xoa,i)
      models$p<-models$p[-xoa]
      models$d<-models$d[-xoa]
      models$q<-models$q[-xoa]
      aic<-aic[-xoa]
      
      #xep loai
      xl<-1:length(aic)
      aicxl<-sort(aic)
      for(k in 1:length(aic)){
        for(l in 1:length(aic)){
          if(aic[k]==aicxl[l]){
            if(l<10){xl[k]<-paste("..........",l); break;}
            else
              if(l<100){xl[k]<-paste(".........",l); break;}
          }}}
      
      #tim chi so mo hinh toi uu
      for(t in 1:length(aic)) if(aic[t]==min(aic)) break;
      id.best<-t
      
      #hop nhat mo hinh
      mohinh<-1:length(aic)
      giatri.AIC<-1:length(aic)
      for(i in 1:length(mohinh)){
        mohinh[i]<-paste("ARIMA(", models$p[i],",", models$d[i],",", models$q[i], ")",sep="")
        giatri.AIC[i]<-paste("...AIC =",round(aic[i],2))
      }
      
      #xu ly ket qua xuat
      kq1<-data.frame(mohinh,giatri.AIC,xl)
      namescot1<-paste("Mo","hinh",sep=" ")
      namescot2<-paste("Gia","tri","AIC",sep=" ")
      namescot3<-paste("Xep","loai",sep=" ")
      colnames(kq1)<-c(namescot1,namescot2,namescot3)
      stt<-1:length(mohinh)
      for(j in 1:length(mohinh)) stt[j]<-paste("mo hinh",j)
      dimnames(kq1)[[1]]<-stt
      kq2<-data.frame("the.best.model"=paste(mohinh[id.best], giatri.AIC[id.best]))
      colnames(kq2)<-c(paste("Mo","hinh","toi","uu",sep=" "))
      dimnames(kq2)[[1]]<-" "
      print<-list(mohinh=kq1,best=kq2)
    }#finish ARIMA
    
    if(type=="SARIMA"){
      if (missing(order) || is.null(order) || !is.numeric(order) || length(order) != 3L || any(order < 0)) 
        stop("'order' cau mo hinh SARIMA phai la mot vector so khong am va co chieu dai la 3!")
      if (is.null(seas) || !is.list(seas))stop("'seas' phai la mot danh sach gom hai thanh phan 'order' va 'frequency'!")
      if (is.null(seas$order) || !is.numeric(seas$order) || length(seas$order) != 3 || any(seas$order < 0)) 
        stop("'seas$order' phai la mot vector so khong am va co chieu dai la 3!")
      if (is.null(seas$frequency) || is.na(seas$frequency) || seas$frequency[1] < 0 ) 
        stop("'seas$frequency' phai la mot so nguyen khong am!")
      
      #Ghi nhan cac he so
      p<-order[1]
      d<-order[2]
      q<-order[3]
      P<-seas$order[1]
      D<-seas$order[2]
      Q<-seas$order[3]
      s<-seas$frequency[1]
      #Kiem he so mot lan nua
      if (is.na(p)||!is.numeric(p) || p < 0 || !is.wholenumber(p)) stop("p phai la nguyen duong")
      if (is.na(d)||!is.numeric(d) || d < 0 || !is.wholenumber(d)) stop("d phai la nguyen duong")
      if (is.na(q)||!is.numeric(q) || q < 0 || !is.wholenumber(q)) stop("p phai la nguyen duong")
      if (is.na(P)||!is.numeric(P) || P < 0 || !is.wholenumber(P)) stop("P phai la nguyen duong")
      if (is.na(D)||!is.numeric(D) || D < 0 || !is.wholenumber(D)) stop("D phai la nguyen duong")
      if (is.na(Q)||!is.numeric(Q) || Q < 0 || !is.wholenumber(Q)) stop("Q phai la nguyen duong")
      if (is.na(s)||!is.numeric(s) || s < 0 || !is.wholenumber(s)) stop("s phai la nguyen duong")
      #To hop mo hinh, tinh aic
      anser<-gl(1,1,length=(p+1)*(q+1)*(P+1)*(Q+1),labels=c("aic = ","aic ="))
      models<-list(p=1:((p+1)*(q+1)*(P+1)*(Q+1)),d=1:((p+1)*(q+1)*(P+1)*(Q+1)),
                   q=1:((p+1)*(q+1)*(P+1)*(Q+1)),P=1:((p+1)*(q+1)*(P+1)*(Q+1)),
                   D=1:((p+1)*(q+1)*(P+1)*(Q+1)),Q=1:((p+1)*(q+1)*(P+1)*(Q+1)),
                   s=1:((p+1)*(q+1)*(P+1)*(Q+1)))
      aic<-1:((p+1)*(q+1)*(P+1)*(Q+1))
      index=0
      for(i1 in 0:p){
        for(i2 in 0:q){
          for(j1 in 0:P){
            for(j2 in 0:Q){
              index=index+1
              models$p[index]<-i1
              models$d[index]<-d
              models$q[index]<-i2
              models$P[index]<-j1
              models$D[index]<-D
              models$Q[index]<-j2
              models$s[index]<-s
              aic[index]<-AIC(arima(DataTimeSeries,order=c(i1,d,i2),seasonal=list(order=c(j1,D,j2),s)))
            }}}}
      
      #Xoa bo cac mo hinh 0 co y nghia
      xoa<-0
      for(i in 1:length(aic)) if(models$p[i]==0 & models$q[i]==0) xoa<-cbind(xoa,i)
      for(i in 1:length(aic)) if(models$P[i]==0 & models$Q[i]==0) xoa<-cbind(xoa,i)
      xoa<-unique(as.numeric(xoa[-1]))
      models$p<-models$p[-xoa]
      models$d<-models$d[-xoa]
      models$q<-models$q[-xoa]
      models$P<-models$P[-xoa]
      models$D<-models$D[-xoa]
      models$Q<-models$Q[-xoa]
      models$s<-models$s[-xoa]
      aic<-aic[-xoa]
      
      #xep loai
      xl<-1:length(aic)
      aicxl<-sort(aic)
      #do tim va cap so
      for(k in 1:length(aic)){
        for(l in 1:length(aic)){
          if(aic[k]==aicxl[l]){
            if(l<10){xl[k]<-paste("..........",l); break;}
            else
              if(l<100){xl[k]<-paste(".........",l); break;}
          }}}
      
      #tim chi so mo hinh toi uu
      for(t in 1:length(aic)) if(aic[t]==min(aic)) break;
      id.best<-t
      
      #hop nhat mo hinh
      mohinh<-1:length(aic)
      giatri.AIC<-1:length(aic)
      for(i in 1:length(mohinh)){
        mohinh[i]<-paste("SARIMA(",models$p[i],",",models$d[i],",",models$q[i],
                         ")*(",models$P[i],",",models$D[i],",",models$Q[i],
                         "),s=", models$s[i],sep="")
        giatri.AIC[i]<-paste("...AIC =",round(aic[i],2))}
      
      #xu ly ket qua in ra
      kq1<-data.frame(mohinh,giatri.AIC,xl)
      namescot1<-paste("Mo","hinh",sep=" ")
      namescot2<-paste("Gia","tri","AIC",sep=" ")
      namescot3<-paste("Xep","loai",sep=" ")
      colnames(kq1)<-c(namescot1,namescot2,namescot3)
      stt<-1:length(mohinh)
      for(j in 1:length(mohinh)) stt[j]<-paste("mo hinh",j)
      dimnames(kq1)[[1]]<-stt
      kq2<-data.frame("the.best.model"=paste(mohinh[id.best], giatri.AIC[id.best]))
      colnames(kq2)<-c(paste("Mo","hinh","toi","uu",sep=" "))
      dimnames(kq2)[[1]]<-" "
      print<-list(mohinh=kq1,best=kq2)
    }#finish SARIMA
    
    if(type=="ARMAX"){

      if(is.null(xreg))stop("Phai khai bao xreg cho mo hinh!")
      if(is.vector(xreg)) if(length(xreg)!=length(DataTimeSeries))stop("Chieu dai chuoi quan sat va xreg khac nhau!")
      if(is.data.frame(xreg)) if(dim(xreg)[1]!=length(DataTimeSeries))stop("Chieu dai chuoi quan sat va xreg khac nhau!")
      
      if(!is.null(seas$order)||!is.null(seas$frequency))stop("Mo hinh khong phu hop!")
      if (missing(order) ||is.null(order) || !is.numeric(order) || length(order) != 2L || any(order < 0)) stop("Loi o bac cua mo hinh!")
      
      #Ghi nhan cac he so
      p<-order[1]
      d<-0
      q<-order[2]
      
      #Kiem he so mot lan nua
      if (is.na(p)||!is.numeric(p) || p < 0 || !is.wholenumber(p))  stop("Loi o bac cua mo hinh!")
      if (is.na(q)||!is.numeric(q) || q < 0 || !is.wholenumber(q))  stop("Loi o bac cua mo hinh!")
      
      #To hop mo hinh, tinh aic
      anser<-gl(1,1,length=(p+1)*(q+1),labels=c("aic = ","aic ="))
      models<-list(p=1:((p+1)*(q+1)),d=1:((p+1)*(q+1)),q=1:((p+1)*(q+1)))
      aic<-1:((p+1)*(q+1))
      index=0
      for(i1 in 0:p){
        for(i2 in 0:q){
          index=index+1
          models$p[index]<-i1
          models$d[index]<-d
          models$q[index]<-i2
          aic[index]<-AIC(arimax(DataTimeSeries,order=c(i1,d,i2),xreg=xreg))
        }}
      
      #Xoa bo cac mo hinh 0 co y nghia
      xoa<-0
      for(i in 1:length(aic))
        if(models$p[i]==0 & models$q[i]==0) xoa<-cbind(xoa,i)
      models$p<-models$p[-xoa]
      models$d<-models$d[-xoa]
      models$q<-models$q[-xoa]
      aic<-aic[-xoa]
      
      #xep loai
      xl<-1:length(aic)
      aicxl<-sort(aic)
      for(k in 1:length(aic)){
        for(l in 1:length(aic)){
          if(aic[k]==aicxl[l]){
            if(l<10){xl[k]<-paste("..........",l); break;}
            else
              if(l<100){xl[k]<-paste(".........",l); break;}
          }}}
      
      #tim chi so mo hinh toi uu
      for(t in 1:length(aic)) if(aic[t]==min(aic)) break;
      id.best<-t
      
      #hop nhat mo hinh
      mohinh<-1:length(aic)
      giatri.AIC<-1:length(aic)
      for(i in 1:length(mohinh)){
        mohinh[i]<-paste("ARMAX(", models$p[i],",", models$q[i], ")",sep="")
        giatri.AIC[i]<-paste("...AIC =",round(aic[i],2))
      }
      
      #xu ly ket qua xuat
      kq1<-data.frame(mohinh,giatri.AIC,xl)
      namescot1<-paste("Mo","hinh",sep=" ")
      namescot2<-paste("Gia","tri","AIC",sep=" ")
      namescot3<-paste("Xep","loai",sep=" ")
      colnames(kq1)<-c(namescot1,namescot2,namescot3)
      stt<-1:length(mohinh)
      for(j in 1:length(mohinh)) stt[j]<-paste("mo hinh",j)
      dimnames(kq1)[[1]]<-stt
      kq2<-data.frame("the.best.model"=paste(mohinh[id.best], giatri.AIC[id.best]))
      colnames(kq2)<-c(paste("Mo","hinh","toi","uu",sep=" "))
      dimnames(kq2)[[1]]<-" "
      print<-list(mohinh=kq1,best=kq2)
    }#finish ARMAX
    
    if(type=="ARIMAX"){
      
      if(is.null(xreg))stop("Phai khai bao xreg cho mo hinh!")
      if(is.vector(xreg)) if(length(xreg)!=length(DataTimeSeries))stop("Chieu dai chuoi quan sat va xreg khac nhau!")
      if(is.data.frame(xreg)) if(dim(xreg)[1]!=length(DataTimeSeries))stop("Chieu dai chuoi quan sat va xreg khac nhau!")
      
      if(!is.null(seas$order)||!is.null(seas$frequency))stop("Mo hinh khong phu hop!")
      if (missing(order) ||is.null(order) || !is.numeric(order) || length(order) != 3L || any(order < 0)) 
        stop("Loi o bac cua mo hinh!")
      
      #Ghi nhan cac he so
      p<-order[1]
      d<-order[2]
      q<-order[3]
      
      #Kiem he so mot lan nua
      if (is.na(p)||!is.numeric(p) || p < 0 || !is.wholenumber(p)) stop("Loi o bac cua mo hinh!")
      if (is.na(d)||!is.numeric(d) || d < 0 || !is.wholenumber(d)) stop("Loi o bac cua mo hinh!")
      if (is.na(q)||!is.numeric(q) || q < 0 || !is.wholenumber(q)) stop("Loi o bac cua mo hinh!")
      
      #To hop mo hinh, tinh aic
      anser<-gl(1,1,length=(p+1)*(q+1),labels=c("aic = ","aic ="))
      models<-list(p=1:((p+1)*(q+1)),d=1:((p+1)*(q+1)),q=1:((p+1)*(q+1)))
      aic<-1:((p+1)*(q+1))
      index=0
      for(i1 in 0:p){
        for(i2 in 0:q){
          index=index+1
          models$p[index]<-i1
          models$d[index]<-d
          models$q[index]<-i2
          aic[index]<-AIC(arimax(DataTimeSeries,order=c(i1,d,i2),xreg=xreg))
        }}
      
      #Xoa bo cac mo hinh 0 co y nghia
      xoa<-0
      for(i in 1:length(aic))
        if(models$p[i]==0 & models$q[i]==0) xoa<-cbind(xoa,i)
      models$p<-models$p[-xoa]
      models$d<-models$d[-xoa]
      models$q<-models$q[-xoa]
      aic<-aic[-xoa]
      
      #xep loai
      xl<-1:length(aic)
      aicxl<-sort(aic)
      for(k in 1:length(aic)){
        for(l in 1:length(aic)){
          if(aic[k]==aicxl[l]){
            if(l<10){xl[k]<-paste("..........",l); break;}
            else
              if(l<100){xl[k]<-paste(".........",l); break;}
          }}}
      
      #tim chi so mo hinh toi uu
      for(t in 1:length(aic)) if(aic[t]==min(aic)) break;
      id.best<-t
      
      #hop nhat mo hinh
      mohinh<-1:length(aic)
      giatri.AIC<-1:length(aic)
      for(i in 1:length(mohinh)){
        mohinh[i]<-paste("ARIMAX(", models$p[i],",", models$d[i],",", models$q[i], ")",sep="")
        giatri.AIC[i]<-paste("...AIC =",round(aic[i],2))
      }
      
      #xu ly ket qua xuat
      kq1<-data.frame(mohinh,giatri.AIC,xl)
      namescot1<-paste("Mo","hinh",sep=" ")
      namescot2<-paste("Gia","tri","AIC",sep=" ")
      namescot3<-paste("Xep","loai",sep=" ")
      colnames(kq1)<-c(namescot1,namescot2,namescot3)
      stt<-1:length(mohinh)
      for(j in 1:length(mohinh)) stt[j]<-paste("mo hinh",j)
      dimnames(kq1)[[1]]<-stt
      kq2<-data.frame("the.best.model"=paste(mohinh[id.best], giatri.AIC[id.best]))
      colnames(kq2)<-c(paste("Mo","hinh","toi","uu",sep=" "))
      dimnames(kq2)[[1]]<-" "
      print<-list(mohinh=kq1,best=kq2)
    }#finish ARIMAX
    
    if(type=="SARIMAX"){
      
      if(is.null(xreg))stop("Phai khai bao xreg cho mo hinh!")
      if(is.vector(xreg)) if(length(xreg)!=length(DataTimeSeries))stop("Chieu dai chuoi quan sat va xreg khac nhau!")
      if(is.data.frame(xreg)) if(dim(xreg)[1]!=length(DataTimeSeries))stop("Chieu dai chuoi quan sat va xreg khac nhau!")
      
      
      if (missing(order) || is.null(order) || !is.numeric(order) || length(order) != 3L || any(order < 0)) 
        stop("'order' cau mo hinh SARIMA phai la mot vector so khong am va co chieu dai la 3!")
      if (is.null(seas) || !is.list(seas))stop("'seas' phai la mot danh sach gom hai thanh phan 'order' va 'frequency'!")
      if (is.null(seas$order) || !is.numeric(seas$order) || length(seas$order) != 3 || any(seas$order < 0)) 
        stop("'seas$order' phai la mot vector so khong am va co chieu dai la 3!")
      if (is.null(seas$frequency) || is.na(seas$frequency) || seas$frequency[1] < 0 ) 
        stop("'seas$frequency' phai la mot so nguyen khong am!")
      
      #Ghi nhan cac he so
      p<-order[1]
      d<-order[2]
      q<-order[3]
      P<-seas$order[1]
      D<-seas$order[2]
      Q<-seas$order[3]
      s<-seas$frequency[1]
      #Kiem he so mot lan nua
      if (is.na(p)||!is.numeric(p) || p < 0 || !is.wholenumber(p)) stop("p phai la nguyen duong")
      if (is.na(d)||!is.numeric(d) || d < 0 || !is.wholenumber(d)) stop("d phai la nguyen duong")
      if (is.na(q)||!is.numeric(q) || q < 0 || !is.wholenumber(q)) stop("p phai la nguyen duong")
      if (is.na(P)||!is.numeric(P) || P < 0 || !is.wholenumber(P)) stop("P phai la nguyen duong")
      if (is.na(D)||!is.numeric(D) || D < 0 || !is.wholenumber(D)) stop("D phai la nguyen duong")
      if (is.na(Q)||!is.numeric(Q) || Q < 0 || !is.wholenumber(Q)) stop("Q phai la nguyen duong")
      if (is.na(s)||!is.numeric(s) || s < 0 || !is.wholenumber(s)) stop("s phai la nguyen duong")
      #To hop mo hinh, tinh aic
      anser<-gl(1,1,length=(p+1)*(q+1)*(P+1)*(Q+1),labels=c("aic = ","aic ="))
      models<-list(p=1:((p+1)*(q+1)*(P+1)*(Q+1)),d=1:((p+1)*(q+1)*(P+1)*(Q+1)),
                   q=1:((p+1)*(q+1)*(P+1)*(Q+1)),P=1:((p+1)*(q+1)*(P+1)*(Q+1)),
                   D=1:((p+1)*(q+1)*(P+1)*(Q+1)),Q=1:((p+1)*(q+1)*(P+1)*(Q+1)),
                   s=1:((p+1)*(q+1)*(P+1)*(Q+1)))
      aic<-1:((p+1)*(q+1)*(P+1)*(Q+1))
      index=0
      for(i1 in 0:p){
        for(i2 in 0:q){
          for(j1 in 0:P){
            for(j2 in 0:Q){
              index=index+1
              models$p[index]<-i1
              models$d[index]<-d
              models$q[index]<-i2
              models$P[index]<-j1
              models$D[index]<-D
              models$Q[index]<-j2
              models$s[index]<-s
              aic[index]<-AIC(arimax(DataTimeSeries,order=c(i1,d,i2),seasonal=list(order=c(j1,D,j2),s),xreg=xreg))
            }}}}
      
      #Xoa bo cac mo hinh 0 co y nghia
      xoa<-0
      for(i in 1:length(aic)) if(models$p[i]==0 & models$q[i]==0) xoa<-cbind(xoa,i)
      for(i in 1:length(aic)) if(models$P[i]==0 & models$Q[i]==0) xoa<-cbind(xoa,i)
      xoa<-unique(as.numeric(xoa[-1]))
      models$p<-models$p[-xoa]
      models$d<-models$d[-xoa]
      models$q<-models$q[-xoa]
      models$P<-models$P[-xoa]
      models$D<-models$D[-xoa]
      models$Q<-models$Q[-xoa]
      models$s<-models$s[-xoa]
      aic<-aic[-xoa]
      
      #xep loai
      xl<-1:length(aic)
      aicxl<-sort(aic)
      #do tim va cap so
      for(k in 1:length(aic)){
        for(l in 1:length(aic)){
          if(aic[k]==aicxl[l]){
            if(l<10){xl[k]<-paste("..........",l); break;}
            else
              if(l<100){xl[k]<-paste(".........",l); break;}
          }}}
      
      #tim chi so mo hinh toi uu
      for(t in 1:length(aic)) if(aic[t]==min(aic)) break;
      id.best<-t
      
      #hop nhat mo hinh
      mohinh<-1:length(aic)
      giatri.AIC<-1:length(aic)
      for(i in 1:length(mohinh)){
        mohinh[i]<-paste("SARIMAX(",models$p[i],",",models$d[i],",",models$q[i],
                         ")*(",models$P[i],",",models$D[i],",",models$Q[i],
                         "),s=", models$s[i],sep="")
        giatri.AIC[i]<-paste("...AIC =",round(aic[i],2))}
      
      #xu ly ket qua in ra
      kq1<-data.frame(mohinh,giatri.AIC,xl)
      namescot1<-paste("Mo","hinh",sep=" ")
      namescot2<-paste("Gia","tri","AIC",sep=" ")
      namescot3<-paste("Xep","loai",sep=" ")
      colnames(kq1)<-c(namescot1,namescot2,namescot3)
      stt<-1:length(mohinh)
      for(j in 1:length(mohinh)) stt[j]<-paste("mo hinh",j)
      dimnames(kq1)[[1]]<-stt
      kq2<-data.frame("the.best.model"=paste(mohinh[id.best], giatri.AIC[id.best]))
      colnames(kq2)<-c(paste("Mo","hinh","toi","uu",sep=" "))
      dimnames(kq2)[[1]]<-" "
      print<-list(mohinh=kq1,best=kq2)
    }#finish SARIMAX
    
    if(type=="ARCH"){
      
      if(!is.null(seas$order)||!is.null(seas$frequency))stop("Mo hinh khong phu hop!")
      if (missing(order) || is.null(order) ||!is.numeric(order) || length(order) != 1L || any(order < 0)) 
        stop("'order' cua mo hinh ARCH phai la mot so nguyen duong")
      
      #Ghi nhan cac he so
      p<-0
      q<-order[1]
      if (is.na(q)||!is.numeric(q) || q < 0 || !is.wholenumber(q)) stop("q phai la nguyen duong")
      
      #To hop mo hinh, tinh aic
      anser<-gl(1,1,length=(p+1)*(q+1),labels=c("aic = ","aic ="))
      models<-list(p=1:((p+1)*(q+1)),q=1:((p+1)*(q+1)))
      aic<-1:((p+1)*(q+1))
      index=0
      for(i1 in 0:p){
        for(i2 in 0:q){
          index=index+1
          models$p[index]<-i1
          models$q[index]<-i2
          if(index!=1)aic[index]<-AIC(garch(DataTimeSeries,order=c(i1,i2),trace=FALSE))
        }}
      aic<-aic[-1]
      models$p<-models$p[-1]
      models$q<-models$q[-1]
      
      #xep loai
      xl<-1:length(aic)
      aicxl<-sort(aic)
      for(k in 1:length(aic)){
        for(l in 1:length(aic)){
          if(aic[k]==aicxl[l]){
            if(l<10){xl[k]<-paste("..........",l); break;}
            else
              if(l<100){xl[k]<-paste(".........",l); break;}
          }}}
      
      #tim chi so mo hinh toi uu
      for(t in 1:length(aic)) if(aic[t]==min(aic)) break;
      id.best<-t
      
      #Suat ket qua
      #hop nhat mo hinh
      mohinh<-1:length(aic)
      giatri.AIC<-1:length(aic)
      
      for(i in 1:length(aic)){
        mohinh[i]<-paste("ARCH(",models$q[i], ")",sep="")
        giatri.AIC[i]<-paste("...AIC =",round(aic[i],2))
      }
      
      #xu ly ket qua xuat
      kq1<-data.frame("Cac mo hinh ARCH.." = mohinh,"...Gia tri AIC.." = giatri.AIC,"....xep loai"=xl)
      namescot1<-paste("Mo","hinh",sep=" ")
      namescot2<-paste("Gia","tri","AIC",sep=" ")
      namescot3<-paste("Xep","loai",sep=" ")
      colnames(kq1)<-c(namescot1,namescot2,namescot3)
      stt<-1:length(mohinh)
      for(j in 1:length(mohinh)) stt[j]<-paste("mo hinh",j)
      dimnames(kq1)[[1]]<-stt
      kq2<-data.frame("the.best.model"=paste(mohinh[id.best], giatri.AIC[id.best]))
      colnames(kq2)<-c(paste("Mo","hinh","toi","uu",sep=" "))
      dimnames(kq2)[[1]]<-" "
      print<-list(mohinh=kq1,best=kq2)
    }#finish ARCH
    
    if(type=="GARCH"){
      
      if(!is.null(seas$order)||!is.null(seas$frequency))stop("Mo hinh khong phu hop!")
      if (missing(order) || is.null(order) ||!is.numeric(order) || length(order) != 2L || any(order < 0)) 
        stop("'order' cua mo hinh GARCH phai la mot vector gom hai so nguyen duong")
      
      #Ghi nhan cac he so
      p<-order[1]
      q<-order[2]
      if (is.na(p)||!is.numeric(p) || p < 0 || !is.wholenumber(p)) stop("p phai la nguyen duong")
      if (is.na(q)||!is.numeric(q) || q < 0 || !is.wholenumber(q)) stop("q phai la nguyen duong")
      
      #To hop mo hinh, tinh aic
      anser<-gl(1,1,length=(p+1)*(q+1),labels=c("aic = ","aic ="))
      models<-list(p=1:((p+1)*(q+1)),q=1:((p+1)*(q+1)))
      aic<-1:((p+1)*(q+1))
      index=0
      for(i1 in 0:p){
        for(i2 in 0:q){
          index=index+1
          models$p[index]<-i1
          models$q[index]<-i2
          if(i1!=0)aic[index]<-AIC(garch(DataTimeSeries,order=c(i1,i2),trace=FALSE))
        }}
      aic<-aic[-c(1:(q+1))]
      models$p<-models$p[-c(1:(q+1))]
      models$q<-models$q[-c(1:(q+1))]
      
      #xep loai
      xl<-1:length(aic)
      aicxl<-sort(aic)
      for(k in 1:length(aic)){
        for(l in 1:length(aic)){
          if(aic[k]==aicxl[l]){
            if(l<10){xl[k]<-paste("..........",l); break;}
            else
              if(l<100){xl[k]<-paste(".........",l); break;}
          }}}
      
      #tim chi so mo hinh toi uu
      for(t in 1:length(aic)) if(aic[t]==min(aic)) break;
      id.best<-t
      
      #Suat ket qua
      #hop nhat mo hinh
      mohinh<-1:length(aic)
      giatri.AIC<-1:length(aic)
      
      for(i in 1:length(aic)){
        mohinh[i]<-paste("GARCH(",models$p[i],",",models$q[i], ")",sep="")
        giatri.AIC[i]<-paste("...AIC =",round(aic[i],2))
      }
      
      #xu ly ket qua xuat
      kq1<-data.frame("Cac mo hinh GARCH.." = mohinh,"...Gia tri AIC.." = giatri.AIC,"....xep loai"=xl)
      namescot1<-paste("Mo","hinh",sep=" ")
      namescot2<-paste("Gia","tri","AIC",sep=" ")
      namescot3<-paste("Xep","loai",sep=" ")
      colnames(kq1)<-c(namescot1,namescot2,namescot3)
      stt<-1:length(mohinh)
      for(j in 1:length(mohinh)) stt[j]<-paste("mo hinh",j)
      dimnames(kq1)[[1]]<-stt
      kq2<-data.frame("the.best.model"=paste(mohinh[id.best], giatri.AIC[id.best]))
      colnames(kq2)<-c(paste("Mo","hinh","toi","uu",sep=" "))
      dimnames(kq2)[[1]]<-" "
      print<-list(mohinh=kq1,best=kq2)
      
    }#finsh GARCH
    
    print
  }
