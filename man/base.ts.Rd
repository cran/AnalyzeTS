\name{base.ts}
\alias{base.ts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Base Time Series
}
\description{
Ham tinh toan mot so chi tieu co ban ve day so thoi gian
}
\usage{
base.ts(ts, weight = NULL, type = c("mean.period", "mean.point1", 
"mean.point2", "abs.period", "abs.root", "abs.mean", "develop.period", 
"develop.root", "develop.mean", "growth.period", "growth.root", "growth.mean"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ts}{
Mot doi tuong chuoi thoi gian
}
  \item{weight}{
So ngay cho type = "mean.point2"
}
  \item{type}{
"mean.period": muc do trung binh cua day so thoi ky. "mean.point1": muc do trung binh cua day so thoi diem (khoang cach cac thoi diem la bang nhau). "mean.point2": muc do trung binh cua day so thoi diem (khoang cach cac thoi diem la khac nhau). "abs.period": luong tang giam tuyet doi tung ky. "abs.root": luong tang giam tuyet doi dinh goc. "abs.mean": luong tang giam tuyet doi trung binh. "develop.period": toc do phat trien tung ky. "develop.root": toc do phat trien dinh goc. "develop.mean": toc do phat trien trung binh. "growth.period": toc do tang giam tung ky. "growth.root": toc do tang giam dinh goc. "growth.mean": toc do tang giam trung binh.
}
}

\value{
Ket qua tinh toan cho tung chi tieu cu the.
}
\references{
Nguyen Hong Dan. Bai giang Thong ke du bao 2014
}
\author{
Hong Viet Minh <hongvietminh@gmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
sl<-c(931.1,722.2,749.4,976.2,892.4)
base.ts(sl,type="mean.period")

gia<-c(1.317,1.3165,1.310,1.3075,1.294,1.294)
base.ts(gia,type="mean.point1")

sld<-c(0,100,110,130,120,125)
songay<-c(31,9,5,14,14,17)
base.ts(sld,weight=songay,type="mean.point2")

loinhuan<-c(500,540,590,645,700)
base.ts(loinhuan,type="abs.period")
base.ts(loinhuan,type="abs.root")
base.ts(loinhuan,type="abs.mean")

100*base.ts(loinhuan,type="develop.period")
100*base.ts(loinhuan,type="develop.root")
100*base.ts(loinhuan,type="develop.mean")

100*base.ts(loinhuan,type="growth.period")
100*base.ts(loinhuan,type="growth.root")
100*base.ts(loinhuan,type="growth.mean")
}

\keyword{base.ts }