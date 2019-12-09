forecast.Greg.ts <-
function (object, model = "ALL", n.ahead = 5, plot = FALSE) 
{
    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x - 
        round(x)) < tol
    if (class(object)[1] != "list") 
        stop("The 'object' must be resul returning from Greg.ts() function!")
    if (prod(names(object) == c("Models", "Actual", "Interpolate", 
        "Error")) == 0) 
        stop("The 'object' must be resul returning from Greg.ts() function!")
    if (is.na(n.ahead) || !is.numeric(n.ahead) || n.ahead < 1 || 
        !is.wholenumber(n.ahead)) 
        stop("The 'n.ahead' must be a integer numbaer and greater than 0!")
    ts = object$Actual
    p.max = 1
    n <- length(ts)
    t <- c(start(ts)[1]):c(end(ts)[1])
    t1 <- c((t[n] + 1):(t[n] + n.ahead))
    dubao <- data.frame(point = t1)
    mh.hoiqui <- lm(ts ~ t)
    db.hoiqui <- predict(mh.hoiqui, newdata = data.frame(t = t1))
    dubao <- data.frame(dubao, db.hoiqui)
    namlog = log(t)
    thuctelog = log(ts)
    mh.luythua = lm(thuctelog ~ namlog)
    namlog1 = log(t1)
    db.luythua <- predict(mh.luythua, newdata = data.frame(namlog = namlog1))
    db.luythua = exp(db.luythua)
    dubao <- data.frame(dubao, db.luythua)
    x1 = sin(t)
    x2 = cos(t)
    mh.luonggiac = lm(ts ~ x1 + x2)
    x1 = sin(t1)
    x2 = cos(t1)
    db.luonggiac <- predict(mh.luonggiac, newdata = data.frame(x1, 
        x2))
    dubao <- data.frame(dubao, db.luonggiac)
    nam1 = 1/t
    mh.hyperbol = lm(ts ~ nam1)
    nam1 = 1/t1
    db.hyperbol <- predict(mh.hyperbol, newdata = data.frame(nam1))
    dubao <- data.frame(dubao, db.hyperbol)
    mh.cscong = (log(ts[n]) - log(ts[1]))/(t[n] - t[1])
    db.cscong = ts[n] * (1 + mh.cscong * (t1 - t[n]))
    dubao <- data.frame(dubao, db.cscong)
    mh.csnhan = ((ts[n]/ts[1])^(1/(t[n] - t[1]))) - 1
    db.csnhan = ts[n] * (1 + mh.csnhan)^(t1 - t[n])
    dubao <- data.frame(dubao, db.csnhan)
    dubao
}
