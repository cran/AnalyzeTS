Greg.ts <-
function (ts, p.max = 3, r = 4, plot = FALSE) 
{
    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x - 
        round(x)) < tol
    if (!is.numeric(ts)) 
        stop("Error in 'ts'!")
    if (!is.ts(ts)) 
        stop("Error in 'ts'!")
    else if (!is.null(dim(ts))) 
        stop("Error in 'ts'!")
    kt <- 0
    for (i in 1:length(ts)) if (is.na(ts[i])) 
        kt = kt + 1
    if (kt > 0) 
        stop("'ts' contain NA!")
    if (frequency(ts) != 1) 
        stop("The 'ts' must be a time series with frequency == 1!")
    if (is.na(p.max) || !is.numeric(p.max) || p.max < 2 || !is.wholenumber(p.max)) 
        stop("The 'p.max' must be a integer numbaer and greater than 1!")
    n <- length(ts)
    t <- c(start(ts)[1]):c(end(ts)[1])
    L.mh <- vector("list", 6 + p.max - 1)
    interpolate <- data.frame(ts)
    error <- data.frame(ts)
    p.max = 1
    mh.hoiqui <- lm(ts ~ t)
    L.mh[[1]] <- mh.hoiqui
    names.Lmh <- c("xu the")
    ns.hoiqui = fitted(mh.hoiqui)
    ss.hoiqui = ts - ns.hoiqui
    interpolate <- data.frame(interpolate, ns.hoiqui)
    error <- data.frame(error, ss.hoiqui)
    hs.hoiqui <- round(coef(mh.hoiqui), 4)
    if (hs.hoiqui[2] >= 0) 
        text.hoiqui <- paste("Xu the: Y =", hs.hoiqui[1], "+", 
            paste(hs.hoiqui[2], "*t", sep = ""))
    if (hs.hoiqui[2] < 0) 
        text.hoiqui <- paste("Xu the: Y =", hs.hoiqui[1], paste(hs.hoiqui[2], 
            "*t", sep = ""))
    hs.mohinh <- c(text.hoiqui)
    namlog = log(t)
    thuctelog = log(ts)
    mh.luythua = lm(thuctelog ~ namlog)
    L.mh[[p.max + 1]] <- mh.luythua
    names.Lmh <- c(names.Lmh, "luy thua")
    ns.luythua = exp(fitted(mh.luythua))
    ss.luythua = ts - ns.luythua
    interpolate <- data.frame(interpolate, ns.luythua)
    error <- data.frame(error, ss.luythua)
    hs.luythua <- round(coef(mh.luythua), 4)
    text.luythua <- paste("Luy thua: Y =", paste("exp(", hs.luythua[1], 
        ")*t^", hs.luythua[2], sep = ""))
    hs.mohinh <- c(hs.mohinh, text.luythua)
    x1 = sin(t)
    x2 = cos(t)
    mh.luonggiac = lm(ts ~ x1 + x2)
    L.mh[[p.max + 2]] <- mh.luonggiac
    names.Lmh <- c(names.Lmh, "luong giac")
    ns.luonggiac = fitted(mh.luonggiac)
    ss.luonggiac = ts - ns.luonggiac
    interpolate <- data.frame(interpolate, ns.luonggiac)
    error <- data.frame(error, ss.luonggiac)
    hs.luonggiac <- round(coef(mh.luonggiac), 4)
    if (hs.luonggiac[2] >= 0) 
        text.luonggiac <- paste("Luong giac: Y =", hs.luonggiac[1], 
            "+", paste(hs.luonggiac[2], "*sin(t)", sep = ""))
    if (hs.luonggiac[2] < 0) 
        text.luonggiac <- paste("Luong giac: Y =", hs.luonggiac[1], 
            paste(hs.luonggiac[2], "*sin(t)", sep = ""))
    if (hs.luonggiac[3] >= 0) 
        text.luonggiac <- paste(text.luonggiac, "+", paste(hs.luonggiac[3], 
            "*cos(t)", sep = ""))
    if (hs.luonggiac[3] < 0) 
        text.luonggiac <- paste(text.luonggiac, paste(hs.luonggiac[3], 
            "*cos(t)", sep = ""))
    hs.mohinh <- c(hs.mohinh, text.luonggiac)
    nam1 = 1/t
    mh.hyperbol = lm(ts ~ nam1)
    L.mh[[p.max + 3]] <- mh.hyperbol
    names.Lmh <- c(names.Lmh, "hyperbol")
    ns.hyperbol = fitted(mh.hyperbol)
    ss.hyperbol = ts - ns.hyperbol
    interpolate <- data.frame(interpolate, ns.hyperbol)
    error <- data.frame(error, ss.hyperbol)
    hs.hyperbol <- round(coef(mh.hyperbol), 4)
    if (hs.hyperbol[2] >= 0) 
        text.hyperbol <- paste("Hyperbol: Y =", hs.hyperbol[1], 
            "+", paste(hs.hyperbol[2], "/t", sep = ""))
    if (hs.hyperbol[2] < 0) 
        text.hyperbol <- paste("Hyperbol: Y =", hs.hyperbol[1], 
            paste(hs.hyperbol[2], "/t", sep = ""))
    hs.mohinh <- c(hs.mohinh, text.hyperbol)
    mh.cscong = (log(ts[n]) - log(ts[1]))/(t[n] - t[1])
    L.mh[[p.max + 4]] <- mh.cscong
    names.Lmh <- c(names.Lmh, "cs cong")
    ns.cscong = ts[n] * (1 + mh.cscong * (t - t[n]))
    ss.cscong = ts - ns.cscong
    interpolate <- data.frame(interpolate, ns.cscong)
    error <- data.frame(error, ss.cscong)
    if (mh.cscong >= 0) 
        text.cscong <- paste("CS cong: Y = ", ts[n], "*[1 + ", 
            round(mh.cscong, 4), "*(t -", t[n], ")]", sep = "")
    if (mh.cscong < 0) 
        text.cscong <- paste("CS cong: Y = ", ts[n], "*[1 ", 
            round(mh.cscong, 4), "*(t -", t[n], ")]", sep = "")
    hs.mohinh <- c(hs.mohinh, text.cscong)
    mh.csnhan = ((ts[n]/ts[1])^(1/(t[n] - t[1]))) - 1
    L.mh[[p.max + 5]] <- mh.csnhan
    names.Lmh <- c(names.Lmh, "cs nhan")
    ns.csnhan = ts[n] * (1 + mh.csnhan)^(t - t[n])
    ss.csnhan = ts - ns.csnhan
    interpolate <- data.frame(interpolate, ns.csnhan)
    error <- data.frame(error, ss.csnhan)
    if (mh.csnhan >= 0) 
        text.csnhan <- paste("CS nhan: Y = ", ts[n], "*(1 + ", 
            round(mh.csnhan, 4), ")^(t-", t[n], ")", sep = "")
    if (mh.cscong < 0) 
        text.csnhan <- paste("CS nhan: Y = ", ts[n], "*(1 ", 
            round(mh.csnhan, 4), ")^(t -", t[n], ")", sep = "")
    hs.mohinh <- c(hs.mohinh, text.csnhan)
    names(L.mh) <- names.Lmh
    KQ <- vector("list", 4)
    names(KQ) <- c("Models", "Actual", "Interpolate", "Error")
    KQ[[1]] <- hs.mohinh
    KQ[[2]] <- ts
    KQ[[3]] <- interpolate[, -1]
    KQ[[4]] <- error[, -1]
    KQ
}
