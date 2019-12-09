Gfuzzy.ts1 <-
function (ts, n = 5, D1 = 0, D2 = 0, type = "Chen", bin = NULL, 
    plot = FALSE, grid = FALSE) 
{
    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x - 
        round(x)) < tol
    if (!is.numeric(ts)) 
        stop("Error in 'ts'!")
    rule1 <- function(x2, x1, A3, D) {
        tapmo <- as.character(D[, 1])
        low <- D[, 2]
        up <- D[, 3]
        bw <- D[, 4]
        for (i in 1:length(tapmo)) if (A3 == tapmo[i]) {
            vt = i
            break
        }
        s <- (up[vt] - low[vt])/4
        different <- abs(x2 - x1)/2
        distant = abs(up[vt] - low[vt])/2
        if (different > distant) 
            x = low[vt] + 3 * s
        else if (different == distant) 
            x = low[vt] + 2 * s
        else if (different < distant) 
            x = low[vt] + 1 * s
        x
    }
    rule2 <- function(x3, x2, x1, A4, D) {
        tapmo <- as.character(D[, 1])
        low <- D[, 2]
        up <- D[, 3]
        bw <- D[, 4]
        for (i in 1:length(tapmo)) if (A4 == tapmo[i]) {
            vt = i
            break
        }
        s <- (up[vt] - low[vt])/4
        different1a <- abs((x3 - x2) - (x2 - x1)) * 2 + x3
        different1b <- x3 - abs((x3 - x2) - (x2 - x1)) * 2
        different2a <- abs((x3 - x2) - (x2 - x1))/2 + x3
        different2b <- x3 - abs((x3 - x2) - (x2 - x1))/2
        if (low[vt] <= different1a & different1a <= up[vt]) 
            x = low[vt] + 3 * s
        else if (low[vt] <= different1b & different1b <= up[vt]) 
            x = low[vt] + 3 * s
        else if (low[vt] <= different2a & different2a <= up[vt]) 
            x = low[vt] + 1 * s
        else if (low[vt] <= different2b & different2b <= up[vt]) 
            x = low[vt] + 1 * s
        else x = low[vt] + 2 * s
        x
    }
    rule3 <- function(x3, x2, x1, A4, D) {
        tapmo <- as.character(D[, 1])
        low <- D[, 2]
        up <- D[, 3]
        bw <- D[, 4]
        for (i in 1:length(tapmo)) if (A4 == tapmo[i]) {
            vt = i
            break
        }
        s <- (up[vt] - low[vt])/4
        different1a <- abs((x3 - x2) - (x2 - x1)) * 2 + x3
        different1b <- x3 - abs((x3 - x2) - (x2 - x1)) * 2
        different2a <- abs((x3 - x2) - (x2 - x1))/2 + x3
        different2b <- x3 - abs((x3 - x2) - (x2 - x1))/2
        if (low[vt] <= different2a & different2a <= up[vt]) 
            x = low[vt] + 1 * s
        else if (low[vt] <= different2b & different2b <= up[vt]) 
            x = low[vt] + 1 * s
        else if (low[vt] <= different1a & different1a <= up[vt]) 
            x = low[vt] + 3 * s
        else if (low[vt] <= different1b & different1b <= up[vt]) 
            x = low[vt] + 3 * s
        else x = low[vt] + 2 * s
        x
    }
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
    if (!is.ts(ts)) 
        stop("Error in 'ts'!")
    else if (!is.null(dim(ts))) 
        stop("Error in 'ts'!")
    kt <- 0
    for (i in 1:length(ts)) if (is.na(ts[i])) 
        kt = kt + 1
    if (kt > 0) 
        stop("Trong chuoi co gia tri NA!")
    if (is.na(D1) | !is.numeric(D1) | length(D1) > 1) 
        stop("Error in 'D1'!")
    if (is.na(D2) | !is.numeric(D2) | length(D2) > 1) 
        stop("Error in 'D2'!")
    if (sum(is.na(n)) > 0 || sum(!is.numeric(n)) > 0 || sum(n < 
        1) > 0 || sum(!is.wholenumber(n)) > 0 || !is.vector(n)) 
        stop("Error in 'n'!")
    if (sum(type != "Chen" & type != "Singh" & type != "Heuristic" & 
        type != "Chen-Hsu") > 0) 
        stop("Error in 'type'!")
    if (sum(table(type) != 1) > 0) 
        stop("Error in 'type'!")
    if (sum(type == "Chen-Hsu") == 0) 
        if (!is.null(bin)) 
            stop("'bin' just for Chen-Hsu model!")
    if (sum(type == "Chen-Hsu") != 0 & !is.null(bin)) {
        if (class(bin)[1] != "list") 
            stop("'bin' must be a list or NULL!")
        else if (length(bin) != length(n)) 
            stop("Length 'bin' must be length 'n'!")
    }
    if (plot != 0 & plot != 1) 
        stop("Error in 'plot'!")
    so.mohinh <- length(n) * length(type)
    thamso.n <- rep(n, rep(so.mohinh/length(n), length(n)))
    thamso.D1 <- rep(D1, so.mohinh/length(D1))
    thamso.D2 <- rep(D2, so.mohinh/length(D2))
    thamso.type <- rep(type, so.mohinh/length(type))
    thamso.bin <- rep("rong", so.mohinh)
    if (sum(type == "Chen-Hsu") != 0 & !is.null(bin)) {
        CH.vt.list <- 0
        for (CH in 1:so.mohinh) if (thamso.type[CH] == "Chen-Hsu") {
            CH.vt.list <- CH.vt.list + 1
            thamso.bin[CH] <- CH.vt.list
        }
    }
    thamso.mohinh <- data.frame(thamso.n, thamso.D1, thamso.D2, 
        thamso.type, thamso.bin)
    d1 = D1
    d2 = D2
    thoidiem <- namthang(ts)
    min.x = min(ts) - d1
    max.x = max(ts) + d2
    if (sum(type == "Chen-Hsu") > 0 & (sum(thamso.bin == "rong") == 
        so.mohinh)) {
        mohinh.CH <- thamso.mohinh[thamso.type == "Chen-Hsu", 
            ]
        L.CH <- vector("list", c(dim(mohinh.CH)[1]))
        for (mohinh in 1:c(dim(mohinh.CH)[1])) {
            h = (max.x - min.x)/n[mohinh]
            k <- 1:c(n[mohinh] + 1)
            U <- 1:c(n[mohinh])
            for (i in 1:(n[mohinh] + 1)) {
                if (i == 1) 
                  k[i] = min.x
                else {
                  k[i] = min.x + (i - 1) * h
                  U[i - 1] = paste("A", i - 1, sep = "")
                }
            }
            D <- data.frame(U, low = k[1:c(n[mohinh])], up = k[2:(n[mohinh] + 
                1)])
            D$Bw <- (1/2) * (D$low + D$up)
            loai <- 1:length(ts)
            for (i in 1:length(ts)) {
                for (j in 1:c(n[mohinh])) {
                  if (D$low[j] <= ts[i] & ts[i] <= D$up[j]) {
                    loai[i] = paste("A", j, sep = "")
                    break
                  }
                }
            }
            loai.old <- 1:length(loai)
            for (i in 1:length(loai)) {
                if (i == 1) 
                  loai.old[i] = NA
                else loai.old[i] = loai[i - 1]
            }
            D1 <- data.frame(ts, loai, loai.old)
            b <- table(D1$loai)
            ni <- 1:c(n[mohinh])
            for (i in 1:c(n[mohinh])) for (j in 1:c(n[mohinh])) {
                if (paste("A", i, sep = "") == names(b)[j] & 
                  j <= length(b)) {
                  ni[i] = b[j]
                  break
                }
                else {
                  if (j > length(b)) {
                    ni[i] = 0
                    break
                  }
                }
            }
            D$ni <- ni
            colnames(D) <- c("set", "dow", "up", "mid", "num")
            L.CH[[mohinh]] <- D
        }
        ten.CH <- rep(0, c(dim(mohinh.CH)[1]))
        for (ten.temp in 1:c(dim(mohinh.CH)[1])) ten.CH[ten.temp] <- paste("Chen-Hsu model of", 
            mohinh.CH[ten.temp, 1], "fuzzy set")
        names(L.CH) <- ten.CH
        KQ <- L.CH
        plot <- 0
    }
    test1 <- prod(length(type) == 1, type == "Chen-Hsu")
    if ((if (test1) 
        1
    else (sum(type == "Chen" | type == "Singh" | type == "Heuristic") > 
        0)) & (if (sum(type == "Chen-Hsu") > 0) {
        !is.null(bin)
    }
    else 1)) {
        KQ.big <- vector("list", length(n))
        for (n.fset in 1:length(n)) {
            h = (max.x - min.x)/n[n.fset]
            k <- 1:c(n[n.fset] + 1)
            U <- 1:c(n[n.fset])
            for (i in 1:(n[n.fset] + 1)) {
                if (i == 1) 
                  k[i] = min.x
                else {
                  k[i] = min.x + (i - 1) * h
                  U[i - 1] = paste("A", i - 1, sep = "")
                }
            }
            D <- data.frame(U, low = k[1:c(n[n.fset])], up = k[2:(n[n.fset] + 
                1)])
            D$Bw <- (1/2) * (D$low + D$up)
            loai <- 1:length(ts)
            for (i in 1:length(ts)) {
                for (j in 1:c(n[n.fset])) {
                  if (D$low[j] <= ts[i] & ts[i] <= D$up[j]) {
                    loai[i] = paste("A", j, sep = "")
                    break
                  }
                }
            }
            loai.old <- 1:length(loai)
            for (i in 1:length(loai)) {
                if (i == 1) 
                  loai.old[i] = NA
                else loai.old[i] = loai[i - 1]
            }
            D1 <- data.frame(ts, loai, loai.old)
            b <- table(D1$loai)
            ni <- 1:c(n[n.fset])
            for (i in 1:c(n[n.fset])) for (j in 1:c(n[n.fset])) {
                if (paste("A", i, sep = "") == names(b)[j] & 
                  j <= length(b)) {
                  ni[i] = b[j]
                  break
                }
                else {
                  if (j > length(b)) {
                    ni[i] = 0
                    break
                  }
                }
            }
            D$ni <- ni
            a <- 1:(n[n.fset] * n[n.fset])
            a <- matrix(a, nrow = n[n.fset])
            for (cot in 1:c(n[n.fset])) {
                for (i in 2:length(D1$loai.old)) {
                  for (j in 1:c(n[n.fset])) {
                    if (D1$loai.old[i] == paste("A", cot, sep = "") & 
                      a[j, cot] != paste("A", j, sep = "") & 
                      D1$loai[i] == paste("A", j, sep = "")) {
                      a[j, cot] = paste("A", j, sep = "")
                      break
                    }
                  }
                }
            }
            for (cot in 1:c(n[n.fset])) {
                for (j in 1:c(n[n.fset])) if (a[j, cot] != paste("A", 
                  j, sep = "")) 
                  a[j, cot] <- NA
            }
            if (sum(type == "Chen") > 0) {
                mo <- 1:c(n[n.fset])
                for (cot in 1:c(n[n.fset])) {
                  if (cot > 1) 
                    a[a == 1] <- NA
                  s <- length(na.omit(a[, cot]))
                  a[is.na(a)] <- 1
                  if (s == 1) {
                    for (j in 1:c(n[n.fset])) if (a[j, cot] == 
                      paste("A", j, sep = "")) {
                      t = D$Bw[j]
                      break
                    }
                    mo[cot] = t
                  }
                  else if (s > 1) {
                    t = 0
                    for (j in 1:c(n[n.fset])) if (a[j, cot] == 
                      paste("A", j, sep = "")) 
                      t = t + D$Bw[j]
                    mo[cot] = t/s
                  }
                  else if (s == 0) {
                    mo[cot] = D$Bw[cot]
                  }
                }
                db <- 1:length(loai.old)
                for (i in 1:length(loai.old)) {
                  if (i == 1) 
                    db[i] = NA
                  else for (j in 1:c(n[n.fset])) if (loai.old[i] == 
                    paste("A", j, sep = "")) {
                    db[i] = mo[j]
                    break
                  }
                  if (cot == c(n[n.fset])) 
                    a[a == 1] <- NA
                }
                dubao.Chen <- ts(db, start = start(ts), frequency = frequency(ts))
            }
            if (sum(type == "Singh") > 0) {
                F <- 1:length(ts)
                Ds <- 1:length(ts)
                X <- 1:length(ts)
                XX <- 1:length(ts)
                Y <- 1:length(ts)
                YY <- 1:length(ts)
                P <- 1:length(ts)
                PP <- 1:length(ts)
                Q <- 1:length(ts)
                QQ <- 1:length(ts)
                G <- 1:length(ts)
                GG <- 1:length(ts)
                H <- 1:length(ts)
                HH <- 1:length(ts)
                E <- ts
                tt = 0
                v <- 1:length(D$ni)
                for (t0 in 1:length(D$ni)) if (D$ni[t0] == 0) {
                  tt = tt + 1
                  v[tt] = t0
                }
                if (tt == 0) 
                  (v = NULL)
                else v = v[1:tt]
                if (!is.null(v)) {
                  Dt <- D[-v, ]
                  t1 = 1
                  while (t1 <= length(levels(Dt$U))) {
                    test = 0
                    for (t2 in 1:length(Dt$U)) if (levels(Dt$U)[t1] == 
                      Dt$U[t2]) {
                      test = 1
                      break
                    }
                    if (test == 0) 
                      levels(Dt$U)[t1] = NA
                    else t1 = t1 + 1
                  }
                }
                if (is.null(v)) 
                  Dt <- D
                for (i in 0:(length(ts) - 1)) {
                  if (i < 3) 
                    F[i + 1] <- NA
                  else {
                    R = 0
                    S = 0
                    Ds[i] = abs(abs(E[i] - E[i - 1]) - abs(E[i - 
                      1] - E[i - 2]))
                    X[i] = E[i] + (Ds[i]/2)
                    XX[i] = E[i] - (Ds[i]/2)
                    Y[i] = E[i] + Ds[i]
                    YY[i] = E[i] - Ds[i]
                    P[i] = E[i] + (Ds[i]/4)
                    PP[i] = E[i] - (Ds[i]/4)
                    Q[i] = E[i] + 2 * Ds[i]
                    QQ[i] = E[i] - 2 * Ds[i]
                    G[i] = E[i] + (Ds[i]/6)
                    GG[i] = E[i] - (Ds[i]/6)
                    H[i] = E[i] + 3 * Ds[i]
                    HH[i] = E[i] - 3 * Ds[i]
                    Dt.U <- as.character(Dt$U)
                    D1.loai <- as.character(D1$loai)
                    for (j in 1:length(Dt.U)) if (D1.loai[i + 
                      1] == Dt.U[j]) {
                      l = j
                      break
                    }
                    if (Dt$low[l] <= X[i] & X[i] <= Dt$up[l]) {
                      R = R + X[i]
                      S = S + 1
                    }
                    if (Dt$low[l] <= XX[i] & XX[i] <= Dt$up[l]) {
                      R = R + XX[i]
                      S = S + 1
                    }
                    if (Dt$low[l] <= Y[i] & Y[i] <= Dt$up[l]) {
                      R = R + Y[i]
                      S = S + 1
                    }
                    if (Dt$low[l] <= YY[i] & YY[i] <= Dt$up[l]) {
                      R = R + YY[i]
                      S = S + 1
                    }
                    if (Dt$low[l] <= P[i] & P[i] <= Dt$up[l]) {
                      R = R + P[i]
                      S = S + 1
                    }
                    if (Dt$low[l] <= PP[i] & PP[i] <= Dt$up[l]) {
                      R = R + PP[i]
                      S = S + 1
                    }
                    if (Dt$low[l] <= Q[i] & Q[i] <= Dt$up[l]) {
                      R = R + Q[i]
                      S = S + 1
                    }
                    if (Dt$low[l] <= QQ[i] & QQ[i] <= Dt$up[l]) {
                      R = R + QQ[i]
                      S = S + 1
                    }
                    if (Dt$low[l] <= G[i] & G[i] <= Dt$up[l]) {
                      R = R + G[i]
                      S = S + 1
                    }
                    if (Dt$low[l] <= GG[i] & GG[i] <= Dt$up[l]) {
                      R = R + GG[i]
                      S = S + 1
                    }
                    if (Dt$low[l] <= H[i] & H[i] <= Dt$up[l]) {
                      R = R + H[i]
                      S = S + 1
                    }
                    if (Dt$low[l] <= HH[i] & HH[i] <= Dt$up[l]) {
                      R = R + HH[i]
                      S = S + 1
                    }
                    F[i + 1] = (R + Dt$Bw[l])/(S + 1)
                  }
                }
                dubao.Singh <- ts(F, start = start(ts), frequency = frequency(ts))
            }
            if (sum(type == "Heuristic") > 0) {
                tap = "begin"
                t.thai = "calm"
                Bw.h = 0
                for (cot in 1:c(n[n.fset])) {
                  locate <- rep(0, n[n.fset])
                  for (i in 1:c(n[n.fset])) if (!is.na(a[i, cot])) 
                    locate[i] = i
                  locate[locate == 0] <- NA
                  locate <- na.omit(locate)
                  if (length(locate) > 0) {
                    l = 0
                    locate[length(locate) + 1] <- (n[n.fset] + 
                      1)
                    for (i in 1:(length(locate) - 1)) if (cot >= 
                      locate[i] & cot < locate[i + 1]) {
                      l = i
                      break
                    }
                    locate = locate[1:(length(locate) - 1)]
                    if (l != 0) {
                      t = 0
                      for (i in 1:l) t = t + D$Bw[locate[i]]
                      tb <- t/length(locate[1:l])
                      tap[length(tap) + 1] = paste("A", cot, 
                        sep = "")
                      t.thai[length(t.thai) + 1] = "down"
                      Bw.h[length(Bw.h) + 1] = tb
                    }
                    l = 0
                    for (i in 1:length(locate)) if (cot <= locate[i]) {
                      l = i
                      break
                    }
                    if (l != 0) {
                      t = 0
                      for (i in l:length(locate)) t = t + D$Bw[locate[i]]
                      tb <- t/length(locate[l:length(locate)])
                      tap[length(tap) + 1] = paste("A", cot, 
                        sep = "")
                      t.thai[length(t.thai) + 1] = "raise"
                      Bw.h[length(Bw.h) + 1] = tb
                    }
                  }
                }
                tap <- factor(tap[2:length(tap)])
                t.thai <- t.thai[2:length(t.thai)]
                Bw.h <- Bw.h[2:length(Bw.h)]
                D.h <- data.frame(tap, t.thai, Bw.h)
                Heuristic <- 1:length(ts)
                Heuristic[1] <- NA
                for (j in 2:length(ts)) {
                  xj <- ts[j] - ts[j - 1]
                  D1.loai.old <- as.character(D1$loai.old)
                  D.h.tap <- as.character(D.h$tap)
                  if (xj >= 0) {
                    for (v in 1:dim(D.h)[1]) if (D1.loai.old[j] == 
                      D.h.tap[v] & D.h$t.thai[v] == "raise") 
                      bt = v
                    Heuristic[j] = D.h$Bw.h[bt]
                  }
                  else if (xj < 0) {
                    for (v in 1:dim(D.h)[1]) if (D1.loai.old[j] == 
                      D.h.tap[v] & D.h$t.thai[v] == "down") 
                      bt = v
                    Heuristic[j] = D.h$Bw.h[bt]
                  }
                }
                dubao.Heuristic <- ts(Heuristic, start = start(ts), 
                  frequency = frequency(ts))
            }
            if (sum(type == "Chen-Hsu") > 0) {
                if (!is.null(bin)) {
                  bin.temp <- bin[[n.fset]]
                  if (!is.null(bin.temp)) {
                    if (min(bin.temp) <= min.x || max(bin.temp) >= 
                      max.x || length(bin.temp) < 1) 
                      stop(paste("Error in bin[[", n.fset, "]]!", 
                        sep = ""))
                    k2 <- 1:(length(bin.temp) + 2)
                    U2 <- 1:(length(bin.temp) + 1)
                    k2[1] <- min.x
                    k2[length(k2)] <- max.x
                    for (t in 2:(length(k2) - 1)) k2[t] = bin.temp[t - 
                      1]
                    for (t in 1:(length(bin.temp) + 1)) U2[t] = paste("A", 
                      t, sep = "")
                    n2 <- length(U2)
                    D.ch <- data.frame(U2, low2 = k2[1:n2], up2 = k2[2:(n2 + 
                      1)])
                    D.ch$Bw2 <- (1/2) * (D.ch$low + D.ch$up)
                    loai2 <- 1:length(ts)
                    for (i in 1:length(ts)) {
                      for (j in 1:n2) {
                        if (D.ch$low2[j] <= ts[i] & ts[i] <= 
                          D.ch$up2[j]) {
                          loai2[i] = paste("A", j, sep = "")
                          break
                        }
                      }
                    }
                    D1.ch <- data.frame(ts, loai2, loai2.old = c(NA, 
                      loai2[1:(length(loai2) - 1)]))
                    b <- table(D1.ch$loai2)
                    ni <- 1:n2
                    for (i in 1:n2) for (j in 1:n2) {
                      if (paste("A", i, sep = "") == names(b)[j] & 
                        j <= length(b)) {
                        ni[i] = b[j]
                        break
                      }
                      else {
                        if (j > length(b)) {
                          ni[i] = 0
                          break
                        }
                      }
                    }
                    D.ch$ni <- ni
                    tt = 0
                    v <- 1:length(D.ch$ni)
                    for (t0 in 1:length(D.ch$ni)) if (D.ch$ni[t0] == 
                      0) {
                      tt = tt + 1
                      v[tt] = t0
                    }
                    if (tt == 0) 
                      (v = NULL)
                    else v = v[1:tt]
                    if (!is.null(v)) {
                      Dt <- D.ch[-v, ]
                      t1 = 1
                      while (t1 <= length(levels(Dt$U2))) {
                        test = 0
                        for (t2 in 1:length(Dt$U2)) if (levels(Dt$U2)[t1] == 
                          Dt$U2[t2]) {
                          test = 1
                          break
                        }
                        if (test == 0) 
                          levels(Dt$U2)[t1] = NA
                        else t1 = t1 + 1
                      }
                    }
                    else Dt <- D.ch
                    X = ts
                    A <- as.character(D1.ch$loai2)
                    P <- 1:length(X)
                    tapmochonam2 <- as.character(Dt[, 1])
                    for (i in 1:length(tapmochonam2)) if (A[2] == 
                      tapmochonam2[i]) {
                      vt = i
                      break
                    }
                    P[1] <- NA
                    P[2] = Dt[vt, 4]
                    for (t in 2:(length(ts) - 1)) {
                      if (t == 2) 
                        P[t + 1] = rule1(X[t], X[t - 1], A[t + 
                          1], Dt)
                      else {
                        different = (X[t] - X[t - 1]) - (X[t - 
                          1] - X[t - 2])
                        if (A[t + 1] > A[t] & different >= 0) 
                          P[t + 1] = rule2(X[t], X[t - 1], X[t - 
                            2], A[t + 1], Dt)
                        else if (A[t + 1] > A[t] & different <= 
                          0) 
                          P[t + 1] = rule3(X[t], X[t - 1], X[t - 
                            2], A[t + 1], Dt)
                        else if (A[t + 1] < A[t] & different >= 
                          0) 
                          P[t + 1] = rule2(X[t], X[t - 1], X[t - 
                            2], A[t + 1], Dt)
                        else if (A[t + 1] < A[t] & different <= 
                          0) 
                          P[t + 1] = rule3(X[t], X[t - 1], X[t - 
                            2], A[t + 1], Dt)
                        else if (A[t + 1] == A[t] & different >= 
                          0) 
                          P[t + 1] = rule2(X[t], X[t - 1], X[t - 
                            2], A[t + 1], Dt)
                        else if (A[t + 1] == A[t] & different <= 
                          0) 
                          P[t + 1] = rule3(X[t], X[t - 1], X[t - 
                            2], A[t + 1], Dt)
                      }
                    }
                    dubao.ChenHsu <- ts(P, start = start(ts), 
                      frequency = frequency(ts))
                  }
                }
            }
            if (sum(type == "Chen") == 0) 
                dubao.Chen <- rep(0, length(ts))
            if (sum(type == "Singh") == 0) 
                dubao.Singh <- rep(0, length(ts))
            if (sum(type == "Heuristic") == 0) 
                dubao.Heuristic <- rep(0, length(ts))
            if (sum(type == "Chen-Hsu") == 0) 
                dubao.ChenHsu <- rep(0, length(ts))
            ten.Chen <- paste("Chen", n[n.fset], sep = "")
            ten.Singh <- paste("Singh", n[n.fset], sep = "")
            ten.Heuristic <- paste("Heuristic", n[n.fset], sep = "")
            if (!is.null(bin)) 
                ten.ChenHsu <- paste("ChenHsu", length(bin[[n.fset]]) + 
                  1, sep = "")
            if (is.null(bin)) 
                ten.ChenHsu <- paste("ChenHsu", n[[n.fset]], 
                  sep = "")
            ten.KQ.small <- c(ten.Chen, ten.Singh, ten.Heuristic, 
                ten.ChenHsu)
            KQ.small <- data.frame(dubao.Chen, dubao.Singh, dubao.Heuristic, 
                dubao.ChenHsu)
            tru.ra <- c(1:4) * c(1 * c(!is.na(KQ.small[1, ])))
            tru.ra <- tru.ra[-c(1:4) * c(tru.ra == 0)]
            if (length(tru.ra) > 0) {
                KQ.small <- data.frame(KQ.small[, -tru.ra])
                colnames(KQ.small) <- ten.KQ.small[-tru.ra]
            }
            if (length(tru.ra) == 0) 
                colnames(KQ.small) <- ten.KQ.small
            KQ.big[[n.fset]] <- KQ.small
        }
        KQ <- data.frame(thoidiem)
        for (gop.kq in 1:length(n)) KQ <- data.frame(KQ, KQ.big[[gop.kq]])
        KQ <- KQ[, -1]
        if (class(KQ)[1] != "ts") 
            rownames(KQ) <- as.character(thoidiem)
    }
    if (plot == TRUE) {
        if (c(par()$mfrow)[1] > 2 | c(par()$mfrow)[2] > 2) 
            warning("Graph only paint when: c(par()$mfrow)[1] < 3 & c(par()$mfrow)[1] < 3")
        else {
            goc <- ts
            dubao <- KQ
            n.dothi <- sum(par()$mfrow)
            n.dothi <- n.dothi/2
            if (n.dothi == 1) 
                n.dothi <- 1/0.8
            cex.legend <- n.dothi
            main.plot <- c("Actual series vs forecated series by", 
                "fuzzy time series models")
            G.lty <- c(1, rep(2:(length(n) + 1), length(type)))
            G.col <- c(1, rep(2:(length(type) + 1), length(n)))
            G.pch <- c(8, rep(15:(length(n) + 14), length(type)))
            kieuduong <- rep(1, 1e+05)
            mausac <- rep(c("burlywood4", "darkgreen", "deepskyblue3", 
                "chartreuse4", "firebrick2", "darkorchid", "gold", 
                "darkslateblue", "deeppink", "burlywood2", "lightsalmon2", 
                "mediumpurple", "mediumseagreen", "greenyellow", 
                "lightslateblue", "mistyrose3", "indianred1", 
                "indianred4", "maroon", "orange", "plum2", "sienna2", 
                "orange4", "red1", "slategray3"), 10000)
            kieudiem <- rep(c(15, 17, 19), 1e+05)
            for (id in 1:length(G.lty)) G.lty[id] <- kieuduong[id]
            for (id in 1:length(G.col)) G.col[id] <- mausac[id]
            for (id in 1:length(G.pch)) G.pch[id] <- kieudiem[id]
            KQ1 <- data.frame(KQ, ts)
            y.range <- max(KQ1, na.rm = 1) - min(KQ1, na.rm = 1)
            y.min <- min(KQ1, na.rm = 1) - 1/12 * (y.range)
            y.max <- max(KQ1, na.rm = 1)
            if ((length(n) * length(type) > 4) & (length(n) * 
                length(type) < 8)) 
                y.max <- y.max + 1/10 * (y.range)
            if ((length(n) * length(type) > 8) & (length(n) * 
                length(type) < 16)) 
                y.max <- y.max + 2/10 * (y.range)
            if ((length(n) * length(type) > 16)) 
                y.max <- y.max + 3/10 * (y.range)
            if ((c(par()$mfrow)[1] == 1) & (c(par()$mfrow)[2] == 
                1)) 
                n.chuthich <- 4
            if ((c(par()$mfrow)[1] == 1) & (c(par()$mfrow)[2] == 
                2)) 
                n.chuthich <- 2
            if ((c(par()$mfrow)[1] == 2) & (c(par()$mfrow)[2] == 
                1)) 
                n.chuthich <- 5
            if ((c(par()$mfrow)[1] == 2) & (c(par()$mfrow)[2] == 
                2)) 
                n.chuthich <- 3
            if (length(goc) < 50) {
                tde1 <- c(paste(main.plot[1], main.plot[2]))
                tde2 <- main.plot
                if (c(par()$mfrow)[2] == 1) 
                  gve <- list(col = G.col, cex.main = 0.8, type = "o", 
                    pch = G.pch, ylim = c(y.min, y.max), xlab = "point", 
                    ylab = "data", bty = "l", main = tde1, lty = G.lty)
                if (c(par()$mfrow)[2] == 2) 
                  gve <- list(col = G.col, cex.main = 0.8, type = "o", 
                    pch = G.pch, ylim = c(y.min, y.max), xlab = "point", 
                    ylab = "data", bty = "l", main = tde2, lty = G.lty)
                ts.plot(goc, dubao, gpars = gve)
                legend("topleft", "(x,y)", c("Actual", colnames(KQ)), 
                  ncol = n.chuthich, col = G.col, lty = G.lty, 
                  pch = G.pch, cex = 1/cex.legend, box.lty = 0)
            }
            if (length(goc) > 49) {
                tde1 <- c(paste(main.plot[1], main.plot[2]))
                tde2 <- main.plot
                if (c(par()$mfrow)[2] == 1) 
                  gve <- list(col = G.col, cex.main = 0.8, type = "l", 
                    ylim = c(y.min, y.max), xlab = "point", ylab = "data", 
                    bty = "l", main = tde1, lty = G.lty)
                if (c(par()$mfrow)[2] == 2) 
                  gve <- list(col = G.col, cex.main = 0.8, type = "l", 
                    ylim = c(y.min, y.max), xlab = "point", ylab = "data", 
                    bty = "l", main = tde2, lty = G.lty)
                ts.plot(goc, dubao, gpars = gve)
                legend("topleft", "(x,y)", c("Actual", colnames(KQ)), 
                  ncol = n.chuthich, col = G.col, lty = G.lty, 
                  cex = 1/cex.legend, box.lty = 0)
            }
            if (grid == 1) 
                grid.on()
        }
    }
    KQ
}
