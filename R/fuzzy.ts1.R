fuzzy.ts1 <-
function (ts, n = 5, D1 = 0, D2 = 0, type = c("Chen", "Singh", 
    "Heuristic", "Chen-Hsu"), bin = NULL, trace = FALSE, plot = FALSE) 
{
    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x - 
        round(x)) < tol
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
    quanhe <- function(ts, type) {
        qh <- 1:length(ts)
        if (type == "Chen") 
            for (i in 1:length(ts)) if (i > 1) 
                qh[i] <- c("<--")
        if (type == "Singh") 
            for (i in 1:length(ts)) if (i > 1) 
                qh[i] <- c("<--")
        if (type == "Heuristic") 
            for (i in 1:length(ts)) if (i > 1) 
                qh[i] <- c("<--")
        if (type == "Chen-Hsu") 
            for (i in 1:length(ts)) if (i > 1) 
                qh[i] <- c("<--")
        qh[qh != "<--"] <- "-x-"
        qh
    }
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
        stop("Trong chuoi co gia tri NA!")
    if (is.na(D1) || !is.numeric(D1)) 
        stop("Error in 'D1'!")
    if (is.na(D2) || !is.numeric(D2)) 
        stop("Error in 'D2'!")
    if (is.na(n) || !is.numeric(n) || n < 1 || !is.wholenumber(n)) 
        stop("Error in 'n'!")
    type <- match.arg(type)
    if (type != "Chen" & type != "Singh" & type != "Heuristic" & 
        type != "Chen-Hsu") 
        stop("Error in 'type'!")
    if (type != "Chen-Hsu") 
        if (!is.null(bin)) 
            stop("'bin' just for Chen-Hsu model!")
    if (plot != 0 & plot != 1) 
        stop("Error in 'plot'!")
    if (trace != 0 & trace != 1) 
        stop("Error in 'plot'!")
    d1 = D1
    d2 = D2
    thoidiem <- namthang(ts)
    quanhemo <- quanhe(ts, type)
    min.x = min(ts) - d1
    max.x = max(ts) + d2
    h = (max.x - min.x)/n
    k <- 1:(n + 1)
    U <- 1:n
    for (i in 1:(n + 1)) {
        if (i == 1) 
            k[i] = min.x
        else {
            k[i] = min.x + (i - 1) * h
            U[i - 1] = paste("A", i - 1, sep = "")
        }
    }
    D <- data.frame(U, low = k[1:n], up = k[2:(n + 1)])
    D$Bw <- (1/2) * (D$low + D$up)
    loai <- 1:length(ts)
    for (i in 1:length(ts)) {
        for (j in 1:n) {
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
    quanhemokq <- paste(loai, quanhemo, loai.old, sep = "")
    D1 <- data.frame(ts, loai, loai.old)
    b <- table(D1$loai)
    ni <- 1:n
    for (i in 1:n) for (j in 1:n) {
        if (paste("A", i, sep = "") == names(b)[j] & j <= length(b)) {
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
    a <- 1:(n * n)
    a <- matrix(a, nrow = n)
    for (cot in 1:n) {
        for (i in 2:length(D1$loai.old)) {
            for (j in 1:n) {
                if (D1$loai.old[i] == paste("A", cot, sep = "") & 
                  a[j, cot] != paste("A", j, sep = "") & D1$loai[i] == 
                  paste("A", j, sep = "")) {
                  a[j, cot] = paste("A", j, sep = "")
                  break
                }
            }
        }
    }
    for (cot in 1:n) {
        for (j in 1:n) if (a[j, cot] != paste("A", j, sep = "")) 
            a[j, cot] <- NA
    }
    if (type == "Chen") {
        mo <- 1:n
        for (cot in 1:n) {
            if (cot > 1) 
                a[a == 1] <- NA
            s <- length(na.omit(a[, cot]))
            a[is.na(a)] <- 1
            if (s == 1) {
                for (j in 1:n) if (a[j, cot] == paste("A", j, 
                  sep = "")) {
                  t = D$Bw[j]
                  break
                }
                mo[cot] = t
            }
            else if (s > 1) {
                t = 0
                for (j in 1:n) if (a[j, cot] == paste("A", j, 
                  sep = "")) 
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
            else for (j in 1:n) if (loai.old[i] == paste("A", 
                j, sep = "")) {
                db[i] = mo[j]
                break
            }
        }
        db <- ts(db, start = start(ts), frequency = frequency(ts))
        DB1 <- data.frame(thoidiem, D1$ts, quanhemokq, db)
    }
    if (type == "Singh") {
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
                for (j in 1:length(Dt.U)) if (D1.loai[i + 1] == 
                  Dt.U[j]) {
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
        F <- ts(F, start = start(ts), frequency = frequency(ts))
        DB2 <- data.frame(thoidiem, sl.goc = D1$ts, quanhemo = quanhemokq, 
            .....sl.mo = F)
    }
    if (type == "Heuristic") {
        tap = "begin"
        t.thai = "calm"
        Bw.h = 0
        for (cot in 1:n) {
            locate <- rep(0, n)
            for (i in 1:n) if (!is.na(a[i, cot])) 
                locate[i] = i
            locate[locate == 0] <- NA
            locate <- na.omit(locate)
            if (length(locate) > 0) {
                l = 0
                locate[length(locate) + 1] <- (n + 1)
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
                  tap[length(tap) + 1] = paste("A", cot, sep = "")
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
                  tap[length(tap) + 1] = paste("A", cot, sep = "")
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
        Heuristic <- ts(Heuristic, start = start(ts), frequency = frequency(ts))
        DB3 <- data.frame(thoidiem, sl.goc = D1$ts, quanhemo = quanhemokq, 
            .....sl.mo = Heuristic)
    }
    if (type == "Chen-Hsu") {
        if (!is.null(bin)) {
            if (min(bin) <= min.x || max(bin) >= max.x || length(bin) < 
                1) 
                stop("Error in 'bin'!")
            k2 <- 1:(length(bin) + 2)
            U2 <- 1:(length(bin) + 1)
            k2[1] <- min.x
            k2[length(k2)] <- max.x
            for (t in 2:(length(k2) - 1)) k2[t] = bin[t - 1]
            for (t in 1:(length(bin) + 1)) U2[t] = paste("A", 
                t, sep = "")
            n2 <- length(U2)
            D.ch <- data.frame(U2, low2 = k2[1:n2], up2 = k2[2:(n2 + 
                1)])
            D.ch$Bw2 <- (1/2) * (D.ch$low + D.ch$up)
            loai2 <- 1:length(ts)
            for (i in 1:length(ts)) {
                for (j in 1:n2) {
                  if (D.ch$low2[j] <= ts[i] & ts[i] <= D.ch$up2[j]) {
                    loai2[i] = paste("A", j, sep = "")
                    break
                  }
                }
            }
            D1.ch <- data.frame(ts, loai2, loai2.old = c(NA, 
                loai2[1:(length(loai2) - 1)]))
            b <- table(D1.ch$loai2)
            quanhemokq <- paste(D1.ch$loai2, quanhemo, D1.ch$loai2.old, 
                sep = "")
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
            for (i in 1:length(tapmochonam2)) if (A[2] == tapmochonam2[i]) {
                vt = i
                break
            }
            P[1] <- NA
            P[2] = Dt[vt, 4]
            for (t in 2:(length(ts) - 1)) {
                if (t == 2) 
                  P[t + 1] = rule1(X[t], X[t - 1], A[t + 1], 
                    Dt)
                else {
                  different = (X[t] - X[t - 1]) - (X[t - 1] - 
                    X[t - 2])
                  if (A[t + 1] > A[t] & different >= 0) 
                    P[t + 1] = rule2(X[t], X[t - 1], X[t - 2], 
                      A[t + 1], Dt)
                  else if (A[t + 1] > A[t] & different <= 0) 
                    P[t + 1] = rule3(X[t], X[t - 1], X[t - 2], 
                      A[t + 1], Dt)
                  else if (A[t + 1] < A[t] & different >= 0) 
                    P[t + 1] = rule2(X[t], X[t - 1], X[t - 2], 
                      A[t + 1], Dt)
                  else if (A[t + 1] < A[t] & different <= 0) 
                    P[t + 1] = rule3(X[t], X[t - 1], X[t - 2], 
                      A[t + 1], Dt)
                  else if (A[t + 1] == A[t] & different >= 0) 
                    P[t + 1] = rule2(X[t], X[t - 1], X[t - 2], 
                      A[t + 1], Dt)
                  else if (A[t + 1] == A[t] & different <= 0) 
                    P[t + 1] = rule3(X[t], X[t - 1], X[t - 2], 
                      A[t + 1], Dt)
                }
            }
            P <- ts(P, start = start(ts), frequency = frequency(ts))
            DB4 <- data.frame(thoidiem, sl.goc = D1.ch$ts, quanhemo = quanhemokq, 
                .....sl.mo = P)
            namescot1 <- "set"
            namescot2 <- "dow"
            namescot3 <- "up"
            namescot4 <- "mid"
            namescot5 <- "num"
            colnames(D.ch) <- c(namescot1, namescot2, namescot3, 
                namescot4, namescot5)
        }
    }
    if (type == "Chen") 
        accuracy <- av.res(Y = data.frame(DB1[, 2]), F = data.frame(Chen = DB1[, 
            4]))
    if (type == "Singh") 
        accuracy <- av.res(Y = data.frame(DB2[, 2]), F = data.frame(Singh = DB2[, 
            4]))
    if (type == "Heuristic") 
        accuracy <- av.res(Y = data.frame(DB3[, 2]), F = data.frame(Heuristic = DB3[, 
            4]))
    if (type == "Chen-Hsu" & !is.null(bin)) 
        accuracy <- av.res(Y = data.frame(DB4[, 2]), F = data.frame(`Chen-Hsu` = DB4[, 
            4]))
    if (trace == TRUE) {
        namescot1 <- "set"
        namescot2 <- "dow"
        namescot3 <- "up"
        namescot4 <- "mid"
        namescot5 <- "num"
        colnames(D) <- c(namescot1, namescot2, namescot3, namescot4, 
            namescot5)
        namescot1 <- "point"
        namescot2 <- "ts"
        namescot3 <- "relative"
        namescot4 <- "forecast"
        if (type == "Chen") 
            colnames(DB1) <- c(namescot1, namescot2, namescot3, 
                namescot4)
        if (type == "Singh") 
            colnames(DB2) <- c(namescot1, namescot2, namescot3, 
                namescot4)
        if (type == "Heuristic") 
            colnames(DB3) <- c(namescot1, namescot2, namescot3, 
                namescot4)
        if (type == "Chen-Hsu" & !is.null(bin)) 
            colnames(DB4) <- c(namescot1, namescot2, namescot3, 
                namescot4)
        if (type == "Chen") 
            MO <- list(type = "Chen", table1 = D, table2 = DB1, 
                accuracy = accuracy)
        if (type == "Singh") 
            MO <- list(type = "Singh", table1 = D, table2 = DB2, 
                accuracy = accuracy)
        if (type == "Heuristic") 
            MO <- list(type = "Heuristic", table1 = D, table2 = DB3, 
                accuracy = accuracy)
        if (type == "Chen-Hsu") {
            if (is.null(bin)) 
                MO <- list(type = "Chen-Hsu", table1 = D)
            else if (!is.null(bin)) 
                MO <- list(type = "Chen-Hsu", table1 = D.ch, 
                  table2 = DB4, accuracy = accuracy)
        }
    }
    else if (trace == FALSE) {
        namescot1 <- "set"
        namescot2 <- "dow"
        namescot3 <- "up"
        namescot4 <- "mid"
        namescot5 <- "num"
        colnames(D) <- c(namescot1, namescot2, namescot3, namescot4, 
            namescot5)
        if (type == "Chen") 
            MO <- DB1[, 4]
        if (type == "Singh") 
            MO <- DB2[, 4]
        if (type == "Heuristic") 
            MO <- DB3[, 4]
        if (type == "Chen-Hsu" & !is.null(bin)) 
            MO <- DB4[, 4]
        if (type == "Chen-Hsu" & is.null(bin)) 
            MO <- list(type = "Chen-Hsu", table1 = D)
    }
    else MO <- c("trace must be 'TRUE' or 'FALSE'")
    if (plot == TRUE) {
        if (type == "Chen") {
            goc <- DB1[, 2]
            dubao <- DB1[, 4]
            n.dothi <- prod(par()$mfrow)
            n.dothi
            if (length(goc) < 50) {
                plot(goc, col = "blue", main = paste("Chen", 
                  n, "fuzzy set"), type = "o", pch = 16, ylim = c(min(c(goc, 
                  dubao), na.rm = 1), max(c(goc, dubao), na.rm = 1)), 
                  xlab = "index", ylab = "data")
                lines(dubao, col = "red", type = "o", pch = 18)
                legend("bottomright", "(x,y)", c("ts", "forecast"), 
                  col = c("blue", "red"), lty = c(1, 1), pch = c(16, 
                    18), cex = 1/n.dothi)
            }
            if (length(goc) > 49) {
                plot(goc, col = "blue", main = paste("Chen", 
                  n, "fuzzy set"), type = "l", ylim = c(min(c(goc, 
                  dubao), na.rm = 1), max(c(goc, dubao), na.rm = 1)), 
                  xlab = "index", ylab = "data")
                lines(dubao, col = "red", type = "l")
                legend("bottomright", "(x,y)", c("ts", "forecast"), 
                  col = c("blue", "red"), lty = c(1, 1), cex = 1/n.dothi)
            }
            if (n.dothi == 1) {
                k.ve <- par()$yaxp
                h = (k.ve[2] - k.ve[1])/k.ve[3]
                for (i in 0:k.ve[3]) abline(h = k.ve[1] + h * 
                  i, lty = 3, col = "gray")
            }
        }
        if (type == "Singh") {
            goc <- DB2[, 2]
            dubao <- DB2[, 4]
            n.dothi <- prod(par()$mfrow)
            n.dothi
            if (length(goc) < 50) {
                plot(goc, col = "blue", main = paste("Singh", 
                  n, "fuzzy set"), type = "o", pch = 16, ylim = c(min(c(goc, 
                  dubao), na.rm = 1), max(c(goc, dubao), na.rm = 1)), 
                  xlab = "index", ylab = "data")
                lines(dubao, col = "red", type = "o", pch = 18)
                legend("bottomright", "(x,y)", c("ts", "forecast"), 
                  col = c("blue", "red"), lty = c(1, 1), pch = c(16, 
                    18), cex = 1/n.dothi)
            }
            if (length(goc) > 49) {
                plot(goc, col = "blue", main = paste("Singh", 
                  n, "fuzzy set"), type = "l", ylim = c(min(c(goc, 
                  dubao), na.rm = 1), max(c(goc, dubao), na.rm = 1)), 
                  xlab = "index", ylab = "data")
                lines(dubao, col = "red", type = "l")
                legend("bottomright", "(x,y)", c("ts", "forecast"), 
                  col = c("blue", "red"), lty = c(1, 1), , cex = 1/n.dothi)
            }
            if (n.dothi == 1) {
                k.ve <- par()$yaxp
                h = (k.ve[2] - k.ve[1])/k.ve[3]
                for (i in 0:k.ve[3]) abline(h = k.ve[1] + h * 
                  i, lty = 3, col = "gray")
            }
        }
        if (type == "Heuristic") {
            goc <- DB3[, 2]
            dubao <- DB3[, 4]
            n.dothi <- prod(par()$mfrow)
            n.dothi
            if (length(goc) < 50) {
                plot(goc, col = "blue", main = paste("Heuristic", 
                  n, "fuzzy set"), type = "o", pch = 16, ylim = c(min(c(goc, 
                  dubao), na.rm = 1), max(c(goc, dubao), na.rm = 1)), 
                  xlab = "index", ylab = "data")
                lines(dubao, col = "red", type = "o", pch = 18)
                legend("bottomright", "(x,y)", c("ts", "forecast"), 
                  col = c("blue", "red"), lty = c(1, 1), pch = c(16, 
                    18), cex = 1/n.dothi)
            }
            if (length(goc) > 49) {
                plot(goc, col = "blue", main = paste("Heuristic", 
                  n, "fuzzy set"), type = "l", ylim = c(min(c(goc, 
                  dubao), na.rm = 1), max(c(goc, dubao), na.rm = 1)), 
                  xlab = "index", ylab = "data")
                lines(dubao, col = "red", type = "l")
                legend("bottomright", "(x,y)", c("ts", "forecast"), 
                  col = c("blue", "red"), lty = c(1, 1), cex = 1/n.dothi)
            }
            if (n.dothi == 1) {
                k.ve <- par()$yaxp
                h = (k.ve[2] - k.ve[1])/k.ve[3]
                for (i in 0:k.ve[3]) abline(h = k.ve[1] + h * 
                  i, lty = 3, col = "gray")
            }
        }
        if (type == "Chen-Hsu") {
            if (!is.null(bin)) {
                n = dim(D.ch)[1]
                goc <- DB4[, 2]
                dubao <- DB4[, 4]
                n.dothi <- prod(par()$mfrow)
                n.dothi
                if (length(goc) < 50) {
                  plot(goc, col = "blue", main = paste("Chen-Hsu", 
                    n, "fuzzy set"), type = "o", pch = 16, ylim = c(min(c(goc, 
                    dubao), na.rm = 1), max(c(goc, dubao), na.rm = 1)), 
                    xlab = "index", ylab = "data")
                  lines(dubao, col = "red", type = "o", pch = 18)
                  legend("bottomright", "(x,y)", c("ts", "forecast"), 
                    col = c("blue", "red"), lty = c(1, 1), pch = c(16, 
                      18), cex = 1/n.dothi)
                }
                if (length(goc) > 49) {
                  plot(goc, col = "blue", main = paste("Chen-Hsu", 
                    n, "fuzzy set"), type = "l", ylim = c(min(c(goc, 
                    dubao), na.rm = 1), max(c(goc, dubao), na.rm = 1)), 
                    xlab = "index", ylab = "data")
                  lines(dubao, col = "red", type = "l")
                  legend("bottomright", "(x,y)", c("ts", "forecast"), 
                    col = c("blue", "red"), lty = c(1, 1), cex = 1/n.dothi)
                }
                if (n.dothi == 1) {
                  k.ve <- par()$yaxp
                  h = (k.ve[2] - k.ve[1])/k.ve[3]
                  for (i in -2:(k.ve[3] + 2)) abline(h = k.ve[1] + 
                    h * i, lty = 3, col = "gray")
                }
            }
        }
    }
    MO
}
