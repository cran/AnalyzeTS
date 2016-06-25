Abbasov.Cs3 <-
function (ts, n = 7, w = 7, D1 = 0, D2 = 0, Cs = NULL, fty = c("ts", 
    "f")) 
{
    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x - 
        round(x)) < tol
    computeVt <- function(matrixVt, fuzzyset, w) {
        n <- (dim(matrixVt)[1] - w)
        cot <- dim(matrixVt)[2]
        Vt <- 1:dim(matrixVt)[1]
        Vt[1:w] <- NA
        for (i in 1:n) {
            O <- matrixVt[i:(w - 1 + (i - 1)), ]
            if (w == 2) 
                O <- t(as.matrix(O))
            K <- matrixVt[w + (i - 1), ]
            R <- O
            for (i1 in 1:cot) for (j1 in 1:(w - 1)) if (O[j1, 
                i1] > K[i1]) 
                R[j1, i1] <- K[i1]
            F <- 1:cot
            for (i2 in 1:cot) F[i2] <- max(R[, i2])
            Vt[w + i] <- sum(F * fuzzyset$Bw)/sum(F)
        }
        Vt
    }
    computeVt2 <- function(matrixVt2, fuzzyset, w) {
        cot <- dim(matrixVt2)[2]
        dong <- (dim(matrixVt2)[1] - 1)
        O <- matrixVt2[1:dong, ]
        if (w == 2) 
            O <- t(as.matrix(O))
        K <- matrixVt2[(dong + 1), ]
        R <- O
        for (i1 in 1:cot) for (j1 in 1:(w - 1)) if (O[j1, i1] > 
            K[i1]) 
            R[j1, i1] <- K[i1]
        F <- 1:cot
        for (i2 in 1:cot) F[i2] <- max(R[, i2])
        Vt2 <- sum(F * fuzzyset$Bw)/sum(F)
        Vt2
    }
    if (!is.numeric(ts)) 
        stop("Error in 'ts'!")
    if (sum(is.na(ts) * 1) > 1) 
        stop("Time series contain 'NA value'!")
    if (length(n) > 1 | is.na(n) | !is.numeric(n) | n < 1 | !is.wholenumber(n)) 
        stop("Error in 'n'!")
    if (length(w) > 1 | is.null(w) | is.na(w) | !is.numeric(w) | 
        w < 2 | !is.wholenumber(w) | w > length(ts)) 
        stop("Error in 'w'!")
    if (length(D1) > 1 | is.na(D1) | !is.numeric(D1)) 
        stop("Error in 'D1'!")
    if (length(D2) > 1 | is.na(D2) | !is.numeric(D2)) 
        stop("Error in 'D2'!")
    if (!is.numeric(Cs)) 
        stop("Error in 'Cs'!")
    fty <- match.arg(fty)
    if (fty != "ts" & fty != "f") 
        stop("Error in 'fty'!")
    ts1 <- as.vector(diff(ts))
    min.x = min(ts1) - D1
    max.x = max(ts1) + D2
    h = (max.x - min.x)/n
    k <- 1:(n + 1)
    U <- 1:n
    for (i in 1:(n + 1)) {
        if (i == 1) 
            k[i] = min.x
        else {
            k[i] = min.x + (i - 1) * h
            U[i - 1] = paste("u", i - 1, sep = "")
        }
    }
    D <- data.frame(U, low = k[1:n], up = k[2:(n + 1)])
    D$Bw <- (1/2) * (D$low + D$up)
    table1 <- D
    ME <- 1:length(Cs)
    MAE <- 1:length(Cs)
    MPE <- 1:length(Cs)
    MAPE <- 1:length(Cs)
    MSE <- 1:length(Cs)
    RMSE <- 1:length(Cs)
    for (t in 1:length(Cs)) {
        C <- Cs[t]
        MATRIX <- matrix(1:(length(ts1) * n), ncol = n)
        for (i in 1:length(ts1)) {
            for (j in 1:n) {
                my.At <- 1/(1 + (C * (ts1[i] - table1$Bw[j]))^2)
                MATRIX[i, j] <- my.At
            }
        }
        V <- computeVt(MATRIX, table1, w)
        N <- ts[-length(ts)] + V
        MATRIX1 <- MATRIX[(dim(MATRIX)[1] - w + 1):dim(MATRIX)[1], 
            ]
        V1 <- computeVt2(MATRIX1, table1, w)
        if (fty == "ts") 
            N1 <- ts[length(ts)] + V1
        if (fty == "f") 
            N1 <- N[length(N)] + V1
        danso <- c(N, N1)
        et <- na.omit(ts - danso)
        ne <- length(et)
        Yt <- ts[(length(ts) - ne + 1):length(ts)]
        ME[t] = sum(et)/ne
        MAE[t] = sum(abs(et))/ne
        MPE[t] = sum((et/Yt) * 100)/ne
        MAPE[t] = sum((abs(et)/Yt) * 100)/ne
        MSE[t] = sum(et * et)/ne
        RMSE[t] = sqrt(sum(et * et)/ne)
    }
    accuracy <- data.frame(ME = ME, MAE = MAE, MPE = MPE, MAPE = MAPE, 
        MSE = MSE, RMSE = RMSE)
    sort.accuracy <- accuracy
    for (xieploai in 1:6) {
        saiso <- accuracy[, xieploai]
        xl <- 1:length(saiso)
        saisoxl <- sort(saiso)
        for (k in 1:length(saiso)) {
            for (l in 1:length(xl)) {
                if (saiso[k] == saisoxl[l]) {
                  xl[k] <- l
                  break
                }
            }
        }
        sort.accuracy[, xieploai] <- xl
    }
    table2 <- data.frame(Cs, accuracy, sort.accuracy)
    names(table2) <- c("C values", "ME", "MAE", "MPE", "MAPE", 
        "MSE", "RMSE", "MExl", "MAExl", "MPExl", "MAPExl", "MSExl", 
        "RMSExl")
    answer <- table2
    answer
}
