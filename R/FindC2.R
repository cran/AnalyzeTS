FindC2 <-
function (ts, n = 7, w = 7, D1 = 0, D2 = 0, type = "MAE", error = 1e-04, 
    trace = 0, MinC.start = 0, MaxC.start = 1000) 
{
    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x - 
        round(x)) < tol
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
    if (trace != 0 & trace != 1) 
        stop("Error in 'trace'!")
    if (type != "ME" & type != "MAE" & type != "MPE" & type != 
        "MAPE" & type != "MSE" & type != "RMSE") 
        stop("Error in 'type'!")
    Min <- MinC.start
    Max <- MaxC.start
    Step <- (Max - Min)/1000
    KQ1 <- data.frame(1:1001)
    while (1) {
        C.table <- Abbasov.Cs2(ts, n = n, w = w, D1 = D1, D2 = D2, 
            Cs = seq(Min, Max, Step))
        C.value <- data.frame(C.table[, c("C values", type, paste(type, 
            "xl", sep = ""))])
        KQ1 <- data.frame(KQ1, C.value)
        locate <- KQ1[C.value[, 2] == min(C.value[, 2]), 1]
        KQ0 <- c(C.value[locate[1], 1], C.value[locate[1], 2])
        C.min <- min(C.value[, 1])
        C.max <- max(C.value[, 1])
        C.error <- C.max - C.min
        if (C.error <= error) 
            break
        C.center <- KQ0[1]
        Min <- C.center - Step
        Max <- C.center + Step
        Step <- (Max - Min)/1000
    }
    if (trace == 0) {
        names(KQ0) <- c("C value", paste(type, "value"))
        KQ <- KQ0
    }
    if (trace == 1) {
        KQ1 <- KQ1[, -1]
        ncol <- dim(KQ1)[2]
        nlist <- ncol/3
        L <- vector("list", nlist)
        for (i in 1:nlist) {
            L.member <- KQ1[, (3 * (i - 1) + 1):(3 * i)]
            names(L.member) <- c("C values", paste(type, "values"), 
                paste("Sort by", type))
            L[[i]] <- L.member
        }
        L.name <- "list"
        for (i in 1:nlist) {
            L.member <- L[[i]]
            L.member.name <- paste("Step ", i, ": MinC = ", L.member[1, 
                1], ", MaxC = ", L.member[1001, 1], ", StepC = ", 
                (L.member[1001, 1] - L.member[1, 1])/1000, sep = "")
            L.name <- c(L.name, L.member.name)
        }
        L.name <- L.name[-1]
        names(L) <- L.name
        KQ <- L
    }
    KQ
}
