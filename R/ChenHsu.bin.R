ChenHsu.bin <-
function (table, n.subset) 
{
    if (length(n.subset) != dim(table)[1]) 
        stop("Error in n.subset!")
    b <- 1:(sum(n.subset) - 1)
    t = 0
    for (j in 1:length(n.subset)) {
        hi <- (table[j, 3] - table[j, 2])/n.subset[j]
        for (i in 1:n.subset[j]) {
            t = t + 1
            b[t] <- table[j, 2] + i * hi
        }
    }
    b <- b[-length(b)]
    b
}
