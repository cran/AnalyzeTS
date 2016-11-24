ChenHsu.bin <-
function (table, n.subset) 
{
if(class(table)!="data.frame")
stop("'table' must be 'table1' component in result returning from fuzzy.ts1(...,type=\"Chen-Hsu\",bin=\"NUL\") function!")

str.real<-toString(colnames(table))
str.test<-"set, dow, up, mid, num"
if(str.real!=str.test)
stop("'table' must be 'table1' component in result returning from fuzzy.ts1(...,type=\"Chen-Hsu\",bin=\"NUL\") function!")

if (length(n.subset) != dim(table)[1]) 
stop("Length of 'n.subset' must be equal fuzzy set number!")

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
