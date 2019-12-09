pmax <-
function (ts) 
{
    pmx <- floor(12 * (length(ts)/100)^(1/4))
    pmx <- c(`pmax value` = pmx)
    pmx
}
