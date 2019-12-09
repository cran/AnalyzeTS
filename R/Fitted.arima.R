Fitted.arima <-
function (object) 
{
    if (prod(class(object)[1] != "Arima", class(object)[1] != "Arimax")) 
        stop("The 'object' must be a \"Arima\" or \"Arimax\"!")
    eval(object$call$x) - object$residuals
}
