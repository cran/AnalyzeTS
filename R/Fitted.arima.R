Fitted.arima <-
function(object){
if(prod(class(object)!="Arima",class(object)!="Arimax")) 
stop("The 'object' must be a \"Arima\" or \"Arimax\"!")
eval(object$call$x) - object$residuals
}
