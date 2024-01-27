#title~ Approximating sin(x) around x0=pi/2 by Taylor's polynomial
fun1 = function(x){sin(x)}
fun2 = function(x){1-(x-pi/2)^2/2}
x = seq(pi/4,3*pi/4,.01)
matplot(x,cbind(fun1(x),fun2(x)),type='l',col=c('blue','red'))
legend('topright',c('sin(x)','1-(x-pi/2)^2/2'),lty=c(1,2),col=c('blue','red'))