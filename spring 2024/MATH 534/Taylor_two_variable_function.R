# Second Taylor approximation of f(x1, x2) = x1 *exp(x2) + 1 around the point (1,0)
# The approximation is 1 + x1 + x1*x2 + x2^2/2
library(rgl)
f <- function(x1, x2){x1 *exp(x2) + 1}
x1 <- seq(0, 2, 0.1)
x2 <- seq(-1, 2, 0.1)
f <- outer(x1, x2, FUN = f)
persp3d(x1, x2, f, col = "lightblue",shade = 0.1, alpha = 0.5)

# The approximation

Q <- function(x1,x2){1 + x1 + x1*x2 + x2^2/2}
Q <- outer(x1, x2, FUN = Q)
persp3d(x1, x2, Q, col = "red", add = TRUE, alpha = 0.5)

# The error of the approximation
error <- abs(f - fapprox)

persp3d(x1, x2, error, col = "green", alpha = 0.5)

filled.contour(x1, x2, error)
grid(nx = 20, ny = 20)
