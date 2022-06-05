Tb <- function(beta) {
  function(x, ign = NULL) {
    (beta * x) %% 1
  }
}

Sigma1b <- function(beta) {
  TbI <- Tb(beta)
  n <- 1
  Vcomp <- NULL
  
  while(n < .Machine$integer.max) {
    if (Reduce(f = TbI, x = 1:n, init = 1) <= .Machine$double.eps ** .5) {
      n <- n - 1
      break
    }
    n <- n + 1
  }

  Vcomp <- Reduce(f = TbI, x = 1:n, init = 1, accumulate = TRUE)
  
  function(x) {
    TOSUM <- (c(which(x < Vcomp)) - 1)
    sum((1 / beta) ** TOSUM)
  }
}

Normalise <- function (f) {
  g <- function(x) {sapply(X = x, FUN = f)}
  Constant <- integrate(f = g, lower = 0, upper = 1, rel.tol = .Machine$double.eps ** .5)$value
  function(x) {g(x) / Constant}
}

graphpdf <- function(FF, beta, n = 10000, ...) {
  curve(FF, from = 0, to = 1, n = 10000, 
        ylab = "Probability density function",
        main = expression("Plot of function that defines a measure invariant with respect to " ~ T[beta]),
        ...)
  vectposition <- NULL
  #vectexpression <- NULL
  #k <- 1
  #while(k / beta < 1) {
  #  vectposition <- c(vectposition, k / beta)
  #  vectexpression <- c(vectexpression, 
  #                      eval(substitute(expr = expression(r / beta), 
  #                                      env = list(r = k))))
  #  k <- k + 1
  #}
  #for (v in c(1:k/beta,1)) {
  #  abline(v = v, 
  #       lty = 2, 
  #       col = "navy", 
  #       lwd = 3, 
  #       ...)
  #}
  # I tried to add "discontinuity points but I didn't work :(
}


graphpdf <- function(FF, beta1, n = 10000, ...) {
  curve(FF, from = 0, to = 1, n = 10000, 
        ylab = "Probability density function",
        main = expression("Plot of function that defines a measure invariant with respect to " ~ T[beta]),
        xaxt='n', 
        ...)
  abline(v = (sqrt(5) - 1) / 2, 
         lty = 2, 
         col = "navy", 
         lwd = 3, 
         ...)
  abline(v = 1, 
         lty = 2, 
         col = "navy", 
         lwd = 3,
         ...)
  vectposition <- NULL
  vectexpression <- NULL
  k <- 1
  while(k / beta1 < 1) {
    vectposition <- c(vectposition, k / beta1)
    #vectexpression <- c(vectexpression, as.expression(gsub('r', eval(k), expression(r / beta))) %>% paste0)
    vectexpression <- c(vectexpression, 
                        eval(substitute(expr = expression(r / beta), 
                                        env = list(r = k)))) #paste(k ,"/beta", sep = "") %>% eval()
    k <- k + 1
  }
  axis(side = 1, ...,
       at = c(0, .25, .5, .75, 1, vectposition), 
       labels = as.expression(c(0, .25, .5, .75, 1, vectexpression)))
}
