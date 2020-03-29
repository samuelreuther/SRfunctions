# Omits observations with values which a not regular (=Not a Regular Value) when
# object is a vector, a factor, a data.frame or a matrix
#
# Author: Rene Locher
# Version: 2005-10-17
# source: https://rdrr.io/cran/IDPmisc/src/R/NaRV.omit.R
#
SR_omit_non_regular_values <- function(x){
  if (is.vector(x)) {
    if (is.numeric(x)) x <- na.omit(x[is.finite(x)]) else
      x <- na.omit(x)
  } else if (is.factor(x)) {
    x <- na.omit(x)
  } else if (is.data.frame(x)) {
    x.num <- as.matrix(x[,sapply(x, is.numeric)])
    ri <- (!apply(x.num, MARGIN = 1,
                  function(x) sum(is.infinite(x)) > 0) &
             !apply(x, MARGIN = 1, function(x) sum(is.na(x)) > 0))
    x <- x[ri,,drop = F]
    ## class omit is incompatible with class data.frame
    ## attributes(x) <- c(attributes(x),list(na.action = which(!ri),class = "omit"))
  } else if (is.matrix(x)) {
    if (is.numeric(x)) {
      ri <- !apply(x, MARGIN = 1,
                   function(x) {sum(is.na(x) | is.infinite(x)) > 0})
      x <- x[ri,,drop = F]
      attributes(x) <- c(attributes(x),list(na.action = which(!ri), class = "omit"))
    } else x <- na.omit(x)
  } else {
    warning(paste0("'x' is neither a vector, nor a factor, nor a data.frame nor a ",
                   "matrix. \n'x' is returned unchanged\n"))
    return(x)
  }
  return(x)
}
