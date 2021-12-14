x <-
  c(55,
    73,
    64,
    63,
    65,
    57,
    59,
    85,
    54,
    55,
    56,
    43,
    60,
    58,
    62,
    58,
    59,
    63,
    61,
    64,
    20,
    89,
    90,
    5,
    10)
quantile(x)
qSample <- quantile(x)
q1 <- qSample["25%"]
q3 <- qSample["75%"]
iqr <- q3 - q1
condQ1 <- q1 - 1.5 * iqr
condQ3 <- q3 + 1.5 * iqr
outlierBool <- (x > condQ3) | (x < condQ1)
Data <- x[outlierBool]
Index <- which(outlierBool)
outlier <- data.frame(index = Index, data = Data)
outlier