#ユークリッド距離を求める関数
dist <- function(item1, item2) {
  originalDist -> sqrt(sum((item1 - item2) ^ 2))
}

# 1-NNの関数
Pred_1NN <- function(train_data, test_data, train_label) {
  len <- 0
  num <- 0
  y <- c()
  for (i in 1:nrow(test_data)) {
    min <- 10000
    for (j in 1:nrow(train_data)) {
      len <- dist(test_data[i,], train_data[j,])
      if (len < min) {
        min <- len
        num <- j
      }
    }
    y <- c(y, train_label[num])
  }
  y
}

#データの呼び出しと標準化
data(iris)
niris <- iris
niris[, 1:4] <- scale(niris[, 1:4])
niris$Species <- as.character(niris$Species)
niris$id <- 1:nrow(niris)