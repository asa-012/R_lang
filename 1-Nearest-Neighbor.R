#ユークリッド距離を求める関数
dist <- function(item1, item2) {
  return(sqrt(sum((item1 - item2) ^ 2)))
}

# 1-NNの関数
Pred_1NN <- function(train_data, test_data, train_label) {
  # 初期化
  length <- 0
  num <- 0
  result <- c()
  #列の数を取得してループ　データ数７５回
  for (i in 1:nrow(test_data)) {
    # 初期値のminが小さいとlength < minで
    # 一番初めのlengthがminより大きくない場合が起きうるからlength < min
    min <- 10000
    #列の数を取得してループ　データ数７５回
    for (j in 1:nrow(train_data)) {
      # それぞれの距離を足す
      length <- dist(test_data[i,], train_data[j,])
      # ユークリッド距離が一番小さい値の教師データの行数を代入
      if (length < min) {
        min <- length
        num <- j
      }
    }
    # 結果のベクトル
    result <- c(result, train_label[num])
  }
  result
}

#データの呼び出しと標準化
data(iris)
niris <- iris
niris[, 1:4] <- scale(niris[, 1:4])
niris$Species <- as.character(niris$Species)
niris$id <- 1:nrow(niris)

#前半を教師，後半をテスト
train_data_1 <- niris[c(1:25, 51:75, 101:125), 1:4] # 教師データセット
test_data_1 <- niris[-c(1:25, 51:75, 101:125), 1:4] # テストデータセット
train_label_1 <- niris[c(1:25, 51:75, 101:125), 5] # 教師データの分類ラベル
test_label_1 <- niris[-c(1:25, 51:75, 101:125), 5] # テストデータの正解ラベル
pred_label_1 <- Pred_1NN(train_data_1, test_data_1, train_label_1)
error_rate_1 <- sum(pred_label_1 != test_label_1) / 75.0

#前半をテスト，後半を教師
train_data_2 <- niris[-c(1:25, 51:75, 101:125), 1:4] # 教師データセット
test_data_2 <- niris[c(1:25, 51:75, 101:125), 1:4] # テストデータセット
train_label_2 <- niris[-c(1:25, 51:75, 101:125), 5] # 教師データの分類ラベル
test_label_2 <- niris[c(1:25, 51:75, 101:125), 5] # テストデータの正解ラベル
pred_label_2 <- Pred_1NN(train_data_2, test_data_2, train_label_2)
error_rate_2 <- sum(pred_label_2 != test_label_2) / 75.0

#誤分類率の平均を求める
avg <- (error_rate_1 + error_rate_2) / 2.0
cat("error rate=", avg, "\n")
