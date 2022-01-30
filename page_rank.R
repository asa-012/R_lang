# 内容語の抽出
f <- 5
sarukaniDF <-
  docDF("sarukani_gassen-utf8.txt",
        type = 1,
        pos = c("名詞", "形容詞", "動詞"))
# 降順(行番号大→小)　４列目を並び替えたもの
a <- sarukaniDF[order(sarukaniDF[, 4], decreasing = TRUE),]
# f以上のもののみを抽出しbに代入
b <- a[a$"sarukani_gassen-utf8.txt" >= f,]
c <-
  b[(b$POS2 != "非自立") &
      (b$POS2 != "接尾") & (b$POS2 != "特殊") & (b$POS2 != "代名詞"),]
termRow <- c$TERM
resultExtractionTable <-
  matrix(0, nrow = length(termRow), ncol = length(termRow))
# 共起頻度の作成 collocateのspan=3と最小頻度f=5とする
for (i in 1:length(termRow)) {
  co_occurrence_frequency <-
    collocate("sarukani_gassen-utf8.txt",
              node = termRow[i],
              span = 3)
  row <- length(rownames(co_occurrence_frequency))
  for (j in 1:length(termRow)) {
    for (k in 1:(row - 2)) {
      if (termRow[j] == co_occurrence_frequency$Term[k]) {
        resultExtractionTable[i, j] <- co_occurrence_frequency$Span[k] break
      }
    }
  }
}
# 表の斜めを０にする
for (i in 1:length(termRow)) {
  P1[i, i] <- 0
}
colnames(P1) <- c(termRow)
rownames(P1) <- c(termRow)
# 表の正規化
h <- 0
for (i in 1:length(termRow)) {
  if (sum(P1[, i]) == 0) {
    h <- i
  }
}
P2 <- P1[-h,-h]
# 確率行列化
for (i in 1:ncol(P2)) {
  for (j in 1:nrow(P2)) {
    if (P2[i, j] != 0) {
      P2[i, j] <-1
    }
  }
}
sumcol <- c()
#各列の列和をsumcolに保存
for (i in 1:ncol(P2)) {
  sumcol <- c(sumcol, sum(P2[, i]))
}
for (i in 1:nrow(P2)) {
  for (j in 1:ncol(P2)) {
    if (P2[i, j] != 0) {
      P2[i, j] <- P2[i, j] / sumcol[j]
    }
  }
}

# PageRank計算