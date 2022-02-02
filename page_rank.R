# 1.内容語の抽出
f <- 5
sarukaniDF <-
  docDF("sarukani_gassen-utf8.txt",
        type = 1,
        pos = c("名詞", "形容詞", "動詞"))
# 降順(行番号大→小)　４列目を並び替えたもの
a <- sarukaniDF[order(sarukaniDF[, 4], decreasing = TRUE),]
# f以上のもののみを抽出しbに代入
b <- a[a$"sarukani_gassen-utf8.txt" >= f,]
# 非自立、接尾、特殊、代名詞を除く
termList <-
  b[(b$POS2 != "非自立") &
      (b$POS2 != "接尾") & (b$POS2 != "特殊") & (b$POS2 != "代名詞"),]
#TERM列を抽出
termRow <- termList$TERM
resultExtractionTable <-
  matrix(0, nrow = length(termRow), ncol = length(termRow))
# 2.共起頻度の作成 collocateのspan=3と最小頻度f=5とする
for (i in 1:length(termRow)) {
  co_occurrence_frequency <-
    collocate("sarukani_gassen-utf8.txt",
              node = termRow[i],
              span = 3)
  row <- length(rownames(co_occurrence_frequency))
  for (j in 1:length(termRow)) {
    for (k in 1:(row - 2)) {
      # i,jの後だからrow-2している
      if (termRow[j] == co_occurrence_frequency$Term[k]) {
        resultExtractionTable[i, j] <- co_occurrence_frequency$Span[k]
        break
      }
    }
  }
  # 表の斜めを０にする
  resultExtractionTable[i, i] <- 0
}
colnames(resultExtractionTable) <- c(termRow)
rownames(resultExtractionTable) <- c(termRow)
# 3.頻度行列の正規化(Step1)
h <- 0
for (i in 1:length(termRow)) {
  if (sum(resultExtractionTable[, i]) == 0) {
    h <- i
  }
}
# 頻度行列の正規化(Step2)
newResultExtractionTable <- resultExtractionTable[-h,-h]
# 確率行列化(Step1)
for (i in 1:ncol(newResultExtractionTable)) {
  for (j in 1:nrow(newResultExtractionTable)) {
    if (newResultExtractionTable[i, j] != 0) {
      newResultExtractionTable[i, j] <- 1
    }
  }
}
# 4.確率行列化(Step2)
sumcol <- c()
#各列の列和をsumcolに保存
for (i in 1:ncol(newResultExtractionTable)) {
  sumcol <- c(sumcol, sum(newResultExtractionTable[, i]))
}
for (i in 1:nrow(newResultExtractionTable)) {
  for (j in 1:ncol(newResultExtractionTable)) {
    if (newResultExtractionTable[i, j] != 0) {
      newResultExtractionTable[i, j] <-
        newResultExtractionTable[i, j] / sumcol[j]
    }
  }
}

# 5.PageRank計算
alfa <- 0.85
n <- nrow(newResultExtractionTable)
ip <- 10 ^ -4
u <- matrix(1 / n, nrow = n, ncol = 1)
u0 <- matrix(1 / n, nrow = n, ncol = 1)
while (TRUE) {
  u1 <- alfa * newResultExtractionTable %*% u0 + (1 - alfa) * u
  if (norm(u1 - u0) < ip) {
    break
  }
  u0 <- u1
}
PageRank1 <- u1
#順序変更　降順
pageRankRow <- rownames(PageRank1)
term <- pageRankRow[order(PageRank1, decreasing = TRUE)]

pageRankResult <- PageRank1[order(PageRank1, decreasing = TRUE)]
pageRankResult <- data.frame(term, pageRankResult)
pageRankResult