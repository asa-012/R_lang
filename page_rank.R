# 内容語の抽出

tableAfterExtraction <- function(){
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
d <- c$TERM
result <-matrix(0, nrow = length(d), ncol = length(d))
return(result)
}
table_aftere_extraction <- tableAfterExtraction(f)
# 共起頻度の作成 collocateのspan=3と最小頻度f=5とする
# 表の正規化
# 確率行列化
# PageRank計算