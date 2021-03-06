#1-Q0 docMatrix 関数で boj フォルダからターム・文書行列を作成 名詞のみ　重みは“ tf idf *norm
bojDF0 <-
  docMatrix("boj", pos = c(" 名詞 "), weight = "tf*idf*norm")
bojDF1 <- makeRNDF(bojDF0)

#1-Q1: 株価の表をもとに要素数９の株価ベクトルを作成するプログラム
AVG <-
  c(17225, 17287, 18138, 16785, 15307, 12525, 13481, 11259, 8859)

#1-Q2:　Q0で作ったターム・文書行列は各タームにつき9つの頻度を持つ.
#各タームごとの 9 つの 要素を一つのベクトルとみなし，
#Q1で作った株価ベクトルとの相関係数を求めるプログラム（相関係数はベクトル）
bojDF1Length = nrow(bojDF1)
res <- c()
for (x in 1:bojDF1Length) {
  res <- c(res, cor(bojDF1[x,], AVG))
}

#1-Q3:　Q2で求めた各タームに対する相関係数ベクトルから，
#タームと相関係数からなるデータフレームを作成するプログラム
res1 <- data.frame("term" = rownames(bojDF1), "SOUKAN" = res)

#1-Q4: Q3で作成したタームと相関係数のデータフレームに対し，
#相関係数の降順(大きい順)に 並び変える
res2 <- rev(order(res))
res3 <- res1[res2,]

#1-Q5: Q4のデータフレームをもとに相関係数が最も高いターム
#とその相関係数，最も小さいタームを表示
print(res3[1,])
(res3[bojDF1Length,])