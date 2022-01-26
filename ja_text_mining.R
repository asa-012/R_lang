#1-Q0 docMatrix 関数で boj フォルダからターム・文書行列を作成 名詞のみ　重みは“ tf idf *norm

bojDF0 <- docMatrix("boj", pos = c(" 名詞 "), weight = "tf*idf*norm")
bojDF1 <- makeRNDF(bojDF0)

#1-Q1: 株価の表をもとに要素数９の株価ベクトルを作成するプログラム
AVG <-
  c(17225, 17287, 18138, 16785, 15307, 12525, 13481, 11259, 8859)

#1-Q2:　Q0で作ったターム・文書行列は各タームにつき9つの頻度を持つ.
#各タームごとの 9 つの 要素を一つのベクトルとみなし，
#Q1で作った株価ベクトルとの相関係数を求めるプログラム（相関係数はベクトル）
len = nrow(bojDF1)
res <- c()
for (x in 1:len) {
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
print(res3[len,])

#2-Q0: docMatrix()関数で”boj”フォルダからターム・文書行列を作成
#品詞は名詞のみ 重みは“ tf”
bojDF02 <- docMatrix("boj", pos = c("名詞"), weight = "tf")
bojDF2 <- makeRNDF(bojDF02)


#2-Q1: データフレームDFのK番目の列を頻度で並び替えその頻度順で
#上位ｒ個の頻出単語をベクトルで返す関数freqNoun(DF,K,r)を作成
freqNoun <-
  function(DF, K, X) {
    sortedDF <- DF[order(-DF[, K]),] return(rownames(sortedDF[1:X,]))
  }

#2-Q2: N列あるデータフレームDFの各列に対しfreqNoun()関数で
#r個の頻出単語を求めるとN×r個の単語が見つかる.
#それに対し、重複を除いた単語をベクトルで返す関数
#highFreqNoun(DF,r)の作成.


#2-Q3: Q2で作成した関数highFreqNoun()に入力データフレームとして
#Q0で作成したデータフレームを、頻出語の数として
#５を与えた時の出力単語を求める.

#2-Q4: 課題１のQ4のデータフレームの対し、
#課題2のQ3で得られた頻出語だけを残すプログラムを作成.

#2-Q5: 頻出語を相関係数でグループ分けし表示するプログラムを作成.
# グループ１：相関係数が+0.5以上
# グループ２：相関係数が-0.5～+0.5の間
# グループ３：相関係数が-0.5以下
