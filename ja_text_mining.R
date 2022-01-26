#Q0 docMatrix 関数で boj フォルダからターム・文書行列を作成する
#プログラムを作れ ただし品詞は名詞のみで，重みは“ tf idf *norm とする

bojDF0 <- docMatrix("boj", pos = c(" 名詞 "), weight = "tf*idf*norm")
bojDF1 <- makeRNDF(bojDF0)

#Q1
#Q2
#Q3
#Q4
#Q5

