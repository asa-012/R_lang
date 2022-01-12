wordfreq <- function(filename, k) {
  text.row <- readLines(filename) # テキストファイルを読みこむ
  text.vec <-
    unlist(strsplit(text.row, split = "[[:space:]]|[[:punct:]]")) # 語句ごとに分割
  text.vec <- text.vec[text.vec != ""] # 空文字を消去
  uterms <- unique(text.vec) # 重複を削除
  df <-
    data.frame(term = uterms, freq = numeric(length(uterms))) # 語句と頻度のデータフレームを作成
  for (term in uterms) {
    df[df$term == term, "freq"] <- sum(text.vec == term) # 各語句の頻度を代入
  }
  df <- df[order(-df$freq),] # 頻度の大きい順にソート
  df <- data.frame(rank = c(1:nrow(df)), df) # 順位をデータフレームに追加
  return(df[1:k,]) # k行目までのデータフレームを返す
}
talice <- wordfreq("alice.txt", 20)
tbohemia <- wordfreq("bohemia.txt", 20)

#出力
talice
tbohemia

#調査
#wakati192<-wordfreq("192_wakati.txt",1000) 
#wakati187<-wordfreq("187_wakati.txt",1000) 
#wakati185<-wordfreq("185_wakati.txt",1000) 
#wakati183<-wordfreq("183_wakati.txt",1000) 

