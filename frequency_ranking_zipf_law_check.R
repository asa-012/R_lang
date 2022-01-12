wordfreq <- function(filename, k) {
  text.row <- readLines(filename) #  テキストファイルを読みこむ
  text.vec <-
    unlist(strsplit(text.row, split = "[[:space:]]|[[:punct:]]")) #  語句ごとに分割
  text.vec <- text.vec[text.vec != ""] #  空文字を消去
  uterms <- unique(text.vec) #  重複を削除
  df <-
    data.frame(term = uterms, freq = numeric(length(uterms))) #  語句と頻度のデータフレームを作成
  for (term in uterms) {
    df[df$term == term, "freq"] <- sum(text.vec == term) #  各語句の頻度を代入
  }
  df <- df[order(-df$freq),] #  頻度の大きい順にソート
  df <- data.frame(rank = c(1:nrow(df)), df)    #  順位をデータフレームに追加
  return(df[1:k,]) # k 行目までのデータフレームを返す
}
plotziplaw <- function(inputfilename, outputfilename, k) {
  pdf(outputfilename)
  df <- wordfreq(inputfilename, k) # wordfreq を呼び出す
  plot(
    log(df$rank),
    log(df$freq),
    axes = F,
    ann = F,
    lty = 0
  ) #  散布図を作成
  par(new = T)
  plot(
    0:log(df$freq[1]),
    log(df$freq[1]):0,
    type = "l",
    col = "red",
    main = outputfilename,
    xlab = "log(rank)",
    ylab = "log(freqency)",
    lty = 1
  ) #  傾き-1 の直線を追加
  legend(
    "topright",
    legend = c("term", "log(freq)=-log(rank)"),
    col = c("black", "red"),
    lty = 0:1,
    pch = c("o", "")
  ) #  凡例を追加
  dev.off()
}
plotziplaw("alice.txt", "zalice.pdf", 1000)
plotziplaw("bohemia.txt", "zbohemia.pdf", 1000)
