wordfreq <- function(filename, k) {
  filedata <- readLines(filename) #ファイルを読み込む
  wordList <-
    unlist(strsplit(filedata, split = "[[:space:]]|[[:punct:]]")) #空白or特殊記号で分割
  wordListAfterDeletingSpace <-
    wordList[wordList != ""] # 1文字ずつ確認し空文字を削除
  noDuplicateList <- unique(wordListAfterDeletingSpace) # 重複のないリストに作り替える
  df <- data.frame(term = noDuplicateList, freq = numeric(length(noDuplicateList)))# 語句と頻度のデータフレームを作成
  for (noDuplicateWord in noDuplicateList) {
    df[df$term == noDuplicateWord, "freq"] <- sum(wordListAfterDeletingSpace == noDuplicateWord) # 各語句の頻度を代入
  }
  df <- df[order(-df$freq), ]#orderで並び替え
  df <- data.frame(rank = c(1:ｋ),df[c(1:k)], df) # rankに1からk(最大値)の数字を入れdf作成
  return(df[1:k, ]) # k行目までのデータフレームを返す
}

plotziplaw <- function(inputfilename, outputfilename, k) {
  pdf(outputfilename)
  df <- wordfreq(inputfilename, k)
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
