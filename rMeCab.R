# 以下をR consoleで入力　RStudioではない
#install.packages("RMeCab", repos = "https://rmecab.jp/R") 

#library(RMeCab)
#merosuDF[merosuDF$TERM == "結婚式",]

#res <- RMeCabFreq("wagahai.txt")
#res

associateWord <- function(filename,word){
  res <- collocate(filename,node=word,span=3)
  res <- collScores(res,node=word,span=3)
  res<-na.omit(res)
  res<-res[res$T>1.65,]
  res<-res[res$MI>1.58,]
  return(res$Term)
}
#Q0
wineDF <- docDF("wine_utf8.txt",type=1,pos=c("名詞","形容詞","動詞","助動詞"))

#Q1
wineDF1 <- wineDF[rev(order(wineDF$wine_utf8.txt)),]

#Q2
wineDF2 <- wineDF1[wineDF1$wine_utf8.txt>=15,]

#Q3
wineDF3 <- wineDF2[!(wineDF2$POS2 %in% c("非自立","数")),]

#Q4
wineDF4 <- wineDF3$TERM

#Q5
for(word in wineDF4){
  cat(word,"\n")
  cat("associate words >> ",associateWord("wine_utf8.txt",word),"\n\n")
}

