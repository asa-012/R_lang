rdf <- read.csv(file = "seiseki.csv")
rdf
japanese <- rdf[, "国語"]
english <- rdf[, "英語"]
math <- rdf[, "数学"]
physics <- rdf[, "物理"]
chemistry <- rdf[, "化学"]
#5教科合計点
sumScore <- c(japanese + english + math + physics + chemistry)
sumScore
#１人１人の平均点
avgScore <- c(sum / 5)
avgScore
#順位を求める　合計点から 点数の大きい順にソート
rank1 <- rank(sumScore,ties.method = "min")
rank1
rank2 <- c(51-rank1)
rank2
#偏差値=(点数-平均点)*10/標準偏差+50
avgallPeople <-c(sum(sumScore)/50)
avgallPeople

# A>65,55<=B<65,55<=C<45,45<=D<35,35>E