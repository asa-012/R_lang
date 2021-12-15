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
avgScore <- c(sumScore / 5)
avgScore
#順位を求める　合計点から 点数の大きい順にソート
rank <- rank(sumScore, ties.method = "min")
rank
rank <- c(51 - rank1)
rank
#偏差値=(点数-平均点)*10/標準偏差+50
sumAllSubject <- sum(sumScore)
sumAllSubject

avgAllPeople <- c(sumAllSubject / 50)
avgAllPeople

#標準偏差
sd <- c(sumScore - avgAllPeople)
sd <- sd ^ 2
sd <- sum(sd)
sd <- c(sd / 50)
sd <- c(sqrt(sd))
sd

#偏差値=(点数-平均点)*10/標準偏差+50
devValue <- c(sum - avgAllPeople)
devValue <- c(devValue * 10)
devValue <- c(devValue / sd)
devValue <- c(devValue + 50)

rdf$sum <- sumScore
rdf$avg <- avgAllPeople
rdf$rank <- rank
rdf$devValue <- devValue

# A>65,55<=B<65,55<=C<45,45<=D<35,35>E

judge <- vector(length = 50, mode = "character")
judge[0 <= devValue] <- "E"
judge[35 <= devValue] <- "D"
judge[45 <= devValue] <- "C"
judge[55 <= devValue] <- "B"
judge[65 <= devValue] <- "A"

judge

rdf$judge <- judge

rdf

write.csv(rdf, "result_seiseki", quote = F)

