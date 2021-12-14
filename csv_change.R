rdf <- read.csv(file = "seiseki.csv")
rdf
japanese <- rdf[,"国語"]
japanese
english <- rdf[,"英語"]
math <- rdf[,"数学"]
physics <- rdf[,"物理"]
chemistry <- rdf[,"化学"]