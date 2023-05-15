## ML distances
library(reshape2)
library(ggplot2)
library(ggbeeswarm)

disttable <- read.table("8Eve_1Gfa.fa.GTR.mldist", skip = 1)
disttable <- disttable[-1, -2]
names(disttable)[-1] <- disttable[,1]
disttable_long <- melt(disttable)
disttable_long$first <- sapply(disttable_long$V1, function(x) strsplit(x, split="|")[[1]][1])
hist(disttable_long$value, breaks = 50, main = "E. verrucosus", xlab = "Patristic distance", las=1)
abline(v=0.16, lty=3, col="blue")

disttable <- read.table("15Gfa_1Eve.fa.GTR.mldist", skip = 1)
disttable <- disttable[-1, -2]
names(disttable)[-1] <- disttable[,1]
disttable_long <- melt(disttable)
disttable_long$first <- sapply(disttable_long$V1, function(x) strsplit(x, split="|")[[1]][1])
hist(disttable_long$value, breaks = 50, main = "G. fasciatus", xlab = "Patristic distance", las=1)
abline(v=0.16)

