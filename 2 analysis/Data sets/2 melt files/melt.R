##getting first block
block1 = read.csv("block1 part 2.csv")
library(reshape)

longdata = melt(block1,
                id = c("Partno", "Judgment"))
                
View(longdata)

colnames(longdata)[3] = "Word Pair"
colnames(longdata)[4] = "Judged Value"

write.csv(longdata, "block1 part2 melted.csv")

##recall for first block
block1_recall = read.csv("block1 recall part 2.csv")

longdata2 = melt(block1_recall,
                 id = "Partno")
View(longdata2)

colnames(longdata2)[2] = "Recall Pair"
colnames(longdata2)[3] = "Recall"

write.csv(longdata2, "block1 recall melted.csv")

##second block
block2 = read.csv("block2 part 2.csv")
View(block2)

longdata3 = melt(block2,
                id = c("Partno", "Judgment2"))

View(longdata3)

colnames(longdata3)[3] = "Word Pair"
colnames(longdata3)[4] = "Judged Value"

write.csv(longdata3, "block2 part 2 melted.csv")

##block2 recall
block2_recall = read.csv("block2 recall part 2.csv")

longdata4 = melt(block2_recall,
                 id = "Partno")
View(longdata4)

colnames(longdata4)[2] = "Recall Pair"
colnames(longdata4)[3] = "Recall"

write.csv(longdata4, "block2 recall part 2 melted.csv")

##block3
block3 = read.csv("block3 part 2.csv")
View(block3)

longdata5 = melt(block3,
                 id = c("Partno", "Judgment3"))

View(longdata5)

colnames(longdata5)[3] = "Word Pair"
colnames(longdata5)[4] = "Judged Value"

write.csv(longdata5, "block3 part 2 melted.csv")

##block3 recall
block3_recall = read.csv("block3 recall part 2.csv")

longdata6 = melt(block3_recall,
                 id = "Partno")
View(longdata6)

colnames(longdata6)[2] = "Recall Pair"
colnames(longdata6)[3] = "Recall"

write.csv(longdata6, "block3 recall part 2 melted.csv")
