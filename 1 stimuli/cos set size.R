setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/Word Norms 2/4 analysis")

##Import all values
master <- read.table("all averaged cosine.txt", quote="\"", comment.char="")

##root raw affix old jcn lsa fsg bsg
apply(master[, 3:10], 2, mean, na.rm = T)

master$equal = master$V1==master$V2

smallerset = subset(master, equal == FALSE)

words = as.data.frame(table(master$V1))
write.csv(words, "cos set size.csv")
