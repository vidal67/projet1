MyData <- read.csv(file="/Users/vidalattias/Desktop/projet 1/hanoi.csv",sep = ";", header = TRUE)
MyData["coups"]

MyData["gen"]

plot(unlist(MyData["gen"]),unlist(MyData["coups"]))


