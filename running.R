# simple lap speed calculator

dist <- c(0, 3.34, 1.84, 1.72)
time <- c(0, 18 + 04/60, 10 + 16/60, 9 + 39/60)

dist <- round(dist, digits = 2)
time <- round(time, digits = 2)

dist[1] <- sum(dist)
time[1] <- sum(time)

speed <- round(dist / time * 60, digits = 2)

table <- cbind(dist, time, speed)

print(table)

##
## ARCHIVE
##
## 2014/08/13
#  dist <- c(0, 3.34, 1.84, 1.72)
#  time <- c(0, 18 + 04/60, 10 + 16/60, 9 + 39/60)
#
## 2014/10/22
#  dist <- c(0, 3.34, 1.96, 1.60)
#  time <- c(0, 17 + 50/60, 10 + 24/60, 8 + 48/60)
#
## 2014/08/27
#  dist <- c(0, 3.34, 4.44, 1.27)
#  time <- c(0, 19 + 10/60, 27 + 20/60, 7 + 48/60)
#
## 2014/08/23
#  dist <- c(0, 3.34, 4.44, 1.27)
#  time <- c(0, 19 + 48/60, 27 + 54/60, 7 + 39/60)
#
## 2014/08/13
#  dist <- c(0, 3.34, 1.84, 1.72)
#  time <- c(0, 19 + 44/60, 10 + 56/60, 10 + 16/60)
#
## 2014/08/10
#  dist <- c(0, 3.34, 1.84, 1.72)
#  time <- c(0, 19 + 4/60, 10 + 55/60, 10 + 26/60)
#