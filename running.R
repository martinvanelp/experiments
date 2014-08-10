# simple lap speed calculator

dist <- c(0, 3.34, 1.84, 1.72)
time <- c(0, 19 + 4/60, 10 + 55/60, 10 + 26/60)

dist <- round(dist, digits = 2)
time <- round(time, digits = 2)

dist[1] <- sum(dist)
time[1] <- sum(time)

speed <- round(dist / time * 60, digits = 2)

table <- cbind(dist, time, speed)

print(table)

##
## archive
##
#  2014/08/10
#  dist <- c(0, 3.34, 1.84, 1.72)
#  time <- c(0, 19 + 4/60, 10 + 55/60, 10 + 26/60)