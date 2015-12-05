library(dplyr)

tday    <- read.table("tday.delim", header=TRUE)
puzzleL <- read.table("puzzle_nov25_2015.delim", header=TRUE)
puzzleH <- read.table("puzzlehard_nov25_2015.delim", header=TRUE)
puzzleX <- read.table("puzzleMvE_dec04_2015.delim", header=TRUE)

# choose puzzle to solve
puzzle0 <- puzzleX

# FUNCTIONS
LtoN <- function(table) {
        for(i in 1:ncol(table)) {
                table[,i] <- match(table[,i], letters)
        }
        table
}

identity <- function(table, target=FALSE) {
        x <- nrow(table)
        if(target==FALSE) { y <- ncol(table) }
        if(target==TRUE)  { y <- 1 }
        
        cols <- as.data.frame(matrix(0, 2, ncol(table)))
        rows <- as.data.frame(matrix(0, x, y))
        
        for(i in 1:x) {
                for(j in 1:y) {
                        if(target==FALSE) { rows[i,j] <- sd(table[i,-j]) }
                        if(target==TRUE)  { rows[i,j] <- sd(table[i,]) }
                }
        }
        
        for(i in 1:ncol(table)) {
                cols[1,i] <- sd(table[,i])
                cols[2,i] <- sum(table[,i])
        }        
        
        output <- list(rows = rows, cols = cols)
        output
}

ordering <- function(puzzle, blueprint, cols=TRUE) {
        output <- vector()
        
        for(i in 1:length(blueprint)) {
                for(j in 1:length(puzzle)) {
                        if(blueprint[i] == puzzle[j] & cols==TRUE) {
                                output[i] <- j
                        }
                        if(blueprint[j] == puzzle[i] & cols==FALSE) {
                                output[i] <- j
                        }
                }
        }
        output
}

# ANALYSIS AND SOLVER
target    <- tday
blueprint <- identity(LtoN(target), target=TRUE)

puzzle    <- puzzle0
puzzleID  <- identity(LtoN(puzzle))

# push a column?
for(i in 1:length(puzzleID$cols[2,])) {
        for(j in 1:length(blueprint$cols[2,])) {
                if(puzzleID$cols[1,i] == blueprint$cols[1,j]) {
                        diff <- puzzleID$cols[2,i] - blueprint$cols[2,j]
                        push <- diff / nrow(puzzle)
                        
                        puzzle[i] <- letters[match(puzzle[,i], letters) - push]
                }
        }
}

puzzleID  <- identity(LtoN(puzzle))

# drop which column?
for (i in 1:ncol(puzzle)) {
        if (sum(puzzleID$rows[,i] %in% blueprint$rows[,]) 
            == nrow(puzzle)) {
                puzzle <- puzzle[,-i]
                break
        }
}

puzzleID <- identity(LtoN(puzzle), target=TRUE)

# determine right row and col order
rowOrder <- ordering(puzzleID$rows[,1], blueprint$rows[,1], cols=FALSE)
colOrder <- ordering(puzzleID$cols[1,], blueprint$cols[1,], cols=TRUE)

# apply select to get right order
puzzle <- arrange(select(puzzle, colOrder), rowOrder)

print("Original")
puzzle0

print("Solution")
puzzle
