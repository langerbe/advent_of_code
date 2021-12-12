library(tidyverse)
data <- readLines("~/Desktop/day11.txt")
mat <- matrix(nrow=nchar(data[1]), ncol=length(data))
data <- strsplit(data, split="") %>% unlist()
for (i in 1:length(data)) {
  mat[i] <- data[i]
}

helper <- function(matrix=mat, pos=2, shift=c("1", "2", "-1")) {
  for (i in 1:length(shift)) {
    mat[pos+as.numeric(shift[i])] <- as.character(as.numeric(mat[pos+as.numeric(shift[i])])+1)
  }
  return(mat)
}
flash <- function(f) {
  if (f==1) {
    topright <- c("1", "10", "11")
    helper(mat, pos=f, shift=topright)
  } else if (f %in% 2:9) {
    colone <- c("-1", "1", "9", "10", "11")
    helper(mat, pos=f, shift=colone)
  } else if (f==10) {
    bottomright <- c("-1", "10", "9")
    helper(mat, pos=f, shift=bottomright)
  } else if (f %in% c(11, 21, 31, 41, 51, 61, 71, 81)) {
    rowone <- c("-10", "-9", "1", "10", "11")
    helper(mat, pos=f, shift=rowone)
  } else if (f==91) {
    topleft <- c("-10", "-9", "1")
    helper(mat, pos=f, shift=topleft)
  } else if (f %in% c(20, 30, 40, 50, 60, 70, 80, 90)) {
    rowten <- c("-11", "-10", "-1", "9", "10")
    helper(mat, pos=f, shift=rowten)
  } else if (f==100) {
    bottomleft <- c("-1", "-10", "-11")
    helper(mat, pos=f, shift=bottomleft)
  } else if (f %in% 92:99) {
    colten <- c("-1", "-10", "-11", "-9", "1")
    helper(mat, pos=f, shift=colten)
  } else {
    all <- c("-1", "-10", "-11", "-9", "1", "9", "10", "11")
    helper(mat, pos=f, shift=all)
  }
}
score <- 0

for (x in 1:225) {
  
  step <- x
  if (step==101) {
    print(paste("score after first 100 steps:", score))
  }
  
  #First, the energy level of each octopus increases by 1.
  
  for (i in 1:length(mat)) {
    mat[i] <- as.character(as.numeric(mat[i])+1)
  }
  
  #Then, any octopus with an energy level greater than 9 flashes. 
  # This increases the energy level of all adjacent octopuses by 1, including octopuses that are diagonally adjacent. 
  # If this causes an octopus to have an energy level greater than 9, it also flashes. 
  # This process continues as long as new octopuses keep having their energy level increased beyond 9. 
  # (An octopus can only flash at most once per step.)
  
  if (any(as.numeric(mat[1:100]) > 9)) {
    
    flares <- c()
    
    while (length(start)!=length(end) | is.null(flares)) {
      start <- flares
      for (i in 1:length(mat)) {
        if (i %in% flares) {
          next
        } else if (as.numeric(mat[i])>9) {
          mat <- flash(i)
          #print(paste("flashing octopus:", i))
          score <- score + 1
          flares <- append(flares, i)
        }
      } 
      end <- flares
    }
    
    if (length(end)==100) {
      print(paste("woah! all the octopi flashed on this step:", step))
    }
    
    #Finally, any octopus that flashed during this step has its energy level set to 0, as it used all of its energy to flash.
    
    for (i in 1:length(mat)) {
      if (as.numeric(mat[i])>9) {
        mat[i] <- "0"
      }
    }
    
  }
  
}




