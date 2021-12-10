library(tidyverse)
lines <- readLines("~/Documents/day10.txt")

reader <- function(line=NULL) {
  line %>% gsub("<>", "", ., fixed=T) %>% gsub("{}", "", ., fixed=T) %>% gsub("[]", "", ., fixed=T) %>% gsub("()", "", ., fixed=T)
}

incomplete <- numeric()
corrupt <- numeric()

for (i in 1:length(lines)) {
  line <- lines[i]
  idx <- i
  while (!(is.na(str_extract(line, pattern="\\{\\}|\\[\\]|\\(\\)|\\<\\>")))) {
    line <- reader(line)
  }
  lines[i] <- line
  if (!is.na(str_extract(line, pattern="\\}|\\]|\\)|\\>"))) {
    corrupt <- append(corrupt, idx)
  } else {
    incomplete <- append(incomplete, idx)
  }
}

corrupt <- lines[corrupt]
points <- tibble(closing=c(")", "]", "}", ">"), points=c(3, 57, 1197, 25137))
closing <- data.frame(closing=NA, index=1:length(corrupt))

for (i in 1:length(corrupt)) {
  string <- corrupt[i]
  idx <- i
  for (n in 1:length(strsplit(string, split="")[[1]])) {
    char <- strsplit(string, split="")[[1]][n]
    if (char %in% points$closing) {
      closing[which(closing$index==idx), 1] <- char
      break
    }
  }
}

counts <- table(closing$closing)
counts <- as_tibble(as.data.frame(counts) %>% rename(n=Freq, closing=Var1))
counts <- merge(counts, points, by="closing") %>% mutate(total=n*points)
sum(counts$total)

# part 2

incomplete <- lines[incomplete]
completion_string <- function(line=NULL) {
  line %>% gsub("<", ">", ., fixed=T) %>% gsub("{", "}", ., fixed=T) %>% gsub("[", "]", ., fixed=T) %>% gsub("(", ")", ., fixed=T)
}
strings <- incomplete %>% lapply(FUN=completion_string) %>% unlist()
points$completion <- 1:4

score_func <- function(completion_string=NULL) {
  completion_string <- stringi::stri_reverse(completion_string)
  score <- as.numeric(points[which(points$closing==((strsplit(completion_string, split="") %>% unlist())[1])),3])
  for (n in 2:length(strsplit(completion_string, split="")[[1]])) {
    char <- strsplit(completion_string, split="")[[1]][n]
    value <- as.numeric(points[which(points$closing==char),3])
    score <- (score*5) + value
  }
  return(score)
}

output <- strings %>% lapply(FUN=score_func) %>% unlist()
sort(output) %>% median