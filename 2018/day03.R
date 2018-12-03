library(data.table)
data <- data.table(input = readLines('input03.txt'))
data[, coords := gsub('#[0-9]+ @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)', '\\1-\\2-\\3-\\4',x = input)]
data[, c('xstart', 'ystart', 'xlen', 'ylen') := transpose(strsplit(coords, '-'))]
data[, x1 := as.integer(xstart)+1]
data[, y1 := as.integer(ystart)+1]
data[, x2 := x1+as.integer(xlen)-1]
data[, y2 := y1+as.integer(ylen)-1]

cloth <- matrix(0, max(data$y2), max(data$x2))

for(i in seq_len(nrow(data))) {
  cloth[data[i, y1]:data[i, y2], data[i, x1]:data[i, x2]] <- 
    cloth[data[i, y1]:data[i, y2], data[i, x1]:data[i, x2]] + 1
}
sum(cloth > 1)

#part2
for(i in seq_len(nrow(data))) {
  if(max(cloth[data[i, y1]:data[i, y2], data[i, x1]:data[i, x2]])==1) {
    print(i)
  }
}
