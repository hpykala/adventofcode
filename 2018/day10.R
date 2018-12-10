library(data.table)
dt <- data.table(input = readLines('input10.txt'))
dt[, sub := gsub('.*< *(-?[0-9]+), *(-?[0-9]+).*< *(-?[0-9]+), *(-?[0-9]+).*', '\\1 \\2 \\3 \\4', input)]
dt[, c('x', 'y', 'vx', 'vy') := transpose(strsplit(sub, ' '))]

pos0 <- data.table(x = as.integer(dt$x), y = as.integer(dt$y))
vel <- data.table(x = as.integer(dt$vx), y = as.integer(dt$vy))

bound <- function(d) {
  max(d$y)-min(d$y)
} 

pos <- copy(pos0)
b0 <- bound(pos0)
b1 <- bound(pos0)
i <- 0
step <- 100
while(b1 <= b0) {
  i <- i+1
  b0 <- b1
  pos <- pos0 + i*step*vel
  b1 <- bound(pos)
}

start <- (i-1)*step
pos <- pos0+start*vel
b0 <- bound(pos)
b1 <- bound(pos)
i <- 0

while(b1 <= b0) {
  b0 <- b1
  pos <- pos + vel
  b1 <- bound(pos)
  print(c(i, b0, b1))
  i <- i+1
}

index <- start+i-1

pic <- pos0+index*vel
minx <- min(pic$x)
miny <- min(pic$y)
maxx <- max(pic$x)
maxy <- max(pic$y)
sky <- matrix('', maxy-miny+1, maxx-minx+1)

for(i in seq_len(nrow(pic))) {
  sky[pic[i,y]-miny+1, pic[i,x]-minx+1] <- 'X'
}

View(sky)
