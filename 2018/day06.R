library(data.table)
dt <- fread('input06.txt', sep = ',', colClasses = 'integer')

setnames(dt, c('x_orig','y_orig'))
dt[, id := .I]
minx <- min(dt$x_orig)
miny <- min(dt$y_orig)
maxx <- max(dt$x_orig)
maxy <- max(dt$y_orig)

lenx <- maxx-minx+1
leny <- maxy-miny+1

dt[, x := x_orig-minx+1]
dt[, y := y_orig-miny+1]

calcDistMatrix <- function(x, y, lenx, leny) {
  xdist <- abs(1:lenx-x)
  xmat <- matrix(rep(xdist, leny), leny, lenx, byrow = TRUE)
  ydist <- abs(1:leny-y)
  ymat <- matrix(rep(ydist, lenx), leny, lenx)
  xmat+ymat
}


dt[, distmat := lapply(.I, function(i) {
  calcDistMatrix(dt[i, x], dt[i, y], lenx, leny)
})]

and <- function(a,b) {a&b}

calcAreaMatrix <- function(dt, i) {
  comp <- 1:nrow(dt)
  comp <- comp[-i]
  Reduce(and, lapply(comp, function(j) {dt[i, distmat][[1]] < dt[j, distmat][[1]]}))
}

dt[, areamat := lapply(.I, calcAreaMatrix, dt=dt)]

checkEdges <- function(mat) {
  any(c(mat[1,], mat[,1], mat[leny,], mat[,lenx]))
}

dt[, onEdge := sapply(areamat, checkEdges)]  
dt[, area := sapply(areamat, sum)]

dt[onEdge == FALSE][order(area, decreasing = TRUE)][1, area]


#part2
fsum <- function(a, b) {a+b}
Reduce(sum, dt$distmat)
sum(mat < 10000)

