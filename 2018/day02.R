library(data.table)

dt <- data.table(input = readLines('input02.txt'))
dt[, sp  := strsplit(input, '')]

counts <- function(sp) {
  cdt <- data.table(code = sp)
  gr <- cdt[, .(.N), by = code]
  return(c(2 %in% gr$N, 3 %in% gr$N))
}

dt[, c('twos', 'threes') := transpose(lapply(sp, counts))]
sum(dt$twos)*sum(dt$threes)

#part2
dists <- adist(dt$input, dt$input, costs = c(Inf,Inf,1))
inds <- which(dists == 1, arr.ind = TRUE)
sp <- dt[inds[1,], sp]
shared_letters <- mapply(function(x,y) {x == y}, x = sp[[1]], y = sp[[2]])
paste0(sp[[1]][shared_letters], collapse = '')
