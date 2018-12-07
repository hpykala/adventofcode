library(data.table)
input <- readLines('input07.txt')
dt <- data.table(edge = gsub('Step ([A-Z]) must be finished before step ([A-Z]) can begin.', '\\1-\\2', input))
dt[, c('start', 'end') := transpose(strsplit(edge, '-'))]

uniqueLetters <- unique(c(dt[,start],dt[, end]))

res <- NULL
findfirst <- function(dt, res) {
  let <- min(uniqueLetters[!uniqueLetters %in% c(res, dt$end)])
  res <- c(res, let)
  dt <- dt[start != let]
  if(nrow(dt) == 0) {
    res <- c(res, sort(uniqueLetters[!uniqueLetters %in% res]))
    return(res)
  } else{ 
    findfirst(dt, res)
}
}
res <- findfirst(dt, res)
paste0(res, collapse = '')


#part2
wtime <- function(letter) {
  60+which(LETTERS == letter)
}

availableNodes <- function(doneTasks, notFinished) {
  available <- uniqueLetters[!uniqueLetters %in% dt[!start %in% doneTasks ,end]]
  sort(available[!available %in% c(doneTasks, notFinished) ])
}

doneTasks <- NULL
while(length(doneTasks) < length(uniqueLetters)) {
  doneTasks <- c(doneTasks, min(availableNodes(doneTasks, character(0))))
}

addTasks <- function(doing, done, t) {
  available <- availableNodes(done$task, doing$task)
  if(length(available) == 0) {
    return(doing)
  }
  new <- data.table(task = available, start_time = t)
  new[, end_time := start_time + wtime(task), by = task]
  
  rbind(doing, new[1:min(nrow(new), 5-nrow(doing))])
}

finishTasks <- function(doing, done) {
  t <- min(doing[, end_time])
  done <- rbind(done, doing[end_time == t])
  doing <- doing[!task %in% done$task]
  return(list(t = t, done = done, doing = doing))
}

t <-0
doing <- data.table(task = character(0), start_time = integer(0), end_time =integer(0))
done <- copy(doing)
l <- list(t = t, done = done, doing = doing)


while(nrow(l$done) < length(uniqueLetters)) {
l$doing <- addTasks(l$doing, l$done, l$t)
print(nrow(l$doing))
l <- finishTasks(l$doing, l$done)
}

l$done[, max(end_time)] 
