library(data.table)



game <- data.table(m = 0:marbles, p = rep(0,marbles+1), n = rep(0, marbles+1))
game[1, p := 2]
game[1, n := 3]
game[2, p := 3]
game[2, n := 1]
game[3, p := 1]
game[3, n := 2]

marbles <- 7095000
players <- 431

p <- rep(0,marbles)
n <- rep(0,marbles)

p[1:3] <- c(2,3,1)
n[1:3] <- c(3,1,2)

player <- 2
scores <- rep(0, players)
pointer <- 3

i <- 4 
while(i <= marbles+1)  {
  if(i%%1000 == 0) print(i)
  player <- ifelse(player == players, 1, player+1)
  if((i-1) %% 23 != 0) {
    left <- n[pointer]
    right <- n[n[pointer]]
    n[i] <- right
    p[i] <- n[pointer]
    p[right] <- i
    n[left] <- i

    pointer <- i
  } else {
    for(j in seq_len(7)) {
      pointer <- p[pointer]
    }
    scores[player] <- scores[player] + i-1 + pointer-1
    n[p[pointer]] <- n[pointer]
    p[n[pointer]] <- p[pointer]
    pointer <- n[pointer]
  }
  i <- i+1
}
max(scores)

for(i in seq(4, marbles+1)) {
  if(i%%1000 == 0) print(i)
  player <- ifelse(player == players, 1, player+1)
  if((i-1) %% 23 != 0) {
    left <- game[pointer, n]
    right <- game[game[pointer, n], n]
    game[left, n := i]
    game[right, p := i]
    game[i, n := right]
    game[i, p := left]
    pointer <- i
  } else {
    for(j in seq_len(7)) {
      pointer <- game[pointer, p]
    }
    scores[player] <- scores[player] + i-1 + game[pointer, m]
    game[i, test := game[pointer, m]]
    game[game[pointer, p], n := game[pointer, n]]
    game[game[pointer, n], p := game[pointer, p]]
    old_p <- pointer
    pointer <- game[pointer, n]
    game[old_p, c('n', 'p') := -1]
  }
}

max(scores)




print_n <- function(n) {
  p <- 1
  for(i in seq_along(n)) {
    print(p-1)
    p <- n[p]
  }
}
print_game <- function(game) {
  p <- 1
  for(i in seq_len(nrow(game))) {
    print(game[p, m])
    p <- game[p, n]
  }
}
