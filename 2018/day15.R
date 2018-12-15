library(data.table)
input <- readLines('input15.txt')
rows <- length(input)
cols <- nchar(input[1])

map <- matrix(unlist(strsplit(input, '')), rows, cols, byrow = T)

types <- c('G', 'E')
coords <- which(map %in% types)
creatures <- as.data.table(arrayInd(coords, dim(map), useNames = TRUE))
creatures[, type := map[coords]]
creatures[, ind := coords]
creatures[, id := .I]
creatures[, AP := 3]
creatures[, HP := 200]

adj <- c(-1, 1, -cols, cols)

adjacentEnemies <- function(creatures, curr_id) {
  creatures[ind %in% creatures[curr_id == id, ind+adj]][type != creatures[id == curr_id, type]]
}

adjacentCells <- function(arr_ind) {
  unique(Reduce(c, sapply(arr_ind, function(i) {i + adj})))
}

minIndex <- function(arr_ind, map) {
  dt <- data.table(arrayInd(arr_ind, dim(map), useNames = T))
  dt[, arr_ind := arr_ind]
  dt[order(row, col), arr_ind][1]
}

moveTarget <- function(map, start_ind) {
  search_map <- copy(map)
  targets <- integer(0)
  search_map[start_ind] <- 0
  ge <- setdiff(types,map[start_ind])
  i <- 0
  while(T) {
    nc <- adjacentCells(which(search_map == i))
    enemies <- nc[which(ge == search_map[nc])]
    if(length(enemies) > 0) {
      targets <- intersect(adjacentCells(enemies), which(search_map == i))
      break
    }
    empty <- nc[search_map[nc] == '.']
    if(length(empty) == 0) {
      break
    }
    i <- i+1
    search_map[empty] <- i
  }
  
  if(length(targets > 0)) {
    target <- minIndex(targets, search_map)
    
    while(i > 1) {
      i <- i-1
      nc <- adjacentCells(target)
      target <- nc[which(search_map[nc] == i)]
    }
    return(minIndex(target, search_map))
  } else {
    return(start_ind)
  }
  
}

printMap<- function(map, creatures) {
  cat(paste0(sapply(seq_len(rows), function(i) {
    paste(paste0(map[i,], collapse = ''), creatures[row == i][order(col), paste(HP, collapse = ' ')])
  }), collapse = '\n'))
}

round <- 0
printMap(map,creatures)

#Main loop
while(T) {
  moveorder <- creatures[order(row,col), id]
  for(curr_id in moveorder) {
    if(!curr_id %in% creatures$id) {
      next
    }
    
    if(length(unique(creatures$type)) == 1) {
      print(round)
      stop()
    }
    
    enemies <- adjacentEnemies(creatures, curr_id) 
    if(nrow(enemies) == 0) {
      move_target <- moveTarget(map, creatures[id == curr_id, ind])
      cr <- map[creatures[id == curr_id, ind]]
      map[creatures[id == curr_id, ind]] <- '.'
      map[move_target] <- cr
      creatures[id == curr_id, c('row', 'col') := as.data.table(arrayInd(move_target, dim(map), useNames = T))]
      creatures[id == curr_id, ind := move_target]
    }
    enemies <- adjacentEnemies(creatures, curr_id) 
    
    if(nrow(enemies) > 0) {
      enemy_id <- enemies[order(HP,row,col), id][1]
      creatures[id == enemy_id, HP := HP-creatures[id == curr_id,AP]]
      if(creatures[id == enemy_id, HP] < 1) {
        print(creatures[id == enemy_id])
        map[creatures[id == enemy_id, ind]]<-'.'
        creatures <- creatures[id != enemy_id]
      }
    }
  }
  round <- round+1
  print(round)
}

creatures[,sum(HP)]
creatures[,sum(HP)]*round


