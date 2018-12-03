data <- as.numeric(readLines('input01.txt'))
sum(data)

#part2
freq <- cumsum(data)
freq_all <- freq
while(!any(duplicated(freq_all))) {
  freq_all <- c(freq_all, freq + tail(freq_all, 1))
}
freq_all[duplicated(freq_all)][1]
