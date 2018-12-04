library(data.table)
dt <- data.table(input=readLines('input04.txt'))
dt[, sp := gsub('\\[([0-9]+-[0-9]+-[0-9]+) ([0-9]+):([0-9]+)\\] (.*)', 
                '\\1_\\2_\\3_\\4', input)]
dt[, c('date_str', 'hour', 'minute', 'desc') := transpose(strsplit(sp, '_'))]
dt[, date := lubridate::ymd(date_str)]

#guard can arrive bit before midnight, but first sleep happened only after it
#there's only one guard per day if you count 23 for the next day, so use 
#the same guard id for the whole day's events
dt[hour == 23, date := date+1]
dt[desc %like% 'Guard', guard_id := gsub('Guard #([0-9]+) .*', '\\1' , desc)]
dt[!is.na(guard_id), length(unique(guard_id)), by = date][, max(V1)] == 1
dt[, guard_id := max(guard_id, na.rm = TRUE), by = date]

#remove guard entering and split into even and uneven rows to get corresponding sleep & wakeup
#on the same row
dt <- dt[!desc %like% 'Guard'][order(input)]
sleep <- dt[seq.int(1,(nrow(dt)),2), .(guard_id, date, input1 = input, sleep_minute = minute)]
stopifnot(all(sleep$input1 %like% 'falls asleep'))
wakeup <- dt[seq.int(2,(nrow(dt)),2), .(input2 = input, wake_minute = minute)]
stopifnot(all(wakeup$input2 %like% 'wakes up'))
sleeps <- cbind(sleep, wakeup)

#get guard id with longest sleep duration
sleeps[, sleep_dur := as.integer(wake_minute)-as.integer(sleep_minute)]
sleepy_guard <- sleeps[, .(sleep_dur = sum(sleep_dur)), by = guard_id][order(sleep_dur, decreasing = TRUE)][1, guard_id]
sleepy_guard_dt <- sleeps[guard_id == sleepy_guard, .(sleep_minute, wake_minute)]

#count occurences of each minute 0-59 in sleep time brackets
sleepy_mins <-sapply(0:59, function(x) {sum(sleepy_guard_dt[,sleep_minute <= x & wake_minute > x])})
names(sleepy_mins) <-0:59

#get maximum and count the checksum
chosen_minute <- as.numeric(names(sleepy_mins)[sleepy_mins == max(sleepy_mins)])
as.numeric(sleepy_guard)*chosen_minute

#part2

#make data table of each guards sleeping minutes
guard_dt <- sleeps[, lapply(0:59, function(x) {sum(.SD[, sleep_minute <= x & wake_minute > x])}), by = guard_id]
guards <- guard_dt$guard_id
guard_dt <- guard_dt[,guard_id := NULL]

ind <- which(guard_dt == max(guard_dt), arr.ind = TRUE)
as.numeric(guards[ind[1]])*(0:59)[ind[2]]
