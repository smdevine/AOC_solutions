workDir <- 'C:/Users/smdevine/Desktop/GITprojects/advent_of_code_solutions/2020/day5'
fname <- 'input_p1.txt'
passes <- read.table(file.path(workDir, fname), stringsAsFactors = FALSE)
choose_row <- function(x, rows_avail) {
  if(x=='F') {
    rows_avail[1:(length(rows_avail)/2)]
  } else {rows_avail[(length(rows_avail)/2+1):length(rows_avail)]}
}
choose_seat <- function(x, seats_avail) {
  if(x=='L') {
    seats_avail[1:(length(seats_avail)/2)]
  } else {seats_avail[(length(seats_avail)/2+1):length(seats_avail)]}
}
results_p1 <- data.frame(pass=passes$V1, row=NA, seat=NA, stringsAsFactors = FALSE)

for(i in 1:nrow(passes)) {
  pass <- unlist(strsplit(results_p1$pass[i], ''))
  row_test <- pass[1:7]
  seat_test <- pass[8:10]
  rows_avail <- 0:127
  for(j in seq_along(row_test)) {
    rows_avail <- choose_row(row_test[j], rows_avail = rows_avail)
  }
  results_p1$row[i] <- rows_avail
  seats_avail <- 0:7
  for(j in seq_along(seat_test)) {
    seats_avail <- choose_seat(seat_test[j], seats_avail = seats_avail)
  }
  results_p1$seat[i] <- seats_avail
}
results_p1$ID <- results_p1$row*8 + results_p1$seat
paste('Day 5, Part I answer is', max(results_p1$ID))

#part 2
first_row <- min(results_p1$row)
last_row <- max(results_p1$row)
check_rows <- sapply(first_row:last_row, function(x) sum(results_p1$row==x))
avail_row <- (first_row:last_row)[which(check_rows==7)] #because 8 indicates row is full
avail_seat <- (0:7)[!(0:7) %in% results_p1$seat[results_p1$row==avail_row]]
paste('Day 5 Part II answer is', avail_row*8+avail_seat)
