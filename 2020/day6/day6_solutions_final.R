workDir <- 'C:/Users/smdevine/Desktop/GITprojects/advent_of_code_solutions/2020/day6'
fname <- 'input_p1.txt'
answers <- readLines(con=file.path(workDir, fname))
blank_lines <- sapply(1:length(answers), function(i) if(answers[i]=='') {TRUE} else{FALSE})
indices_blank <- 1:length(answers)
indices_blank <- indices_blank[blank_lines]
answers_final <- character(length=sum(blank_lines)+1)
record <- answers[1]
j <- 1
for (i in 2:length(answers)) {
  if (i %in% indices_blank) {
    answers_final[j] <- record
    j <- j+1
    record <- character(length = 0)
    next
  }
  record <- paste(record, answers[i])
}
answers_final[length(answers_final)] <- record

results_p1 <- rep(NA, length(answers_final))
for (i in seq_along(results_p1)) {  
  test <- unlist(strsplit(answers_final[i], ''))
  test <- test[!(test=='' | test==' ')]
  results_p1[i] <- length(unique(test))
}
paste('Day 6, Part I answer is', sum(results_p1)) #this is the answer

#part 2
count_shared_letters <- function(ind1, ind2) {
  sum(test2[[ind1]] %in% test2[[ind2]])
}
answers_final[1]
results_part2 <- rep(NA, length(answers_final))
for (i in seq_along(answers_final)) {
  test <- unlist(strsplit(answers_final[i], ' '))
  test <- test[!(test=='' | test==' ')]
  test2 <- strsplit(test, '')
  test2 <- lapply(test2, function(x) unique(x))
  test_grid <- expand.grid(1:length(test2), 1:length(test2))
  test_grid <- t(apply(test_grid, 1, sort))
  test_grid <- cbind(test_grid, NA)
  test_grid[,3] <- sapply(1:nrow(test_grid), function(i) {
    count_shared_letters(ind1 = test_grid[i,1], ind2 = test_grid[i,2])
  })
  results_part2[i] <- min(test_grid[,3])
}
paste('Day 6, Part II answer is', sum(results_part2))
