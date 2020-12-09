workDir <- 'C:/Users/smdevine/Desktop/GITprojects/advent_of_code_solutions/2020/day9'
list.files(workDir)
fname <- 'input_p1.txt'
code <- read.table(file.path(workDir, fname), stringsAsFactors = FALSE)
code <- code$V1
length(code)
create_vecs <- function(x,y) {
  x:y
}
code_chunks <- lapply(1:(length(code)-25), function(i) {
  code[i:(i+25)]
})
length(code_chunks)
head(code_chunks)

results <- rep(FALSE, length(code_chunks))
for (i in seq_along(code_chunks)) {
  test <- combn(code_chunks[[i]][1:25], 2, FUN = sum)
  results[i] <- code_chunks[[i]][26] %in% test
}
invalid_number <- code_chunks[[which(results==FALSE)]][26] 
paste('Day 9 part I answer is', invalid_number) #167829540

#part ii
i <- 1
next_to_test <- 2
test_sum <- code[1]
while(test_sum != invalid_number) {
  test_sum <- code[i+1] + test_sum
  i <- i + 1
  if(test_sum == invalid_number) {
    result_p2 <- code[(next_to_test - 1):i]
    # stop(print(paste('Success from', (next_to_test - 1), 'to', i)))
  } else if(test_sum > invalid_number) {
    i <- next_to_test
    print(i)
    next_to_test <- next_to_test + 1
    test_sum <- code[i]
  } else {next}
}
sum(result_p2) == invalid_number
paste('Day 9 part II answer is', min(result_p2) + max(result_p2))
#28045630 answer