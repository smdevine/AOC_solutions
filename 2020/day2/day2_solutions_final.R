workDir <- 'C:/Users/smdevine/Desktop/GITprojects/advent_of_code_solutions/2020/day2'
fname <- 'input_p1.txt'
codes <- read.table(file.path(workDir, fname), sep=' ', stringsAsFactors = FALSE)
colnames(codes) <- c('min_max', 'letter', 'passwords')
codes$min <- as.integer(sapply(codes$min_max, function(x) unlist(strsplit(x, '-'))[1]))
codes$max <- as.integer(sapply(codes$min_max, function(x) unlist(strsplit(x, '-'))[2]))
codes$letter <- gsub(':', '', codes$letter)
codes$test <- sapply(1:nrow(codes), function(i) {
  sum(unlist(strsplit(codes$passwords[i], ''))==codes$letter[i])
})
codes$legit <- codes$test >= codes$min & codes$test <= codes$max
paste('Day 2, part I answer is', sum(codes$legit)) #396

#part 2
codes <- read.table(file.path(workDir, 'input_p1.txt'), sep=' ', stringsAsFactors = FALSE)
colnames(codes) <- c('pos1_pos2', 'letter', 'passwords')
codes$pos1 <- as.integer(sapply(codes$pos1_pos2, function(x) unlist(strsplit(x, '-'))[1]))
codes$pos2 <- as.integer(sapply(codes$pos1_pos2, function(x) unlist(strsplit(x, '-'))[2]))
codes$letter <- gsub(':', '', codes$letter)
codes$test <- sapply(1:nrow(codes), function(i) {
  sum(unlist(strsplit(codes$passwords[i], ''))[codes$pos1[i]]==codes$letter[i], unlist(strsplit(codes$passwords[i], ''))[codes$pos2[i]]==codes$letter[i])
})
paste('Day 2 part II answer is', sum(codes$test==1))
