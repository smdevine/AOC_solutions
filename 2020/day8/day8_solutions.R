workDir <- 'C:/Users/smdevine/Desktop/GITprojects/advent_of_code_solutions/2020/day8'
list.files(workDir)
code <- read.table(file.path(workDir, 'input_p1.txt'), stringsAsFactors = FALSE)
colnames(code) <- c('op', 'arg')
accumulator <- 0
acc_fun <- function(x) {
  accumulator <- accumulator + x
  accumulator
}
i <- 1
code$run <- 0
while(code$run[i]==0) {
  if(code$op[i]=='acc') {
    accumulator <- acc_fun(code$arg[i])
    code$run[i] <- 1
    i <- i + 1
    print(paste('The accumulator is', accumulator))
    next
  } else if(code$op[i]=='jmp') {
      code$run[i] <- 1
      i <- i + code$arg[i]
      print(paste('The accumulator is', accumulator))
      next
  } else {
      code$run[i] <- 1
      i <- i + 1
      print(paste('The accumulator is', accumulator))
      next
  }
}
#answer is last print out of accumulator

#part II
code <- read.table(file.path(workDir, 'input_p1.txt'), stringsAsFactors = FALSE)
colnames(code) <- c('op', 'arg')
code$run <- 0
nop_indices <- which(code$op=='nop')
jmp_indices <- which(code$op=='jmp')

counter <- 1
change_jmp <- TRUE #means you will test chaning the jmp operation to nop
#change to FALSE if you want to test changing the nop operations to jmp
for (j in if(change_jmp){jmp_indices}else{nop_indices}) {
  code <- read.table(file.path(workDir, 'input_p1.txt'), stringsAsFactors = FALSE)
  colnames(code) <- c('op', 'arg')
  code$run <- 0
  if(change_jmp) {
    code$op[j] <- 'nop'
    print(paste(counter, 'of', length(jmp_indices), 'tests.'))
  } else {
      code$op[j]<-'jmp'
      print(paste(counter, 'of', length(nop_indices), 'tests.'))
    }
  accumulator <- 0
  i <- 1
  while(code$run[i]==0) {
    if(code$op[i]=='acc') {
      accumulator <- acc_fun(code$arg[i])
      code$run[i] <- 1
      i <- i + 1
      if(i==(nrow(code)+1)) {
        stop(print(accumulator))
      } else {next}
    } else if(code$op[i]=='jmp') {
      code$run[i] <- 1
      i <- i + code$arg[i]
      if(i==(nrow(code)+1)) {
        stop(print(paste('Program terminated successfully! Accumulator is', accumulator)))
      } else {next}
    } else {
      code$run[i] <- 1
      i <- i + 1
      if(i==(nrow(code)+1)) {
        stop(print(accumulator))
      } else {next}
    }
  }
  counter <- counter + 1
}
#part II answer is when the "error" occurs with print out of accumulator