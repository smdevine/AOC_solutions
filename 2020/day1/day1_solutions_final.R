workDir <- 'C:/Users/smdevine/Desktop/GITprojects/advent_of_code_solutions/2020/day1'
fname <- 'input_p1.txt'
codes <- read.table(file.path(workDir, fname))
codes <- codes$V1
codes_compiled <- lapply(1:length(codes), function(i) {
  cbind(vec1=rep(codes[i], 199), vec2=codes[-i], vec3=NA)
  }
)
codes_compiled <- lapply(codes_compiled, function(x) {
  x[,3] <- x[,1] + x[,2]
  x
})
result_p1 <- lapply(codes_compiled, function(x) {
  if(2020 %in% x[,3]) {
    x[x[,3]==2020,]
  } else {NA}
})
result_p1 <- result_p1[!is.na(result_p1)]
paste('Day 1, Part I answer is', prod(result_p1[[1]][1:2]))
#787776

#part II
codes <- read.table(file.path(workDir, fname))
codes <- codes$V1
codes_compiled <- lapply(1:length(codes), function(i) {
  lapply(1:(length(codes)-1), function(j) {
    cbind(vec1=rep(codes[i], 198), vec2=rep(codes[-i][j], 198), vec3=codes[-i][-j], vec4=NA)
  })
})
codes_compiled_summed <- lapply(codes_compiled, function(x) {
  lapply(x, function(y) {
    y[,4] <- y[,1] + y[,2] + y[,3]
    y
  })
})
result_p2 <- lapply(codes_compiled_summed, function(x) {
  lapply(x, function(y) {
    if(2020 %in% y[,4]) {
      return(y[y[,4]==2020,])
    }
  })
})
result_p2 <- result_p2[!sapply(result_p2, function(x) is.null(unlist(x)))]
result_p2 <- result_p2[[1]]
result_p2 <- result_p2[!sapply(result_p2, function(x) is.null(unlist(x)))]
paste('Day 1, Part II answer is', prod(result_p2[[1]][1:3]))
#262738554
