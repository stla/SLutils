
# faire aussi benchmark system system2

xfile <- system.file("extdata", "xlsx", "xlsx_big.xlsx", package="SLutils")

exe <- system.file("bin", .Platform$OS.type, "xlsx2json.exe", package="SLutils")
args <- sprintf("-f %s -s Sheet1 -w data -H", xfile)

json <- system2(exe, args, stdout=TRUE, stderr=TRUE)
length(json) # 2

json <- system(paste(exe, args), intern=TRUE)
length(json) # 2

command <- paste(exe, args)
library(microbenchmark)
microbenchmark(
  s2 = system2(exe, args, stdout=TRUE, stderr=TRUE),
  s1 = system(command, intern=TRUE),
  times = 30
)
# kif kif
