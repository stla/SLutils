

xfile <- system.file("extdata", "xlsx", "xlsx_utf8.xlsx", package="SLutils")

exe <- system.file("bin", .Platform$OS.type, "xlsx2json.exe", package="SLutils")
args <- sprintf("-f %s -s Sheet1 -w data -H", xfile)
json <- system2(exe, args, stdout=TRUE, stderr=TRUE)
length(json)
json

withr::with_options(
  list(encoding="UTF-8"),
  system2(exe, args, stdout=TRUE, stderr=TRUE))
# FAIL

