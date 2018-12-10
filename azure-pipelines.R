library(devtools)

devtools::check()
devtools::test(filter="datashield.")

quit(status=0)
