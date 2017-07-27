library(lineprof)

source("sample_pairs.R")

l <- lineprof(f())
shine(l)
