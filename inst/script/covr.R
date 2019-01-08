library(covr)

x <- package_coverage('.', quiet=FALSE)

y <- coverage_to_list(x)

df <- data.frame(file = names(y$filecoverage), coverage = y$filecoverage, row.names=NULL)
df <- df[order(as.numeric(df$coverage)),]

outfile <- "inst/docs/coverage.md"
cat(file=outfile, "# coverage: ",y$totalcoverage, "%\n\n", sep="")
cat(file=outfile, append = TRUE, knitr::kable(df,row.names=FALSE),sep="\n")

x
