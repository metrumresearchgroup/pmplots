file <- file.path("inst", "examples", "pmplots_complete.Rmd")
lines <- readLines(file)
lines <- gsub("^#", "##", lines)
file <- file.path("inst", "examples", "pmplots_complete_validation.Rmd")
writeLines(lines, con = file)
devtools::document()
