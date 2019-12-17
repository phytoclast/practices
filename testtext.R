library(readr)

df <- read.delim("data/fagaceae.txt", skip = 300, nrows = 100, header = FALSE, fileEncoding="UTF-8-BOM", stringsAsFactors=FALSE)
colnames(df) <- colnames(read.delim("data/fagaceae.txt", nrows = 0, header = TRUE, fileEncoding="UTF-8-BOM"))

df2 <- read_tsv("data/fagaceae.txt", skip = 6, n_max = 100, col_names = FALSE)
colnames(df2) <- colnames(read_tsv("data/fagaceae.txt", n_max = 0, col_names = TRUE))

filelinecount <- length(read_lines("data/fagaceae.txt"))
