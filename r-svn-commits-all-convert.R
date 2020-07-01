commit.dt <- data.table::fread("r-svn-commits-all.csv")
commit.dt[, month.POSIXct := suppressWarnings(strptime(
  paste0(month.str, "-01"), "%Y-%m-%d"))]

commit.dt[, .(count=.N), by=author][order(-count)]

count.dt <- commit.dt[, .(
  count=.N
), by=.(author, month.POSIXct)]
some.authors <- c(
  maechler="Martin Maechler",
  luke="Luke Tierney")
some.authors.dt <- data.table(
  author=names(some.authors),
  full.name=some.authors)
some.counts <- count.dt[some.authors.dt, on="author"]

data.table::fwrite(some.counts, "monthly_commits.csv")
