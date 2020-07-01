if(FALSE){
  system("git log --shortstat --all > ../r-devel-emails/r-source-commits-all-shortstat.log")
}
log.f <- "r-source-commits-all-shortstat.log"
some.log <- readLines(log.f, n=100)

system(paste("tail -100", log.f))

commit.rest <- nc::capture_all_str(
  log.f,
  nc::field("commit", " ", ".*"),
  rest="(?:\n(?!commit).*)*")

pattern.up.to.id <- list(
  nc::field("Author", ": ", ".*?"),
  " <.*\n",
  nc::field("Date", ":\\s*", ".*"),
  "\n\n",
  message="(?:.*\n)*?",
  "\\s*git-svn-id: https://svn.r-project.org/R/",
  branch=".*?",
  "@",
  id="[0-9]+", as.integer)
optional.changes <- nc::quantifier(
  ".*",
  "\n\n\\s*",
  files="[0-9]+", as.integer,
  " file.*?",
  insertions="[0-9]+", as.integer,
  " insertion.*?",
  deletions="[0-9]+", as.integer,
  " deletion",
  "?")
na.dt <- nc::capture_first_vec(
  commit.rest[, rest],
  pattern.up.to.id,
  optional.changes,
  nomatch.error=FALSE)
na.dt[, subject := 1:.N]
commit.dt <- na.dt[!is.na(id)]

id.counts <- commit.dt[, .(
  count=.N
), by=.(id, branch)]
unusual.id.counts <- id.counts[1<count]
unusual.commits <- commit.dt[unusual.id.counts, on="id"]
strsplit(commit.rest$rest[unusual.commits$subject], split="\n")

all.ids <- data.table(id=1:max(commit.dt$id))
missing.ids <- commit.dt[all.ids, on="id"][is.na(branch), .(id)]
svn.dt <- data.table::fread("r-svn-commits-all.csv")
svn.missing <- svn.dt[missing.ids, on="id"]
svn.missing[, .(
  count=.N
), by=author]
svn.missing[author=="maechler"]
## not sure why there are some missing svn commits in the r-source git
## repo, but only a few hundred with commit messages that seem
## irrelevant, so ignoring.

commit.dt[, time.POSIXct := suppressWarnings(strptime(
  Date, "%a %b %d %H:%M:%S %Y %z"))]
commit.dt[, month.str := strftime(time.POSIXct, "%Y-%m")]
commit.dt[, month.POSIXct := suppressWarnings(strptime(
  paste0(month.str, "-01"), "%Y-%m-%d"))]
count.dt <- commit.dt[, .(
  commits=.N,
  insertions=sum(insertions, na.rm=TRUE),
  deletions=sum(deletions, na.rm=TRUE)
), by=.(Author, month.POSIXct)]

some.authors <- c(
  maechler="Martin Maechler",
  luke="Luke Tierney")
some.authors.dt <- data.table(
  Author=names(some.authors),
  full.name=some.authors)
some.counts <- count.dt[some.authors.dt, on="Author"]

fwrite(some.counts, "monthly_commits.csv")
  
lab.vec <- paste0(c(1997, seq(2000, 2020, by=5)), "-04")
breaks.vec <- as.POSIXct(strptime(paste0(lab.vec, "-01"), "%Y-%m-%d"))
library(ggplot2)
ggplot()+
  geom_bar(aes(
    month.POSIXct, commits),
    stat="identity",
    data=some.counts)+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(full.name ~ .)
