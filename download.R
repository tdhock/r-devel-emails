prefix <- "https://stat.ethz.ch/pipermail/r-devel/"
list.lines <- readLines(prefix)

some.lines <- c(
  "              <A href=\"2006-November/author.html\">[ Author ]</a>",
  "              <A href=\"2007-January/subject.html\">[ Subject ]</a>",
  "              <A href=\"2007-January/author.html\">[ Author ]</a>")


month.dt <- nc::capture_all_str(
  list.lines,
  '"',
  link=".*?/author.html")
older.dt <- month.dt[-1]

month.dt[, dest := file.path("months", link)]
todo.dt <- month.dt[!file.exists(dest)]
for(todo.i in seq_along(todo.dt$link)){
  todo.row <- todo.dt[todo.i]
  u <- paste0(prefix, todo.row$link)
  cat(sprintf("%4d / %4d %s -> %s\n", todo.i, nrow(todo.dt), u, todo.row$dest))
  dir.create(dirname(todo.row$dest), showWarnings=FALSE)
  download.file(u, todo.row$dest)
}
