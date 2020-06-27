html.files <- Sys.glob("months/*/author.html")

some.lines <- '
<LI><A HREF="079595.html">[Rd]  paste(character(0), collapse=&quot;&quot;, recycle0=FALSE) should be &quot;&quot;
</A><A NAME="79595">&nbsp;</A>
<I>Martin Maechler
</I>

<LI><A HREF="079435.html">[Rd]  [External] Re: R 4.0.0 build error with sysdata.rda on ppc64el architecture
</A><A NAME="79435">&nbsp;</A>
<I>Kirill Maslinsky
</I>
'

pattern.list <- list(
  '<LI><A HREF="',
  link='.*?',
  '">',
  Subject=".*",
  '\n</A><A NAME="',
  id=".*?",
  '">&nbsp;</A>\n<I>',
  author=".*")
nc::capture_all_str(
  some.lines,
  pattern.list)

library(data.table)
file.dt <- data.table(html=html.files, csv=sub("html$", "csv", html.files))
todo.dt <- file.dt[!file.exists(csv)]
for(todo.i in seq_along(todo.dt$html)){
  file.row <- todo.dt[todo.i]
  file.lines <- readLines(file.row$html)
  message.dt <- nc::capture_all_str(file.lines, pattern.list, engine="ICU")
  data.table::fwrite(message.dt, file.row$csv)
}
