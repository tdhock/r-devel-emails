library(ggplot2)
library(data.table)
message.dt <- data.table(csv=Sys.glob("months/*/author.csv"))[, {
  fread(csv)
}, by=csv]

author.funs <- list(
  "Luke Tierney"=function(author)grepl("uke", author) & grepl("ier|stat.uiowa", author),
  "Toby Hocking"=function(author)grepl("Hocking", author),
  "Hadley Wickham"=function(author)grepl("h(?:adley)?.wickham", author, ignore.case=TRUE),
  "Matt Dowle"=function(author)grepl("Dowle", author),
  "Brian Ripley"=function(author)grepl("ipley", author),
  "Martin Maechler"=function(author)grepl("maechler", author, ignore.case=TRUE))
message.dt[, Author := NA_character_]
for(a in names(author.funs)){
  fun <- author.funs[[a]]
  message.dt[fun(author), Author := a]
}
message.dt[, month.str := basename(dirname(csv))]
Sys.setlocale(locale="C")
message.dt[, month.POSIXct := suppressWarnings(
  strptime(paste0(month.str, "-01"), "%Y-%B-%d")
)]
author.month.dt <- message.dt[, .(
  messages.per.month=.N
), keyby=.(month.POSIXct, month.str, author)]
some.dt <- message.dt[!is.na(Author)]
(fp.check <- some.dt[, .(
  count=.N
), keyby=.(Author, author)])

all.month.dt <- data.table(
  month.POSIXct=message.dt[, seq(
    min(month.POSIXct), max(month.POSIXct), by="month")])
all.month.dt[, month.str := strftime(month.POSIXct, "%Y-%B")]
some.Author.na.dt <- some.dt[, {
  .SD[all.month.dt, on=.(month.str, month.POSIXct)]
}, by=.(Author)]
Author.month.dt <- some.Author.na.dt[, .(
  messages.per.month=sum(!is.na(id))
), keyby=.(Author, month.POSIXct)]

fn.check.list <- list()
for(a in c("Martin Maechler", "Luke Tierney")){
  zero <- Author.month.dt[Author==a & messages.per.month==0]
  fn.check.list[[a]] <- message.dt[zero, sort(unique(author)), on=.(month.POSIXct)]
}
fn.check.list

some.author.na.dt <- some.dt[, {
  .SD[all.month.dt, on=.(month.str, month.POSIXct)]
}, by=.(Author, author)]
author.month.dt <- some.author.na.dt[, .(
  messages.per.month=sum(!is.na(id))
), keyby=.(Author, author, month.POSIXct)]
ggplot()+
  geom_tile(aes(
    month.POSIXct, author, fill=messages.per.month>0),
    data=author.month.dt)+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(Author ~ ., scales="free", space="free")

lab.vec <- paste0(c(1997, seq(2000, 2020, by=5)), "-04")
breaks.vec <- as.POSIXct(strptime(paste0(lab.vec, "-01"), "%Y-%m-%d"))
ggplot()+
  geom_bar(aes(
    month.POSIXct, messages.per.month),
    stat="identity",
    data=Author.month.dt)+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(Author ~ .)+
  scale_x_datetime(
    "Month",
    breaks=breaks.vec,
    labels=lab.vec)

ggplot()+
  geom_point(aes(
    month.POSIXct, messages.per.month),
    data=Author.month.dt[0<messages.per.month])+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(Author ~ .)+
  scale_x_datetime(
    "Month",
    breaks=breaks.vec,
    labels=lab.vec)

