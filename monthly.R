library(data.table)
library(ggplot2)
count.dt.list <- list()
data.type.list <- list(
  "emails"=c("Author", "month.POSIXct", "messages.per.month"),
  "commits"=c("full.name", "month.POSIXct", "commits"))
for(data.type in names(data.type.list)){
  f <- paste0("monthly_", data.type, ".csv")
  cols <- data.type.list[[data.type]]
  dt <- data.table::fread(f)[, cols, with=FALSE]
  setnames(dt, c("author", "month", "count"))
  count.dt.list[[data.type]] <- data.table(data.type, dt)
}
count.dt <- do.call(rbind, count.dt.list)

count.dt[, month.POSIXct := suppressWarnings(strptime(month, "%Y-%m-%d"))]
ggplot()+
  geom_bar(aes(
    month.POSIXct, count),
    stat="identity",
    data=count.dt)+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(author + data.type ~ ., scales="free")
