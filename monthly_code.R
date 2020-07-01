library(data.table)
library(ggplot2)
count.dt.list <- list()
data.type.list <- list(
  "emails"=c("Author", "month.POSIXct", "messages.per.month"),
  "commits"=c("full.name", "month.POSIXct", "commits"))
fmap <- c(
  commits="SVN commits",
  "log10(lines)"="log10(lines of code)",
  emails="R-devel emails")
ftype <- function(x){
  factor(fmap[x], fmap)
}
for(data.type in names(data.type.list)){
  f <- paste0("monthly_", data.type, ".csv")
  cols <- data.type.list[[data.type]]
  dt.raw <- data.table::fread(f, colClasses=list(POSIXct="month.POSIXct"))
  dt <- dt.raw[, cols, with=FALSE]
  setnames(dt, c("author", "month.POSIXct", "count"))
  count.dt.list[[data.type]] <- data.table(
    fac.type=ftype(data.type), dt)
}
count.dt <- do.call(rbind, count.dt.list)
ins.del <- melt(
  dt.raw,
  measure.vars=c("insertions", "deletions"))
ins.del[, fac.type := ftype("log10(lines)")]
ins.del[, log10.value := ifelse(0<value, log10(value), NA)]
ins.del[, side := ifelse(variable=="insertions", 1, -1)]
ins.del[, show := side*log10.value]
ins.del[, author := full.name]
one.insert <- dt.raw[1==insertions]
ins.del[one.insert, on=.(Author, full.name, month.POSIXct)]
ggplot()+
  geom_point(aes(
    month.POSIXct, 0),
    color="grey50",
    size=2,
    data=ins.del)+
  geom_bar(aes(
    month.POSIXct, show,
    fill=variable),
    stat="identity",
    data=ins.del)+
  scale_fill_manual(values=c(
    insertions="#4DAF4A",
    deletions="#E41A1C"))+
  geom_bar(aes(
    month.POSIXct, count),
    stat="identity",
    color="grey50",
    fill="grey50",
    data=count.dt)+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(author + fac.type ~ ., scales="free")

for(a in unique(count.dt$author)){
  a.counts <- count.dt[author==a]
  a.edits <- ins.del[author==a]
  out.png <- paste0(
    "monthly_code_",
    gsub(" ", "_", a),
    ".png")
  lab.vec <- paste0(c(1997, seq(2000, 2020, by=5)), "-04")
  breaks.vec <- as.POSIXct(strptime(paste0(lab.vec, "-01"), "%Y-%m-%d"))
  gg <- ggplot()+
    ggtitle(paste(
      a, "participation in R project"))+
    geom_point(aes(
      month.POSIXct, 0),
      color="grey50",
      size=0.5,
      data=a.edits)+
    geom_bar(aes(
      month.POSIXct, show,
      color=variable,
      fill=variable),
      stat="identity",
      data=a.edits)+
    scale_fill_manual(
      "edits",
      values=c(
        insertions="#4DAF4A",
        deletions="#E41A1C"))+
    scale_color_manual(
      "edits",
      values=c(
        insertions="#4DAF4A",
        deletions="#E41A1C"))+
    geom_bar(aes(
      month.POSIXct, count),
      color="grey50",
      fill="grey50",
      stat="identity",
      data=a.counts)+
    geom_text(aes(
      month.POSIXct, 0,
      label=variable,
      vjust=ifelse(variable=="insertions", -0.5, 1.5)),
      hjust=1,
      data=a.edits[month.POSIXct==max(month.POSIXct)])+
    theme_bw()+
    theme(
      legend.position="none",
      panel.spacing=grid::unit(0, "lines"))+
    facet_grid(fac.type ~ ., scales="free")+
    scale_x_datetime(
      "Month",
      breaks=breaks.vec,
      labels=lab.vec)+
    scale_y_continuous("Contributions per month")      
  png(out.png, width=7, height=5, units="in", res=200)
  print(gg)
  dev.off()
}
