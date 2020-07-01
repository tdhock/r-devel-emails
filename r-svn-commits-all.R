if(FALSE){
  system("svn log -r 0:HEAD https://svn.r-project.org/R/ > r-svn-commits-all.log")
}
commit.dt <- nc::capture_all_str(
  "r-svn-commits-all.log",
  "\nr",
  id="[0-9]+", as.integer,
  " [|] ",
  author=".*?",
  " [|] ",
  month.str="[0-9]{4}-[0-9]{2}")
data.table::fwrite(commit.dt, "r-svn-commits-all.csv")

