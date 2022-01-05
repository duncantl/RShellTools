bigfiles =
function(dir = "~/Downloads")
{
  i = file.info(list.files(dir, full.names = TRUE))    
  i[order(i$size, decreasing = TRUE), c("size", "ctime"), drop = FALSE] 
}

