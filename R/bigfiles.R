bigfiles =
function(dir = ".", recursive = FALSE)
{
  i = file.info(list.files(dir, full.names = TRUE, recursive = recursive))    
  i[order(i$size, decreasing = TRUE), c("size", "ctime"), drop = FALSE] 
}

