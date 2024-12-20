bigfiles =
function(dir = ".", recursive = FALSE, ext = TRUE)
{
    i = file.info(list.files(dir, full.names = TRUE, recursive = recursive))

    i = i[, c("size", "ctime")]
    if(ext) 
        i$ext = tools::file_ext(rownames(i))
        
    i[order(i$size, decreasing = TRUE), , drop = FALSE] 
}

