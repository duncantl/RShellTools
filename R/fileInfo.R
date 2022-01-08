fileInfo = 
function(dir)
{
   ff = list.files(dir, full.names = TRUE, recursive = TRUE, all.files = TRUE, include.dirs = TRUE)
   info = file.info(ff)
   info$symLink = Sys.readlink(rownames(info))
   info
}


cmpDir =
function(dir, target, src = fileInfo(dir))
{
    if(identical(target, src))
        return(TRUE)

    if(nrow(target) != nrow(src)) {
        warning("different number of files in target and src")
        return( list(inTarget = setdiff(rownames(target), rownames(src)), inSource = setdiff(rownames(src), rownames(target))))
    } else {
        m = match(rownames(src), rownames(target))
        if(any(is.na(m))) {
            m2 <- match(rownames(target), rownames(src))
            warning(sum(is.na(m)),  " in src but not in target & ", sum(is.na(m2)), " in target but not in src")

            return( list(inTarget = setdiff(rownames(target), rownames(src)), inSource = setdiff(rownames(src), rownames(target))))        
        }
    }

     # reorder
    target = target[m,]
    if(identical(target, src))
        return(TRUE)

    ana = is.na(src$size)
    bna = is.na(target$size)
    if(!identical(ana, bna)) {
        warning("size NAs for different ", sum(ana != bna) , " files")
    }

    if(any(w <- (src$size[!ana] != target$size[!bna])) && (!all(src$isdir[!ana][w]) && !all(target$isdir[!ana][w]))) {
        warning("different sizes")
        browser()
        return(data.frame(sourceSize = src$size[!ana][w], targetSize = target$size[!bna][w], row.names = rownames(src)[!ana]))
    }
#    browser()
    TRUE
}
