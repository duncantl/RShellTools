du =
function(dir = ".", recursive = FALSE, flags = "-hs")
{
    dir = path.expand(dir)


    if(recursive) {
        dirs = system(sprintf('find %s -type d -exec du -hs *', shQuote(dir)), intern  = TRUE)
        
    }
    
    ll = system(sprintf("du %s %s/*", flags, dir), intern = TRUE)
    readDuOutput(ll)
}

readDuOutput =
function(x, units = NA)    
{
    if(length(x) == 1 && file.exists(x))
       x = readLines(x)

    # ignore lines such as
    # du: cannot read directory 'Library/Mail': Operation not permitted
    x = grep("^du:", x, invert = TRUE, value = TRUE)

    els = strsplit(x, "\t")
    sz = sapply(els, `[`, 1)

    unit = gsub(".*([KMGT])$", "\\1", sz)
    sz = as.numeric(gsub("([KMGT])$", "", sz))
    mul = 1024^c(B = 0, K = 1, M = 2, G = 3, T = 4)
    bytes = sz*mul[unit]

    bytes[is.na(bytes)] = sz[is.na(bytes)] # 0

    if(!is.na(units)) 
        bytes = bytes/mul[units]

    dir = sapply(els, `[`, 2)
    ans = data.frame(bytes = bytes, units = unit, row.names = dir)
    structure(ans[order(ans$bytes, decreasing = TRUE),], class = c("du.output", "data.frame"))
}

cvtTo =
function(vals, to, from = "B")
{
    mul = 1024^c(B = 0, K = 1, M = 2, G = 3, T = 4)
    vals*mul[from]/mul[to]
}

