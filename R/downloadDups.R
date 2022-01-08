downloadDups =
function(dir = "~/Downloads", files = list.files(dir, full = TRUE))
{
    f2 = basename(files)
    i = grep("\\([0-9]+\\)", f2)
    if(length(i) == 0)
       return(files)

    f3 = gsub(" ?\\([0-9]+\\)?", "", f2)
    ans = tapply(f2, f3, rmDups)
    ans = ans[sapply(ans, length) > 0]
    names(ans) = sapply(ans, `[[`, 1)
    lapply(ans, `[`, -1)           
}


rmDups =
function(f)
{
    i = file.info(f)
    g = split(f, i$size)
    tmp = sapply(g, function(x) {
      if(length(x) > 1) {
          list(ok = x[1], delete = structure(x[-1], names = x[1]))
      }
    })

    tmp[sapply(tmp, length) > 0]
}
