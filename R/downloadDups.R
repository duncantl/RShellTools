downloadDups =
    # reassigned below.
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


downloadDups =
function(dir = "~/Downloads", files = list.files(dir, full = TRUE), info = file.info(files),
            ignore = "(OneDrive|^~\\$.*(docx|xlsx|pptx)$v)")
{
    info = info[!info$isdir,]
    # Filter out files that are legitimate duplicates.
    info = info[!grepl(ignore, rownames(info)),]
    bySz = split(info, list(info$size, file.ext(rownames(info))))
    w = sapply(bySz, nrow) > 1
    sapply(rev(bySz[w]), idDups)
}



#  ~ at end
# rm extension   e.g., .zip  and then see if there is a (<count>) or -<count> or ' <count>'
idDups =
function(info, files = rownames(info)    )
{
    ext = unique(file.ext(files))
    fl = sub(paste0(".", ext), "", basename(files))
    pre = getCommonPrefix(fl)
    if(length(pre) == 0)
        return(character())
    if(grepl("[-(][0-9]*$", pre))
        pre = gsub("[-(][0-9]*$", "", pre)
    extra = substring(fl, nchar(pre) + 1)
    w = grepl("^([- _][0-9]+| ?\\([0-9]+\\))$", extra)
    if(all(w)) {
        structure(files[-1], names = rep(files[1], length(files)-1))
    } else if(!any(w)) {
        structure(rep("", length(files)), names = files)
    } else
        structure(files[w], names = rep(files[!w][1], sum(w)))
}

file.ext =
function(x)
{
    w = grepl("\\.tar.gz$", x)
    if(any(w)) {
        ans = rep("", length(x))
        ans[w] = "tar.gz"
        ans[!w] =   tools::file_ext(x[!w])
        return(ans)
    }
        
    tools::file_ext(x)
}

