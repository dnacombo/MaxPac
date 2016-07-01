

#' Find text in files
#' 
#' Print file names and line numbers where a text pattern is found in all text files within in a specified directory (and subdirectories)
#' 
#' @param tofind string: the pattern to find in all files
#' @param filter string: the pattern of file names to search in (default to \code{\.R(md)?$} = files ending in .R or .Rmd)
#' @param dir the directory root to search in (default to \code{getwd()})
#' @param full.names logical: Whether to return full path to files or not (default \code{TRUE})
#' @param recursive logical: Whether to search subdirectories (default \code{FALSE})
#' @examples
#' findtextinfiles('function')
#' 


findtextinfiles <- function (tofind,filter='\\.R(md)?$',dir=getwd(),full.names=T,recursive = F){
  fs = list.files(path=dir,pattern=filter,full.names = full.names,recursive=recursive)
  
  if (length(fs) > 0){
    for (i in 1:length(fs)) {
      t = readLines(fs[[i]])
      found = grepl(tofind,t)
      if (any(found)) {
        cat("In: ", fs[i],'\n')
        found = which(found)
        for (ii in 1:length(found)) {
          cat('line', found[ii], ':', t[found[ii]],'\n')
        }
      }
    }
  }
}