#this is a tweaked version of NCmisc::list.functions.in.file() that lets you 

filename <- "02_get_grambank_data.R"
alphabetic <- TRUE

list.functions.in.file_tweaked <- function(filename,alphabetic=TRUE, unique= TRUE) {
  # from hrbrmstr, StackExchange 3/1/2015
  if(!file.exists(filename)) { stop("couldn't find file ",filename) }
  if(!get.ext(filename)=="R") { warning("expecting *.R file, will try to proceed") }
  # requireNameSpace("dplyr")
  tmp <- getParseData(parse(filename, keep.source=TRUE))
  # next line does what dplyr used to do!
  nms <- tmp$text[which(tmp$token=="SYMBOL_FUNCTION_CALL")]
  funs <- if(alphabetic) { sort(nms) } else { nms }
  if(unique == T){
  funs <- unique(funs)}
  #crit <- quote(token == "SYMBOL_FUNCTION_CALL")
  #tmp <- dplyr::filter(tmp, .dots = crit)
  #tmp <- dplyr::filter(tmp,token=="SYMBOL_FUNCTION_CALL")
  #tmp <- unique(if(alphabetic) { sort(tmp$text) } else { tmp$text })
  src <- paste(as.vector(sapply(funs, find)))
  outlist <- tapply(funs, factor(src), c)
  return(outlist)
}