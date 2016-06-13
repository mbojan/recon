#' Get data from downloaded XLSX from IDI2
#' 
#' By default retrieves all sheets from interviews and returns a list of data frames.
#'
#' @param file character, XLSX file with data
#' @param sheet numeric/character, index number of name of the sheet(s) to be loaded
#' @param ... other arguments passed to \code{\link{readWorksheet}}
#'
#' @return
#' @export
get_data2 <- function(file, sheet=NULL, ...) {
  wb <- XLConnect::loadWorkbook(filename=file)
  sheet_names <- XLConnect::getSheets(wb)
  sh <- grep("^DC|^WF", sheet_names)
  if( is.null(sheet) ) {
    toget <- sh
  } else {
    if( is.numeric(sheet) ) {
      stopifnot( all(sheet %in% seq(along=sh) ) )
      toget <- sheet
    } else {
      stopifnot(is.character(sheet))
      stopifnot(all(sheet %in% sheet_names))
      toget <- which( sheet_names %in% sheet)
    }
  }
  # Get
  if( length(toget) == 1) {
    rval <- XLConnect::readWorksheet(wb, sheet=i, ...)
    return(rval)
  } else {
    rval <- lapply(toget, function(i) XLConnect::readWorksheet(wb, sheet=i, ...))
    names(rval) <- sheet_names[toget]
    return(rval)
  }
}





make_net2 <- function(d) {
  
}