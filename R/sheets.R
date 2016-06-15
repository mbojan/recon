#' Get sheet names from Google/Excel sheet
#' 
#' @param object sheet, see Details for available methods
#' @param ... other arguments passed to/from other methods. Ignored for
#'   googlesheet method
#'   
#' @return Character vector of sheet names
#' @export
sheet_names <- function(object, ...) UseMethod("sheet_names")

#' @method sheet_names googlesheet
#' @export
#' @rdname sheet_names
#'   
#' Google sheets: Method for google sheets uses \code{\link{gs_ws_ls}} to get
#' the sheet names. Additional \code{...} arguments are ignored.
#' 
sheet_names.googlesheet <- function(object, ...) {
  googlesheets::gs_ws_ls(object)
}

#' @method sheet_names workbook
#' @export
#' @rdname sheet_names
#' 
#' Excel files: Interacting with Excel files is through \pkg{XLConnect}. Sheet
#' names are retrieved using \code{\link{getSheets}}, which recieves \code{...}
#' arguments.
#' 
sheet_names.workbook <- function(object, ...) {
  XLConnect::getSheets(object, ...)
}




get_sheets <- function(object, sheet=NULL, ...) { 
  sh_names <- sheet_names(object)
  if( is.numeric(sheet) ) {
    has <- sheet %in% seq(along=sh_names)
  } else {
    stopifnot(is.character(sheet))
    has <- sheet %in% sh_names
  }
  if( any(!has) )
    stop("there are ", length(sh_names), " sheets, cant get: ",
         paste(sheet[!has], collapse=", "))
  rval <- lapply(
    sheet,
    function(s) get_sheet(object, sheet=s, ...)
  )
  names(rval) <- sheet
  rval
}


get_sheet <- function(object, ...) UseMethod("get_sheet")

get_sheet.workbook <- function(object, sheet, ...) {
  XLConnect::readWorksheet(object, sheet, ...)
}

get_sheet.googlesheet <- function(object, sheet, 
                                  method=c("csv", "list", "cell"),
                                  ...) {
  method <- match.arg(method)
  args <- list(...)
  fun <- switch(method,
                csv="gs_read_csv",
                list="gs_read_listfeed",
                cell = "gs_read_cellfeed" )
  rval <- do.call(fun, c( list(ss=object, ws=sheet), args ))
  rval
}


get_xls <- function(from, to="resources.xlsx", ...) {
  gs_download(from=from, to=to, ...)
}
