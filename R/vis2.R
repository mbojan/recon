#' Get data from downloaded XLSX from IDI2
#' 
#' By default retrieves all sheets from interviews and returns a list of data frames.
#'
#' @param file character, XLSX file with data
#' @param sheet numeric/character, index number of name of the sheet(s) to be loaded
#' @param ... other arguments passed to \code{\link{readWorksheet}}
#'
#' @return Data frame or list thereof.
#' @export
get_data2 <- function(file, sheet=NULL, ...) {
  wb <- XLConnect::loadWorkbook(filename=file)
  sheet_names <- XLConnect::getSheets(wb)
  sh <- grep("^DC|^WF", sheet_names)
  # What to get
  if( is.null(sheet) ) {
    toget <- sh
  } else {
    stopifnot(is.character(sheet))
    stopifnot(all(sheet %in% sheet_names))
    toget <- which(sheet_names %in% sheet)
  }
  # Get
  if( length(toget) == 1) {
    rval <- XLConnect::readWorksheet(wb, sheet=toget, ...)
    attr(rval, "idi_id") <- sheet_names[toget]
    return(rval)
  } else {
    rval <- lapply(toget, function(i)
      structure(
        XLConnect::readWorksheet(wb, sheet=i, ...),
        idi_id = sheet_names[i]
      ) )
    names(rval) <- sheet_names[toget]
    return(rval)
  }
}



#' Create network object
#'
#' @param d data frame
#'
#' @return igraph object
#' @export
make_net2 <- function(d) {
  # Vertex DB
  vdb <- data.frame(
    id=d$Nr.wezła.2,
    grupa = gsub("[^0-9,]", "", d$Przynależność.do.grup..na.podstawie.zdjęcie..grupa.może.być.też.dwuosobowa.)
  )
  # Edge DB
  edb <- rbind(
    data.frame(
      ego = d$Nr.wezła.1,
      alter = d$Nr.wezła.2,
      res = gsub("[^0-9,]", "", d$Kategoria.zasobów.pzekazanych)
    ),
    data.frame(
      ego = d$Nr.wezła.2,
      alter = d$Nr.wezła.1,
      res = gsub("[^0-9,]", "", d$Kategorie.zasobów.otrzymanych)
    )
  )
  edb <- subset(edb, ego != alter)
  
  g <- igraph::graph_from_data_frame(edb, directed = TRUE, vertices = vdb)
  g$idi_id <- attr(d, "idi_id")
  g
}