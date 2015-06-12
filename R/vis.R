#' Visualise net from sheet
#'
#' Visualise net from sheet.
#'
#' Needs authorisation with \code{gs_auth()} first.
#'
#' @param sheet sheet name
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(googlesheets)
#' gs_auth()
#' vis("WF01")
#' }
vis <- function(sheet)
{
  # Wczytanie
  obrazki <- googlesheets::gs_key("1IApsDIawqBGH1KuWpo22zJWAbrLJp5ft0oZDUuZ8UOs")
  d <- googlesheets::gs_reshape_cellfeed(googlesheets::gs_read_cellfeed(obrazki, ws=sheet, range=googlesheets::cell_limits(rows=c(2,NA), cols=c(1,7))))
  names(d) <- c("ego", "kolor", "ksztalt", "frame", "grupa", "wspolpracownicy", "boss")
  # fix adjlists
  vnames <- c("grupa", "wspolpracownicy", "boss")
  d[vnames] <- lapply(d[vnames], function(x) gsub(" *, *", ",", x))
  # wspolpraca
  adjlist_wspolpraca <- lapply(strsplit(d$wspolpracownicy, ","), as.numeric)
  g <- igraph::simplify(igraph::graph.edgelist(alist_to_elist(d$ego, adjlist_wspolpraca), directed=TRUE))
  # master-slave
  boss_adjlist <- lapply(strsplit(d$boss, ","), as.numeric)
  g <- igraph::add.edges(g, t(alist_to_elist(d$ego, boss_adjlist)), attr=list(boss=TRUE))
  igraph::E(g)$boss <- !is.na(igraph::E(g)$boss)
  # kolor
  pal <- scales::brewer_pal(type="qual", palette=2)(length(unique(d$kolor)))
  igraph::V(g)$color <- pal[d$kolor]
  # shape
  shps <- c("circle", "square")
  igraph::V(g)$shape <- shps[d$ksztalt]
  # grupy
  l <- strsplit(d$grupa, ",")
  u <- na.omit(unique(unlist(l)))
  grupy <- lapply(u, function(gid) which(sapply(l, function(x) gid %in% x)))
  # rys!
  gb <- igraph::delete.edges(g, igraph::E(g)[!boss])
  gw <- igraph::as.undirected(igraph::delete.edges(g, igraph::E(g)[boss]))
  lay <- igraph::layout.fruchterman.reingold(g)
  igraph::plot.igraph(gb, layout=lay, vertex.shape="none", edge.curved=0.3,
       edge.color="black", mark.groups=grupy)
  igraph::plot.igraph(gw, layout=lay, add=TRUE, edge.color="red", edge.width=3)
}





# adjlist to edgelist
alist_to_elist <- function(ego, adjlist)
{
  stopifnot( length(ego) == length(adjlist) )
  lens <- sapply(adjlist, length)
  rval <- cbind(ego=rep(ego, lens), alter=unlist(adjlist))
  isna <- apply(rval, 1, function(x) any(is.na(x)))
  rval[ !isna , ]
}
