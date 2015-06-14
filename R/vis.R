#' Visualise team networks
#'
#' Download and visualize team networks based on data in a Google Spreadsheet.
#'
#' Needs authorisation with \code{gs_auth()} first.
#'
#' @param sheet sheet name
#' @param ... other arguments passed to \code{vis_net}
#'
#' @export
vis <- function(sheet, ...)
{
  g <- make_net(sheet=sheet)
  vis_net(g, ...)
}




#' \code{make_net} downloads data and creates network object
#'
#' @rdname vis
#' @export
make_net <- function(sheet)
{
  # Wczytanie
  obrazki <- googlesheets::gs_key("1IApsDIawqBGH1KuWpo22zJWAbrLJp5ft0oZDUuZ8UOs")
  d <- googlesheets::gs_reshape_cellfeed(googlesheets::gs_read_cellfeed(obrazki, ws=sheet, range=googlesheets::cell_limits(rows=c(2,NA), cols=c(1,7))))
  names(d) <- c("ego", "kolor", "ksztalt", "frame", "grupa", "wspolpracownicy", "boss")
  # fix adjlists
  vnames <- c("grupa", "wspolpracownicy", "boss")
  d[vnames] <- lapply(d[vnames], function(x) gsub("[^0-9,]", "", x))
  # wspolpraca
  adjlist_wspolpraca <- lapply(strsplit(d$wspolpracownicy, ","), as.numeric)
  vdb <- subset(d, select=c("ego", "kolor", "ksztalt", "frame"))
  edb <- alist_to_elist(d$ego, adjlist_wspolpraca)
  edb$typ <- "wspolpraca"
  # master-slave
  boss_adjlist <- lapply(strsplit(d$boss, ","), as.numeric)
  bossedb <- as.data.frame(alist_to_elist(d$ego, boss_adjlist))
  bossedb$typ <- "boss"
  edb <- rbind(edb, bossedb)
  rval <- igraph::graph.data.frame(edb, vertices=vdb, directed=TRUE)
  igraph::V(rval)$grupa <- d$grupa
  rval
}




#' \code{vis_net} draws created network object
#'
#' @param g network
#' @param vcol vertex color mapping
#' @param vshape vertex shape mapping
#' @param vframe vertex frame color mapping
#' @param gcol vertex group color mapping
#' @param ggroups whether to take groups into account when calculating layout
#'
#' @rdname vis
#' @import igraph
#' @export
vis_net <- function(g, 
                    vcol=c("#e41a1c", "#ffff33", "#377eb8", "white"),
                    vshape=c("circle", "square"),
                    vframe=c("black", "#4daf4a"),
                    gcol = RColorBrewer::brewer.pal(8, "Set3"),
                    ggroups = TRUE )
{
  # grupy
  l <- strsplit(igraph::V(g)$grupa, ",")
  u <- na.omit(unique(unlist(l)))
  grupy <- lapply(u, function(gid) which(sapply(l, function(x) gid %in% x)))
  names(grupy) <- u
  # rys!
  igraph::V(g)$frame[ is.na(igraph::V(g)$frame) ] <- 1
  # add ties within groups just for layout comp
  if(ggroups)
  {
    memb <- sapply(l, function(x) u %in% x)
    am <- t(memb) %*% memb
    colnames(am) <- rownames(am) <- seq(1, igraph::vcount(g))
    lg <- igraph::simplify(igraph::graph.union(g, igraph::graph.adjacency(am)))
    lay <- igraph::layout.fruchterman.reingold(lg)
  } else {
    lay <- igraph::layout.fruchterman.reingold(g)
  }
  gb <- igraph::delete.edges(g, igraph::E(g)[typ != "boss"])
  gw <- igraph::simplify(igraph::as.undirected(igraph::delete.edges(g, igraph::E(g)[typ == "boss"])))
  igraph::plot.igraph(gb, layout=lay, vertex.shape="none", edge.curved=0.3,
       edge.color="black", mark.groups=grupy, 
       mark.col=adjustcolor(gcol[as.numeric(names(grupy))], alpha.f=0.3),
       mark.border=adjustcolor(gcol[as.numeric(names(grupy))], alpha.f=1) )
  igraph::plot.igraph(gw, layout=lay, add=TRUE, edge.color="red", edge.width=3,
                      vertex.color=vcol[igraph::V(gw)$kolor],
                      vertex.shape=vshape[igraph::V(gw)$ksztalt],
                      vertex.size=ifelse(igraph::V(g)$kolor == 4, 25, 15),
                      vertex.frame.color=vframe[igraph::V(gw)$frame] )
}








# convert adjacency list to edgelist
alist_to_elist <- function(ego, adjlist)
{
  stopifnot( length(ego) == length(adjlist) )
  lens <- sapply(adjlist, length)
  rval <- cbind(ego=rep(ego, lens), alter=unlist(adjlist))
  isna <- apply(rval, 1, function(x) any(is.na(x)))
  as.data.frame(rval[ !isna , ])
}
