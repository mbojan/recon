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
  d <- get_data(sheet)
  g <- make_net(d)
  vis_net(g, ...)
}




#' \code{make_net} downloads data and creates igraph object.
#'
#' @param d data frame downloaded from GS
#'
#' @rdname vis
#' @export
make_net <- function(d)
{
  # drop all-NA rows
  nas <- sapply(d, is.na)
  allna <- apply(nas, 1, all)
  d <- subset(d, !allna)
  # fix adjlists
  vnames <- c("grupa", "wspolpracownicy", "boss")
  d[vnames] <- lapply(d[vnames], function(x) gsub("[^0-9,]", "", x))
  # wspolpraca
  adjlist_wspolpraca <- lapply(strsplit(d$wspolpracownicy, ","), as.numeric)
  vdb <- subset(d, select=c("ego", "kolor", "ksztalt", "kasa", "plec"))
  edb <- alist_to_elist(d$ego, adjlist_wspolpraca)
  edb$typ <- rep("wspolpraca", nrow(edb))
  # slave-master
  boss_adjlist <- lapply(strsplit(d$boss, ","), as.numeric)
  bossedb <- as.data.frame(alist_to_elist(d$ego, boss_adjlist))
  bossedb$typ <- rep("boss", nrow(bossedb))
  edb <- rbind(edb, bossedb)
  rval <- igraph::graph.data.frame(edb, vertices=vdb, directed=TRUE)
  igraph::V(rval)$grupa <- d$grupa
  rval
}



get_data <- function(sheet)
{
  # Wczytanie
  obrazki <- googlesheets::gs_key("1IApsDIawqBGH1KuWpo22zJWAbrLJp5ft0oZDUuZ8UOs")
  d <- googlesheets::gs_reshape_cellfeed(googlesheets::gs_read_cellfeed(obrazki, ws=sheet, range=googlesheets::cell_limits(c(2,1), c(NA,8))))
  names(d) <- c("ego", "kolor", "ksztalt", "kasa", "grupa", "wspolpracownicy", "boss", "plec")
  d
}



#' \code{vis_net} draws created igraph object.
#'
#' @param g igraph object
#' @param vid numeric ids of vertices to draw, all by default
#' @param gid numeric ids of groups to draw or \code{"all"} (default), or \code{"none"}.
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
                    vid=V(g),
                    gid="all",
                    vcol = c("#66c2a5", "yellow", "#8da0cb", "white"),
                    vshape=c("mbcircle", "mbsquare"),
                    vframe=c("black"=NA, "#e41a1c"=0, "#377eb8"=1),
                    gcol = RColorBrewer::brewer.pal(8, "Set3"),
                    ggroups = TRUE )
{
  # subset if necessary
  register_vertex_shapes()
  g <- induced.subgraph(g, vids=vid)
  # grupy
  l <- strsplit(igraph::V(g)$grupa, ",")
  u <- na.omit(unique(unlist(l)))
  grupy <- lapply(u, function(gid) which(sapply(l, function(x) gid %in% x)))
  names(grupy) <- u
  # group selection
  if(is.numeric(gid))
  {
    grupy <- grupy[as.character(gid)]
  }
  # rys!
  igraph::V(g)$kasa[ is.na(igraph::V(g)$kasa) ] <- 0
  igraph::V(g)$kolor[ is.na(igraph::V(g)$kolor) ] <- 1
  igraph::V(g)$ksztalt[ is.na(igraph::V(g)$ksztalt) ] <- 1
  # add ties within groups just for layout comp
  if(ggroups && !identical(gid, "none"))
  {
    memb <- matrix(sapply(l, function(x) u %in% x), length(u), length(l))
    am <- t(memb) %*% memb
    colnames(am) <- rownames(am) <- V(g)$name
    lg <- igraph::simplify(igraph::graph.union(g, igraph::graph.adjacency(am)))
    lay <- igraph::layout.fruchterman.reingold(lg)
  } else {
    lay <- igraph::layout.fruchterman.reingold(g)
  }
  gb <- igraph::delete.edges(g, igraph::E(g)[typ != "boss"])
  gw <- igraph::simplify(igraph::as.undirected(igraph::delete.edges(g, igraph::E(g)[typ == "boss"])))
  # czy rysujemy jakieś grupy
  if(identical(gid, "none"))
  {
    igraph::plot.igraph(gb, layout=lay, vertex.shape="none", edge.curved=0.3, edge.width=2,
                      vertex.size=ifelse(igraph::V(g)$kolor == 4, 25, 15),
                        edge.color="black")
  } else {
    igraph::plot.igraph(gb, layout=lay, vertex.shape="none", edge.curved=0.3,
         edge.color="black", mark.groups=grupy, edge.width=2,
                      vertex.size=ifelse(igraph::V(g)$kolor == 4, 25, 15),
         mark.col=adjustcolor(gcol[as.numeric(names(grupy))], alpha.f=0.3),
         mark.border=adjustcolor(gcol[as.numeric(names(grupy))], alpha.f=1) )
  }
  igraph::plot.igraph(gw, layout=lay, add=TRUE, edge.color="black", edge.width=3,
                      vertex.color=vcol[igraph::V(gw)$kolor],
                      vertex.shape=vshape[igraph::V(gw)$ksztalt],
                      vertex.size=ifelse(igraph::V(g)$kolor == 4, 25, 15),
                      vertex.label.color=ifelse(igraph::V(g)$kasa == 1, "red", "black"),
                      # vertex.label.family="sans",
                      vertex.frame.width=ifelse(is.na(V(g)$plec), 1, 4),
                      vertex.frame.color=nameplace(igraph::V(gw)$plec, vframe)
                      )
}





#' \code{vis_all} visualizes all networks and saves them to files 'XXXX.png'
#'
#' @param sheet_names names of GS sheet names to visualize, if \code{NULL} draws all \code{WF} and \code{DC} sheets with numbers 1 to 15. Single character element (e.g. \code{"WF"}) is interpreted as a regular expression to search through sheet names
#' @param skip_sheets character vector, if \code{sheet_names} is \code{NULL}, sheets with these names will be skipped
#' @param overwrite logical redo existing files
#'
#' @rdname vis
#' @export
vis_all <- function(sheet_names=NULL, skip_sheets=NULL, overwrite=FALSE, ...)
{
  all_sheets <- c( paste0("WF", sprintf("%02d", 1:15)),
                     paste0("DC", sprintf("%02d", 1:15)) )
  if(is.null(sheet_names))
  {
    sheet_names <- all_sheets
    if(!is.null(skip_sheets))
    {
      w <- sheet_names %in% skip_sheets
      sheet_names <- sheet_names[!w]
    }
  }
  # regexp
  if(is.character(sheet_names) && length(sheet_names) == 1)
  {
    sheet_names <- grep(sheet_names, all_sheets, value=TRUE)
  }
  obrazki <- googlesheets::gs_key("1IApsDIawqBGH1KuWpo22zJWAbrLJp5ft0oZDUuZ8UOs")
  for( sheet in sheet_names )
  {
    cat("Sheet", sheet, "...\n")
    fname <- paste0(sheet, ".png")
    if(file.exists(fname) && !overwrite)
    {
      cat("SKIPPING\n")
      next
    }
    cat("DRAWING ...\n")
    d <- googlesheets::gs_reshape_cellfeed(googlesheets::gs_read_cellfeed(obrazki, ws=sheet, range=googlesheets::cell_limits(c(2,1), cols=c(NA,8))))
    names(d) <- c("ego", "kolor", "ksztalt", "kasa", "grupa", "wspolpracownicy", "boss", "plec")
    g <- make_net(d)
    png(fname)
    op <- par(mar=c(1,1,1,1))
    r <- try(vis_net(g, ...))
    if(inherits(r, "try-error")) {
      cat("ERROR!\n")
    } else {
      cat("DONE\n")
    }
    par(op)
    dev.off()
  }
}







# convert adjacency list to edgelist
alist_to_elist <- function(ego, adjlist)
{
  stopifnot( length(ego) == length(adjlist) )
  lens <- sapply(adjlist, length)
  isna <- sapply(adjlist, function(x) all(is.na(x)))
  empty <- lens == 1 & isna
  ego <- ego[!empty]
  adjlist <- adjlist[!empty]
  lens <- lens[!empty]
  rval <- cbind(ego=rep(ego, lens), alter=unlist(adjlist))
  as.data.frame(rval)
}


# replace values using by a named vector
nameplace <- function(x, v)
{
  w <- unique(x) %in% v
  if(!all(w)) stop("don't know how to draw", x[!w])
  mv <- match(x, v)
  names(v)[mv]
}
