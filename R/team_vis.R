# 1. import danych z GS
# 2. przekształcenie danych w sieć
# 3. wizualizacja sieci
sk <- "1IApsDIawqBGH1KuWpo22zJWAbrLJp5ft0oZDUuZ8UOs"


# Pobranie danych z GS
import_data <- function(sheet, sheet_key, ...)
{
  sh <- googlesheets::gs_key(sheet_key)
  googlesheets::gs_reshape_cellfeed(gs_read_cellfeed(sh, ws=sheet, ...))
}

gs_auth()
sh <- gs_key(sk)
d <- import_data("WF03", sheet_key=sk, cell_limits(rows=c(2,NA), cols=c(1,7)))

# interpretacja danych z GS
# zwraca  listę ramek danych: osoby, grupy, relacje
interpret <- function(d, 
                      new_col_names=c("ego", "kolor", "ksztalt", "frame", "grupa",
                                  "wspolpracownicy", "boss"),
                      dir_cols="boss",
                      undir_cols="wspolpracownicy"
                      grp_cols="grupa")
{
  if(!is.null(new_col_names)) names(d) <- new_col_names
  # ujednolicenie formatu kolumn relacyjnych
  rel_cols <- c(dir_cols, undir_cols, grp_cols)
  stopifnot(any(duplicated(net_cols)))
  d[rel_cols] <- lapply(d[net_cols], function(x) gsub("[^[0-9,]", "", x))
  # osoby
  osoby <- subset(d, select=!(names(d) %in% rel_cols))
  # grupy (osoba-grupa)
  grupy <- cbind(d$ego, d$grupa
}

interpret(d)



# zainstalować 'googlesheets' jeśli nie ma
devtools::install_github("jennybc/googlesheets")

library(googlesheets)
library(igraph)
library(scales)


# autoryzacja
gs_auth()
# gs_ls()


alist_to_elist <- function(ego, adjlist)
{
  stopifnot( length(ego) == length(adjlist) )
  lens <- sapply(adjlist, length)
  rval <- cbind(ego=rep(ego, lens), alter=unlist(adjlist))
  isna <- apply(rval, 1, function(x) any(is.na(x)))
  rval[ !isna , ]
}


sheet <- "WF01"

vis <- function(sheet)
{
  require(googlesheets)
  require(igraph)
  require(scales)
  # Wczytanie
  obrazki <- gs_key("1IApsDIawqBGH1KuWpo22zJWAbrLJp5ft0oZDUuZ8UOs")
  d <- gs_reshape_cellfeed(gs_read_cellfeed(obrazki, ws=sheet, range=cell_limits(rows=c(2,NA), cols=c(1,7))))
  names(d) <- c("ego", "kolor", "ksztalt", "frame", "grupa", "wspolpracownicy", "boss")
  # fix adjlists
  vnames <- c("grupa", "wspolpracownicy", "boss")
  d[vnames] <- lapply(d[vnames], function(x) gsub(" *, *", ",", x))
  # wspolpraca
  adjlist_wspolpraca <- lapply(strsplit(d$wspolpracownicy, ","), as.numeric)
  g <- simplify(graph.edgelist(alist_to_elist(d$ego, adjlist_wspolpraca), directed=TRUE))
  # master-slave
  boss_adjlist <- lapply(strsplit(d$boss, ","), as.numeric)
  g <- add.edges(g, t(alist_to_elist(d$ego, boss_adjlist)), attr=list(boss=TRUE))
  E(g)$boss <- !is.na(E(g)$boss)
  # kolor
  pal <- brewer_pal(type="qual", palette=2)(length(unique(d$kolor)))
  V(g)$color <- pal[d$kolor]
  # shape
  shps <- c("circle", "square")
  V(g)$shape <- shps[d$ksztalt]
  # grupy
  l <- strsplit(d$grupa, ",")
  u <- na.omit(unique(unlist(l)))
  grupy <- lapply(u, function(gid) which(sapply(l, function(x) gid %in% x)))
  # rys!
  gb <- delete.edges(g, E(g)[!boss])
  gw <- as.undirected(delete.edges(g, E(g)[boss]))
  lay <- layout.fruchterman.reingold(g)
  plot(gb, layout=lay, vertex.shape="none", edge.curved=0.3,
       edge.color="black", mark.groups=grupy)
  plot(gw, layout=lay, add=TRUE, edge.color="red", edge.width=3)
}

# Zastosowanie
vis("WF01")


# TODO narysowanie wszystkich w pętli
