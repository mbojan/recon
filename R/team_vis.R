# 1. import danych z GS
# 2. przekształcenie danych w sieć
# 3. wizualizacja sieci
# sk <- "1IApsDIawqBGH1KuWpo22zJWAbrLJp5ft0oZDUuZ8UOs"


# Pobranie danych z GS
import_data <- function(sheet, sheet_key, ...)
{
  sh <- googlesheets::gs_key(sheet_key)
  googlesheets::gs_reshape_cellfeed(gs_read_cellfeed(sh, ws=sheet, ...))
}

if(FALSE) {
gs_auth()
sh <- gs_key(sk)
d <- import_data("WF03", sheet_key=sk, cell_limits(rows=c(2,NA), cols=c(1,7)))
}

# interpretacja danych z GS
# zwraca  listę ramek danych: osoby, grupy, relacje
interpret <- function(d, 
                      new_col_names=c("ego", "kolor", "ksztalt", "frame", "grupa",
                                  "wspolpracownicy", "boss"),
                      dir_cols="boss",
                      undir_cols="wspolpracownicy",
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
  grupy <- cbind(d$ego, d$grupa)
}

# interpret(d)



# zainstalować 'googlesheets' jeśli nie ma
# devtools::install_github("jennybc/googlesheets")
# library(googlesheets)
# library(igraph)
# library(scales)


# autoryzacja
# g_auth()
# gs_ls()

# sheet <- "WF01"
