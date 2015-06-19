#============================================================================ 
# Additional vertex shapes
#============================================================================ 




#============================================================================ 
# Kwadracik z regulowaną grubością obramowania

vs_square_clip <- function(coords, el, params, end = c("both", "from", "to")) 
{
    end <- match.arg(end)
    if (length(coords) == 0) {
        return(coords)
    }
    vertex.size <- 1/200 * params("vertex", "size")
    square.shift <- function(x0, y0, x1, y1, vsize) {
        m <- (y0 - y1)/(x0 - x1)
        l <- cbind(x1 - vsize/m, y1 - vsize, x1 - vsize, y1 - 
            vsize * m, x1 + vsize/m, y1 + vsize, x1 + vsize, 
            y1 + vsize * m)
        v <- cbind(x1 - vsize <= l[, 1] & l[, 1] <= x1 + vsize & 
            y1 - vsize <= l[, 2] & l[, 2] <= y1 + vsize, x1 - 
            vsize <= l[, 3] & l[, 3] <= x1 + vsize & y1 - vsize <= 
            l[, 4] & l[, 4] <= y1 + vsize, x1 - vsize <= l[, 
            5] & l[, 5] <= x1 + vsize & y1 - vsize <= l[, 6] & 
            l[, 6] <= y1 + vsize, x1 - vsize <= l[, 7] & l[, 
            7] <= x1 + vsize & y1 - vsize <= l[, 8] & l[, 8] <= 
            y1 + vsize)
        d <- cbind((l[, 1] - x0)^2 + (l[, 2] - y0)^2, (l[, 3] - 
            x0)^2 + (l[, 4] - y0)^2, (l[, 5] - x0)^2 + (l[, 6] - 
            y0)^2, (l[, 7] - x0)^2 + (l[, 8] - y0)^2)
        t(sapply(seq(length = nrow(l)), function(x) {
            d[x, ][!v[x, ]] <- Inf
            m <- which.min(d[x, ])
            l[x, c(m * 2 - 1, m * 2)]
        }))
    }
    if (end %in% c("from", "both")) {
        vsize <- if (length(vertex.size) == 1) {
            vertex.size
        }
        else {
            vertex.size[el[, 1]]
        }
        res <- res1 <- square.shift(coords[, 3], coords[, 4], 
            coords[, 1], coords[, 2], vsize)
    }
    if (end %in% c("to", "both")) {
        vsize <- if (length(vertex.size) == 1) {
            vertex.size
        }
        else {
            vertex.size[el[, 2]]
        }
        res <- res2 <- square.shift(coords[, 1], coords[, 2], 
            coords[, 3], coords[, 4], vsize)
    }
    if (end == "both") {
        res <- cbind(res1, res2)
    }
    res
}


vs_square_plot <- function (coords, v = NULL, params) 
{
    vertex.color <- params("vertex", "color")
    if (length(vertex.color) != 1 && !is.null(v)) {
        vertex.color <- vertex.color[v]
    }
    vertex.frame.color <- params("vertex", "frame.color")
    if (length(vertex.frame.color) != 1 && !is.null(v)) {
        vertex.frame.color <- vertex.frame.color[v]
    }
    vertex.size <- 1/200 * params("vertex", "size")
    if (length(vertex.size) != 1 && !is.null(v)) {
        vertex.size <- vertex.size[v]
    }
    vertex.size <- rep(vertex.size, length = nrow(coords))

    vertex.frame.width <- params("vertex", "frame.width")
    if(length(vertex.frame.width) != 1 && !is.null(v)) {
      vertex.frame.width <- vertex.frame.width[v]
    }

    mapply(x = coords[, 1], y = coords[, 2], 
            vertex.color, 
            vertex.frame.color, 
            vertex.size,
            vertex.frame.width,
            FUN=function(x,y, bg, fg, size, lwd) { 
              symbols(x=x, y=y, bg=bg, fg=fg, lwd=lwd, squares=2*size,
                      add=TRUE, inches = FALSE)
            }
            )
}

# Register
add.vertex.shape("square", clip=vs_square_clip, plot=vs_square_plot,
                 parameters=list(vertex.frame.width=1))


#============================================================================ 
# Kółka z grubością obramowania


vs_circle_clip <- function (coords, el, params, end = c("both", "from", "to")) 
{
    end <- match.arg(end)
    if (length(coords) == 0) {
        return(coords)
    }
    vertex.size <- 1/200 * params("vertex", "size")
    if (end == "from") {
        phi <- atan2(coords[, 4] - coords[, 2], coords[, 3] - 
            coords[, 1])
        vsize.from <- if (length(vertex.size) == 1) {
            vertex.size
        }
        else {
            vertex.size[el[, 1]]
        }
        res <- cbind(coords[, 1] + vsize.from * cos(phi), coords[, 
            2] + vsize.from * sin(phi))
    }
    else if (end == "to") {
        phi <- atan2(coords[, 4] - coords[, 2], coords[, 3] - 
            coords[, 1])
        r <- sqrt((coords[, 3] - coords[, 1])^2 + (coords[, 4] - 
            coords[, 2])^2)
        vsize.to <- if (length(vertex.size) == 1) {
            vertex.size
        }
        else {
            vertex.size[el[, 2]]
        }
        res <- cbind(coords[, 1] + (r - vsize.to) * cos(phi), 
            coords[, 2] + (r - vsize.to) * sin(phi))
    }
    else if (end == "both") {
        phi <- atan2(coords[, 4] - coords[, 2], coords[, 3] - 
            coords[, 1])
        r <- sqrt((coords[, 3] - coords[, 1])^2 + (coords[, 4] - 
            coords[, 2])^2)
        vsize.from <- if (length(vertex.size) == 1) {
            vertex.size
        }
        else {
            vertex.size[el[, 1]]
        }
        vsize.to <- if (length(vertex.size) == 1) {
            vertex.size
        }
        else {
            vertex.size[el[, 2]]
        }
        res <- cbind(coords[, 1] + vsize.from * cos(phi), coords[, 
            2] + vsize.from * sin(phi), coords[, 1] + (r - vsize.to) * 
            cos(phi), coords[, 2] + (r - vsize.to) * sin(phi))
    }
    res
}


vs_circle_plot <- function (coords, v = NULL, params) 
{
    vertex.color <- params("vertex", "color")
    if (length(vertex.color) != 1 && !is.null(v)) {
        vertex.color <- vertex.color[v]
    }
    vertex.frame.color <- params("vertex", "frame.color")
    if (length(vertex.frame.color) != 1 && !is.null(v)) {
        vertex.frame.color <- vertex.frame.color[v]
    }
    vertex.size <- 1/200 * params("vertex", "size")
    if (length(vertex.size) != 1 && !is.null(v)) {
        vertex.size <- vertex.size[v]
    }
    vertex.size <- rep(vertex.size, length = nrow(coords))

    vertex.frame.width <- params("vertex", "frame.width")
    if(length(vertex.frame.width) != 1 && !is.null(v)) {
      vertex.frame.width <- vertex.frame.width[v]
    }

    mapply(x = coords[, 1], y = coords[, 2], 
            vertex.color, 
            vertex.frame.color, 
            vertex.size,
            vertex.frame.width,
            FUN=function(x,y, bg, fg, size, lwd) { 
              symbols(x=x, y=y, bg=bg, fg=fg, lwd=lwd, circles=size,
                      add=TRUE, inches = FALSE)
            }
            )
}


# Register
add.vertex.shape("circle", clip=vs_circle_clip, plot=vs_circle_plot,
                 parameters=list(vertex.frame.width=1))
