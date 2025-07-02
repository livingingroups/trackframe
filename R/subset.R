

# This simple wrapper allows that we allways use with = FALSE
"[.data.frame" <- function(x, i, j, drop = FALSE, ...)  {
    base::`[.data.frame`(x, i, j, drop = drop)
}


sc <- function(x, cols, ...) {
    UseMethod("sc")
}

sc.data.frame <- function(x, cols, drop = TRUE, ...) {
    if (length(cols) == 1L && isTRUE(drop)) {
        x[[cols]]    
    } else {
        x[, cols, drop = FALSE]
    }
}

sc.data.table <- function(x, cols, drop = TRUE, ...) {
    if (length(cols) == 1L && isTRUE(drop)) {
        x[[cols]]    
    } else {
        x[, cols, with = FALSE]
    }
}

sc.tibble <- function(x, cols, drop = TRUE, ...) {
    if (length(cols) == 1L && isTRUE(drop)) {
        x[[cols]]    
    } else {
        x[, cols]
    }
}
