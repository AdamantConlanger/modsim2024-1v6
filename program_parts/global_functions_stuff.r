# a useful sublist/subvector maker that's auxiliary to the analysis module's cleanup subroutine
sane_slice <- function(x) {
    if (length(x) > 1) {
        return(x[-length(x)])
    } else if (length(x) == 1) {
        if (identical(x[NULL], list())) {
            return(list())
        } else {
            return(c())
        }
    } else {
        stop("Error: can't remove a value from a list or vector if it's empty, now can you?")
    }
} # returns x without its last entry. Why doesn't negative indexing in R work for singletons? I don't know.

# a useful sequence maker that's auxiliary to the analysis module's cleanup subroutine
sane_sequence <- function(from, to) {
    if (from <= to) {
        return(seq(from = from, to = to))
    } else {
        return(c())
    }
} # returns seq(from, to) if from <= to. Otherwise, returns c(). Really, why doesn't R's builtin stuff work like that?

# lambda calculations for calculating losses and for the Brier game and Vovk-Zhdanov algorithm
calculate_lambda <- function(guess, reality) {
    result <- 0
    # first, we naively act like none of the outcomes are the reality
    for (i in seq_along(guess)) {
        result <- result + guess[[i]]^2
    }
    # then, we compensate for the wrong value we have for i == reality
    result <- result - guess[[reality]]^2 + (1 - guess[[reality]])^2
    return(result)
} # lambda gives the square of the Euclidean distance between a Kronecker delta and a distribution.

# a useful seq_along/names combination that's auxiliary to the display module's readability section
names_or_seq_along <- function(x) {
    the_indices <- seq_along(x)
    the_names <- names(x)
    if (identical(the_names, NULL)) {
        the_names <- rep.int(list(""), times = length(x))
    }
    the_result <- list()
    for (index in the_indices) {
        if (identical(the_names[[index]], "")) {
            the_result[[index]] <- index
        } else {
            the_result[[index]] <- the_names[[index]]
        }
    }
    return(the_result)
} # returns a list-version of seq_along(x), but with name instead of index wherever defined.

# a useful integer checker that's auxiliary to a few things, because "is.integer" isn't an integer checker
sane_is_integer <- function(x, tol = .Machine$double.eps^0.5) {
    return(abs(x - round(x)) < tol)
} # returns whether x is an integer or not.

# a useful list modifier that's auxiliary to a lot of configuration stuff.
sane_modify_list <- function(x, val) {
    stopifnot(is.list(x), is.list(val))
    xnames <- names(x)
    vnames <- names(val)
    vnames <- vnames[nzchar(vnames)]
    for (v in vnames) {
        x[[v]] <- val[[v]]
    }
    x
} # overwrites or appends named entries of val into x.
