#' Move variables
#'
#' Move a selection of variables in a tbl to a location relative to another
#' variable.
#'
#' The variable in the reference location will not be moved, even if it is
#' selected with `...`. If more than one variable is picked in the reference
#' location selection, `.before` uses the first selected variable as the
#' reference location, and `.after` uses the last.
#'
#' @inheritParams select
#' @param ... A selection of variables to move. Passed to [select()].
#' @param .before,.after A selection for a variable to use as the reference
#'   location. Exactly one of these arguments must be supplied.
#' @return An object of the same class as `.data`.
#' @seealso [select()], which `move()` wraps.
#' @export
#' @examples
#' iris <- as_tibble(iris) # so it prints a little nicer
#' move(iris, Petal.Length, .after = Sepal.Length)
#' move(iris, Sepal.Width, .before = Petal.Width)
#'
#' # Move many variables
#' move(iris, starts_with("Sepal"), .before = Species)
#' move(iris, starts_with("Sepal"), .after = starts_with("Petal"))
#'
#' # Move a variable to first
#' move(iris, Species, .before = everything())
#' move(iris, everything(), .after = Species)
#'
#' # Move a variable to last
#' move(iris, Sepal.Length, .after = last_col())
#' move(iris, everything(), .before = Sepal.Length)
move <- function(.data, ..., .before = NULL, .after = NULL) {
  where <- before_or_after({{ .before }}, {{ .after }})
  vars <- tbl_vars(.data)

  ref <- switch(where,
    after  = max(vars_select_pos(vars, {{ .after }})),
    before = min(vars_select_pos(vars, {{ .before }}))
  )

  sels <- vars_select_pos(vars, ...)
  sels <- switch(where,
    after  = c(ref, setdiff(sels, ref)),
    before = c(setdiff(sels, ref), ref)
  )

  head <- setdiff(seq_len(ref - 1L), sels)
  select(.data, !!!head, !!!sels, everything())
}

before_or_after <- function(.before, .after) {
  before_is_null <- quo_is_null(enquo(.before))
  after_is_null <- quo_is_null(enquo(.after))

  if (!xor(before_is_null, after_is_null)) {
    abort("Must supply either `.before` or `.after`.")
  }

  if (!before_is_null) "before" else "after"
}

vars_select_pos <- function(.vars, ...) {
  sels <- tidyselect::vars_select(.vars, ...)
  set_names(match(sels, .vars), names(sels))
}
