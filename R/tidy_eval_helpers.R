#' Continuous summary
#' Quickly summarize variable count, mean, standard deviation, median, min, and max.
#' @param .data 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
continuous_summary <- function(.data, ...) {
  .data %>%
    tidyr::gather("Variable", "Value", ...) %>%
    group_by(Variable, add = TRUE) %>%
    summarise_at(
      "Value",
      list(
        N =      ~ length(.),
        mean =   ~ mean(.),
        sd =     ~ sd(.),
        median = ~ median(.),
        min =    ~ min(.),
        max =    ~ max(.)
      )
    ) %>%
    mutate(
      range = paste(min, "-", max),
      CV = 100 * sd / mean
    )
}

#' Tidyselect helper to sum across rows.
#' Wrangle rowSums() or rowMeans() into accepting tidyverselect helpers. 
#' @param .data
#' @param ...
#' @param .value
#' Specify the output variable name.
#' @param na.rm
#'
#' @return
#' @export
#'
#' @examples
tidyselect_row_sums <-
  function (.data,
            ...,
            .value = "row_sum",
            na.rm = FALSE) {
    dots <- exprs(...)
    value <- sym(.value)
    cols <- select(.data, !!!dots)
    out <- mutate(.data, !!value := rowSums(cols, na.rm = na.rm))
    return (out)
  }

#' Bare to quosure: quo
#'
#' @param x 
#' @param var 
#'
#' @return
#' @export
#'
#' @examples 
#' bare_to_quo(mtcars, quo(cyl))
bare_to_quo <- function(x, var){
  x %>% select(!!var) %>% head(1)
}

#' Bare to quosure in function: enquo
#'
#' @param x 
#' @param var 
#'
#' @return
#' @export
#'
#' @examples 
#' bare_to_quo_in_func(mtcars, mpg)
bare_to_quo_in_func <- function(x, var) {
  var_enq <- enquo(var)
  x %>% select(!!var_enq) %>% head(1)
}

#' Quosure to a name: quo_name
#'
#' @param x 
#' @param nm 
#'
#' @return
#' @export
#'
#' @examples 
#' bare_to_name(mtcars, quo(this_is_42))
bare_to_name <- function(x, nm) {
  nm_name <- quo_name(nm)
  x %>% mutate(!!nm_name := 42) %>% head(1) %>% 
    select(!!nm)
}

#' Quosure to text: quo_text
#'
#' @param x 
#' @param var 
#'
#' @return
#' @export
#'
#' @examples 
#' quo_to_text(mtcars, cyl)
quo_to_text <- function(x, var) {
  var_enq <- enquo(var)
  glue::glue("The following column was selected: {rlang::quo_text(var_enq)}")
}

#' character to name: sym
#'
#' @param x 
#' @param var 
#'
#' @return
#' @export
#'
#' @examples 
#' char_to_quo(mtcars, "vs")
char_to_quo <- function(x, var) {
  var_enq <- rlang::sym(var)
  x %>% select(!!var_enq) %>% head(1)
}

#' Multiple bares to quosures:quos
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
bare_to_quo_mult <- function(x, ...) {
  grouping <- quos(...)
  x %>% group_by(!!!grouping) %>% summarise(nr = n())
}

#' Multiple characters to names: syms
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
bare_to_quo_mult_chars <- function(x, ...) {
  grouping <- rlang::syms(...)
  x %>% group_by(!!!grouping) %>% summarise(nr = n())
}

#' Quoting full expression
#'
#' @param x 
#' @param filter_exp 
#'
#' @return
#' @export
#'
#' @examples
filter_func <- function(x, filter_exp) {
  filter_exp_enq <- enquo(filter_exp)
  x %>% filter(!!filter_exp_enq)
}
#' Quoting full expression in a character: parse_expr
#'
#' @param x 
#' @param char 
#'
#' @return
#' @export
#'
#' @examples
filter_by_char <- function(x, char) {
  func_call <- rlang::parse_expr(char)
  x %>% filter(!!func_call)
}
