#' fim Institution palettes
#'
#' Vectors with hex-color codes that correspond to the color palettes outlined in the fim Style Guide.
#' x
#'
#' @source fim Style Guide
#' x
#'
#'
#' @family fim palettes
#' @rdname fim_palettes
#' @export
fim_colors <-
  c(# Pink
    headline = "#E7619F",
    # Purple
    state_purchases = "#AE68A9",
    # Blue
    federal_purchases = "#2198C7",
    # Green
    consumption = "#1B9553")
#' Function to extract fim colors as hex codes
#'
#' @param ... Character names of fim_colors
#'
fim_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (fim_colors)
  
  fim_colors[cols]
}

#' @rdname fim_palettes
#' @export
fim_palettes <- list(
  `headline` = fim_cols('headline'),
  `expanded`  = fim_cols('federal_purchases',
                         'state_purchases',
                         'consumption'))

#' Return function to interpolate a fim color palette
#'
#' @param palette Character name of palette in fim_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
fim_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- fim_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

#' Color scale constructor for fim colors
#'
#' @param palette Character name of fim_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or scale_color_gradientn(),
#'  used respectively when discrete is TRUE or FALSE
#'
#' @return
#' @export
#'
#' @examples
scale_color_fim <- function(palette = "headline", discrete = TRUE, reverse = FALSE, ...) {
  pal <- fim_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("fim_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for fim colors
#'
#' @param palette Character name of palette in fim_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @return
#' @export
#'
#' @examples
scale_fill_fim <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- fim_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("fim_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
