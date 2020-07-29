#create FIEGE corporate colors vector
fiege_colors <- c(
  `red`        = "#B82136",
  `dark gray`      = "#4D4D4D",
  `light grey`       = "#B9B9BA",
  `pink`     = "#FE325A",
  `green`     = "#21BAA6",
  `rose` = "#E3A6AF",
  `orange grey`  = "#C19356",
  `brown` = "#BA7321",
  `black` = "#000000"
)

#helper functions
#' Function to extract fiege colors as hex codes
#'
#' @param ... Character names of fiege_colors
#' @export
fiege_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (fiege_colors)

  fiege_colors[cols]
}

#create fiege color palettes
fiege_palettes <- list(
  `main`  = fiege_cols("red", "light grey", "dark grey"),

  `roses`  = fiege_cols("red", "rose"),

  `hot`   = fiege_cols("red", "pink", "rose"),

  `mixed` = fiege_cols("red", "light grey", "dark grey", "pink", "green", "rose", "orange grey", "brown"),

  `grey`  = fiege_cols("light grey", "dark grey", "black")
)

#' Return function to interpolate a fiege color palette
#'
#' @param palette Character name of palette in fiege_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#' @export
fiege_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- fiege_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

#' Color scale constructor for fiege colors
#'
#' @param palette Character name of palette in fiege_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#' @export
scale_color_fiege <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- fiege_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("fiege_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for fiege colors
#'
#' @param palette Character name of palette in fiege_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#' @export
scale_fill_fiege <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- fiege_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("fiege_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
