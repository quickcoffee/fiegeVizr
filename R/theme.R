#' @inheritParams ggplot2::theme_light
#' @param bg_color The background color of plot. One of \code{'grey 1',
#' 'grey 2', 'dark grey', 'white'}.
#' @param text_color The color of text in the plot. Hex or color-names.
#' @param base_family Plot base font family.
#' @param title_family Plot title font family.
#' @export
#' @importFrom ggplot2 element_line element_rect element_text element_blank rel

fiege_theme <- function (base_size = 12,
                         base_family = "",
                         title_family = "",
                         bg_color = "grey1",
                         text_color = "black") {
  colorhex <- fiegeVizr::fiege_palettes$bg[bg_color]
  theme_light(base_size = base_size,
              base_family = base_family) %+replace%
    theme(
      line = element_line(
        linetype = 1,
        colour = "black",
        size = 1.5
      ),
      rect = element_rect(
        fill = colorhex,
        linetype = 0,
        colour = NA
      ),
      text = element_text(colour = text_color),
      plot.title = element_text(
        size = 14,
        family = title_family,
        face = "bold",
        hjust = 0,
        vjust = 2
      ),
      axis.text = element_text(face = "bold", size = rel(1)),
      legend.background = element_rect(),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.box = "vertical",
      legend.key = element_rect(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(),
      plot.margin = unit(c(1, 1, 1, 1), "lines"),
      strip.background = element_rect()
    )
}
