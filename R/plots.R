# OVERVIEW ----------------------------------------------------------------


# TODO --------------------------------------------------------------------


# EXPORTED FUNCTIONS ------------------------------------------------------

#' Plot a heatmap
#'
#' Plots a heatmap using geom_tile()
#'
#' @param df df
#' @param x character
#' @param y character
#' @param fill character
#' @param x_order chracter vector
#' @param y_order character vetor
#' @param xlab character
#' @param ylab character
#' @param legend_lab character
#' @param xlab_text_angle numeric
#' @param ylab_text_angle numeric
#' @param xlab_text_size numeric
#' @param ylab_text_size numeric
#' @param geom_tile_color character
#'
#' @return ggplot object
#' @export
plot_heatmap <- function(df,
                         x,
                         y,
                         fill,
                         x_order = NULL,
                         y_order = NULL,
                         xlab = "x title",
                         ylab = "y title",
                         legend_lab = "legend title",
                         xlab_text_angle = 90,
                         ylab_text_angle = 0,
                         xlab_text_size = NULL,
                         ylab_text_size = NULL,
                         geom_tile_color = "white") {

  # order x and y axes, if specified
  if (!is.null(x_order)) {
    df[[x]] <- factor(df[[x]], levels = x_order)
  }

  if (!is.null(y_order)) {
    df[[y]] <- factor(df[[y]], levels = y_order)
  }

  # plot
  df %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[x]], y = .data[[y]], fill = .data[[fill]])) +
    ggplot2::geom_tile(color = geom_tile_color) +
    # scale_fill_distiller(palette = palette) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = xlab_text_angle, size = xlab_text_size),
      axis.text.y = ggplot2::element_text(angle = ylab_text_angle, size = ylab_text_size),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
    ) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::labs(fill = legend_lab) +

    # make tiles square
    ggplot2::coord_equal() +

    # remove grey border around heatmap plot
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0))
}

# PRIVATE FUNCTIONS -------------------------------------------------------
