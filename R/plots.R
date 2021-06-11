# OVERVIEW ----------------------------------------------------------------


# TODO --------------------------------------------------------------------


# EXPORTED FUNCTIONS ------------------------------------------------------

#' Plot a heatmap
#'
#' Plots a heatmap using \code{\link[ggplot2]{geom_tile}}.
#'
#' TODO: read this \href{https://jcoliver.github.io/learn-r/006-heatmaps.html}{blogpost}
#'
#' @param df df
#' @param x character
#' @param y character
#' @param fill character
#' @param label character. Annotate tiles
#' @param label_size numeric. Text size for tile annotations
#' @param x_order chracter vector.
#' @param y_order character vetor
#' @param xlab character
#' @param ylab character
#' @param legend_lab character
#' @param xlab_text_angle numeric
#' @param ylab_text_angle numeric
#' @param xlab_text_size numeric
#' @param ylab_text_size numeric
#' @param xtitle_text_size numeric
#' @param ytitle_text_size numeric
#' @param legend_title_size numeric
#' @param legend_text_size numeric
#' @param geom_tile_color character
#' @param plotly_plot logical. Set as \code{TRUE} if wrapping this plot with
#'   \code{\link[plotly]{ggplotly}}. Default is \code{FALSE}.
#'
#' @return ggplot object
#' @export
plot_heatmap <- function(df,
                         x,
                         y,
                         fill,
                         label = NULL,
                         label_size = TRUE,
                         x_order = NULL,
                         y_order = NULL,
                         xlab = "x title",
                         ylab = "y title",
                         legend_lab = "legend title",
                         xlab_text_angle = 90,
                         ylab_text_angle = 0,
                         xlab_text_size = NULL,
                         ylab_text_size = NULL,
                         xtitle_text_size = NULL,
                         ytitle_text_size = NULL,
                         legend_title_size = NULL,
                         legend_text_size = NULL,
                         geom_tile_color = "white",
                         plotly_plot = FALSE) {
  # order and filter x/y axes, if specified
  if (!is.null(x_order)) {
    # error if no common items
    if (all(!x_order %in% df[[x]])) {
      stop("all values in x_order are not present in df[[x]]")
    }

    # warning if any items in x_order or NOT in df[[x]]
    if (any(!x_order %in% df[[x]])) {
      warning("x_order contains values that are not present in df[[x]]")
    }

    # filter
    df <- df %>%
      dplyr::filter(.data[[x]] %in% x_order)

    # order
    df[[x]] <- factor(df[[x]], levels = x_order)
  }

  if (!is.null(y_order)) {
    # error if no common items
    if (all(!y_order %in% df[[y]])) {
      stop("all values in y_order are not present in df[[x]]")
    }

    # warning if any items in x_order or NOT in df[[x]]
    if (any(!y_order %in% df[[y]])) {
      warning("y_order contains values that are not present in df[[y]]")
    }

    # filter
    df <- df %>%
      dplyr::filter(.data[[y]] %in% y_order)

    # order
    df[[y]] <- factor(df[[y]], levels = y_order)
  }

  # plot
  heatmap <- df %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[x]], y = .data[[y]], fill = .data[[fill]])) +
    ggplot2::geom_tile(color = geom_tile_color) +

    # axis and legend labels
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::labs(fill = legend_lab) +

    # formatting
    # scale_fill_distiller(palette = palette) +
    ggplot2::theme(
      axis.text.x = ggtext::element_markdown(angle = xlab_text_angle, size = xlab_text_size),
      axis.text.y = ggtext::element_markdown(angle = ylab_text_angle, size = ylab_text_size),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.x = ggtext::element_markdown(size = xtitle_text_size),
      axis.title.y = ggtext::element_markdown(size = ytitle_text_size),
      legend.title = ggtext::element_markdown(size = legend_title_size),
      legend.text = ggtext::element_markdown(size = legend_text_size),
    ) +

    # remove grey border around heatmap plot
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0))


  # make tiles square - this makes a big grey border if
  # using plotly::ggplotly though
  if (!plotly_plot) {
    heatmap <- heatmap + ggplot2::coord_equal()
  }

  # annotate tiles
  # warning if only one of these is specified
  # if (sum(is.null(label), is.null(label_size)) == 1) {
  #   warning("To annotate heatmap tiles, BOTH 'label' and 'label_size' arguments must be specified. No annotations will be produced in this plot")
  # }

  if (!is.null(label)) {
    heatmap <-
      heatmap + ggplot2::geom_text(ggplot2::aes(label = .data[[label]]),
                                   size = label_size)
  }

  # return heatmap
  return(heatmap)
}

# PRIVATE FUNCTIONS -------------------------------------------------------
