####  Source:  ####
#https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2

####  Colors  ####
gmri_colors <- c(
`seafood purple`     = "#773891",
`midnight blue`      = "#363b45",
`dark blue`          = "#004966",
`gmri blue`          = "#00608a",
`ecosystems cyan`    = "#07a3b7",
`blue economy teal`  = "#057872",
`moss green`         = "#38431d",
`warm yellow`        = "#ebcb27",
`lv orange`          = "#ea4f12",
`climate change red` = "#b94a40",
`light gray`         = "#E9E9E9",
`dark gray`          = "#535353"
)

#' @title Retrieve GMRI Hex Color(s)
#'
#' @description Return hex codes in return for named GMRI colors.
#' Available options are: seafood purple, midnight blue, dark blue, gmri blue,
#' ecosystems cyan, blue economy teal, moss green, warm yellow, lv orange, climate
#' change red, light gray, & dark gray.
#'
#' @param ... Character names of official GMRI colors
#' @export
#'
#' @examples
#'
#' #Pull a single Hex code
#' gmri_cols("gmri blue")
#'
#' #Multiple colors
#' gmri_cols("gmri blue", "moss green")
#'
#' #Get the whole list of colors
#' gmri_cols()
#'
#' #Add select colors to ggplot3 functions
#' ggplot2::ggplot(mtcars, ggplot2::aes(hp, mpg)) +
#'   ggplot2::geom_point(color = gmri_cols("gmri blue"), size = 4, alpha = .8)
#'
gmri_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols)){
      return (gmri_colors)
  }


  # If nothing is entered into the function return them all
  return_cols <- gmri_colors[cols]
  names(return_cols) <- NULL
  return_cols
}





####  Palettes  ####
gmri_palettes <- list(
  # Main palette
  `main`  = gmri_cols("gmri blue", "ecosytem cyan", "moss green", "warm yellow", "lv orange"),

  # Cool palette
  `cool`  = gmri_cols("midnight blue", "gmri blue", "moss green", "ecosystem cyan"),

  # Hot palette
  `hot`   = gmri_cols("warm yellow", "lv orange", "climate change red"),

  # Mixed palette
  `mixed` = gmri_cols("lv orange", "warm yellow", "moss green", "gmri blue", "dark blue", "seafood purple"),

  # Gray
  `gray`  = gmri_cols("light gray", "dark gray"),

  # Grey for british people
  `grey`  = gmri_cols("light gray", "dark gray")
)



#' @title GMRI Color Palettes
#'
#' @description Interpolation tool for a gmri color palettes using
#' colorRampPalette(). This is the workhorse for scale_color_gmri()
#' and for scale_fill_gmri().
#'
#' @param palette Character name of palette in gmri_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#' @export
#'
#' @examples
#' #You can now get interpolated color ranges using the palettes
#' gmri_pal("cool")(10)
#'
gmri_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- gmri_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}


#' @title GMRI-ggplot2 Scale Color Palettes
#'
#' @param palette Character name of palette in gmri_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#' @export
#'
#' @examples
#' #Color by discrete variable using default palette
#' # ggplot2::ggplot(iris, ggplot2::aes(Sepal.Width, Sepal.Length, color = Species)) +
#' #  ggplot2::geom_point(size = 4) +
#' #  scale_color_gmri()
#'
#' #Color by numeric variable with cool palette
#' # ggplot2::ggplot(iris, ggplot2::aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#' #  ggplot2::geom_point(size = 4, alpha = .6) +
#' #  scale_color_gmri(discrete = FALSE, palette = "cool")
#'
scale_color_gmri <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- gmri_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("gmri_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}




#' @title GMRI-ggplot2 Scale Fill Palettes
#'
#' @param palette Character name of palette in gmri_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#' @export
#'
#' @examples
#' #Fill by discrete variable with different palette + remove legend (guide)
#' #ggplot2::ggplot(mpg, ggplot2::aes(manufacturer, fill = manufacturer)) +
#' #  ggplot2::geom_bar() +
#' #  ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#' #  scale_fill_gmri(palette = "mixed", guide = "none")
#'
scale_fill_gmri <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- gmri_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("gmri_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}



####  Blog theme

# # Adding Logo to plot
#
# logo_path <- paste0(system.file("stylesheets", package = "gmRi"), "/gmri_logo.png")
# lab_logo <- magick::image_read(logo_path)
# (test_plot <- ggplot(palmerpenguins::penguins, aes(bill_length_mm, flipper_length_mm, color = species)) +
#     geom_point(show.legend = F) + labs(x = "Bill length", y = "Flipper Length", subtitle = "Penguin Example Plot",
#                                        caption = "Source: Gulf of Maine Research Institute") +
#     gmRi::scale_color_gmri() +
#     theme(plot.caption = element_text(colour = "gray25", size = 6)))
# grid::grid.raster(lab_logo, x = 0.08, y = 0.03, just = c('left', 'bottom'), width = unit(0.8, 'inches'))
