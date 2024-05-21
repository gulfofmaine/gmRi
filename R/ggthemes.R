####  GGPlot themes using {bslib}, {thematic}, and {showtext}
# reference video: https://www.rstudio.com/resources/rstudioglobal-2021/theming-shiny-and-rmarkdown-with-thematic-and-bslib/
#

# To add gmri font to Rmarkdown simply use:
# showtext::shotext_auto() in an rmarkdown document following the addition of the stylesheet



# base settings from {ggthemes}
#' @title GMRI ggplot2 theme for blog-style plots
#'
#' @param base_size Base text size (font numbers)
#' @param base_family Base Text font family
#' @param title_family Title font family
#' @param facet_color Color label passed to gmRi::gmri_cols() to pick a color for facet tabs
#' @param ... Additional arguments to pass on the fly using ggplot2::theme()
#'
#' @return Returns ggplot theme
#' @export
#'
#' @examples ggplot2::ggplot(mtcars) + theme_gmri()
theme_gmri <- function(base_size = 10,
                       base_family  = "sans",
                       title_family = "sans",
                       facet_color  = "teal",
                       ...) {

  # Color from gmRi palette, sets background color for facet strips
  if(facet_color %in% c(
    "orange", "yellow", "gmri green", "light green", "dark green",
    "green", "teal", "blue", "gmri blue", "light gray", "dark gray")){
    facet_hex <- gmri_cols()[facet_color]
  } else {
    facet_hex <- facet_color
  }


  # Set up theme
  gmri_ggtheme <- ggthemes::theme_foundation(
    base_size   = base_size,
    base_family = base_family) +
    ggplot2::theme(

      # Major Elements
      line = ggplot2::element_line(linetype = 1, colour = "black"),
      rect = ggplot2::element_rect(
        fill = "transparent",
        linetype = 0,
        colour = NA),
      text  = ggplot2::element_text(colour = "black"),
      title = ggplot2::element_text(family = title_family, size = 12),

      # Axis elements
      axis.text.x  = ggplot2::element_text(colour = NULL),
      axis.text.y  = ggplot2::element_text(colour = NULL),
      axis.ticks   = ggplot2::element_line(colour = NULL),
      axis.ticks.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_line(colour = NULL),
      axis.line    = ggplot2::element_line(),
      axis.line.y  = ggplot2::element_blank(),
      axis.text    = ggplot2::element_text(size = 11),
      axis.title   = ggplot2::element_text(size = 12),

      # Legend Elements
      legend.background = ggplot2::element_rect(),
      legend.title      = ggplot2::element_text(size = 9),
      legend.text       = ggplot2::element_text(size = 9),

      # Panel/Grid Setup
      panel.grid = ggplot2::element_line(
        colour = NULL,
        linetype = 3,
        linewidth = 0.3,
        color = "gray80"),
      panel.grid.major   = ggplot2::element_line(colour = "black"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),

      # Title and Caption Details
      plot.title    = ggplot2::element_text(hjust = 0, face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 9),
      plot.caption  = ggplot2::element_text(size = 7.2,
                                            margin = ggplot2::margin(t = 20),
                                            color = "gray40"),
      plot.margin   = ggplot2::unit(c(1, 1, 2, 1), "lines"),

      # Facet Details
      strip.text = ggplot2::element_text(color = "white", face = "bold", size = 11),
      strip.background = ggplot2::element_rect(
        color = "transparent",
        fill = facet_hex,
        linewidth = 1,
        linetype="solid")) +

    # Add any additional theme calls to tweak on the fly
    ggplot2::theme(...)

  return(gmri_ggtheme)
}

# Testing
# ggplot(mtcars, aes(mpg, cyl)) + geom_point() + theme_gmri(panel.grid.major = element_line(color = "red"))



# Set theme up for maps
#' @title GGplot2 Map Theme
#'
#' @description A simple theme that works well for maps made with {sf}.
#' Black border all around, and redundant lat/lon labels removed.
#'
#'
#' @param ... Ellipse are passed to ggplot2::theme() function allowing users
#` to pass in additional arguments, or ones that override the defaults`
#'
#' @return Returns ggtheme
#' @export
#'
#' @examples ggplot2::ggplot(mtcars) + map_theme()
map_theme <- function(...){
  ggmap_theme <- list(
    ggplot2::theme(
      panel.border       = ggplot2::element_rect(color = "black", fill = NA),
      plot.background    = ggplot2::element_rect(color = "transparent", fill = "transparent"),
      line               = ggplot2::element_blank(),
      axis.title.x       = ggplot2::element_blank(), # turn off titles
      axis.title.y       = ggplot2::element_blank(),
      legend.title.align = 0.5,
      ...)
  )


  return(ggmap_theme)
}


# set standard publication-figure theme

# to use, add "+ GMRI_standard" to any ggplot object

# GMRI_standard <- theme_light() + theme(panel.background = element_blank(),
#                                        panel.grid       = element_blank(),
#                                        panel.border     = element_blank(),
#                                        axis.line        = element_line(colour = "#333333"),
#                                        axis.ticks       = element_line(color = "#333333"),
#                                        axis.text        = element_text(size = 13, color="#333333"),
#                                        axis.title       = element_text(size = 14, color="#333333"),
#                                        legend.text      = element_text(size = 13, color="#333333"),
#                                        legend.title     = element_text(size = 14, color="#333333"),
#                                        strip.background = element_blank(),
#                                        strip.text       = element_text(size = 13, color="#333333"))




# ####  Font Loading Tests:  ####
#
# # getting path to fonts from gmRi package, should work for any user:
# gmri_font_paths <- paste0(system.file("stylesheets", package = "gmRi"), "/Fonts/")
#
# # Adding the fonts is another challenge after that
# sysfonts::font_add("http://fast.fonts.net/t/1.css?apiType=css&projectid=806f61f6-d695-4965-a878-820b50bc0269")
#
# # doesn't work
# extrafont::ttf_import(paths = gmri_font_paths)
#


####  Things to Add  ####
# Watermarks: gmri logo



####  Logo Placement Testing:

# relevant links:
# https://github.com/tidyverse/ggplot2/issues/1244 ggplot relative placements
# placing plots within plots: https://www.christophenicault.com/post/npc_ggplot2/
# normalized parent coordinates ^



# # Make a simple plot for testing
# (simp <- ggplot(mpg, aes(displ, hwy, colour = class)) +
#   geom_point() +
#   ggtitle("Cars"))
#
#
#
#
# # Trying to use ggimage, must be within panel...
# #library(ggimage)
# simp + geom_image(aes(x = 0, y = 0, image = logo_path), by = "width",  size = 0.5, asp = 0.5)
#
#
# # Using grid:
# library(grid)
#
# #  wrap it in a grobTree from grid
# g <- grobTree(rasterGrob(image = gmri_logo,
#                          x=unit(0.9, "npc"),
#                          y=unit(0.925,"npc"),
#                          # height = unit(1, "cm"),
#                          # width = unit(3, "cm"),
#                          height = unit(0.05, "npc"),
#                          width = unit(0.137, "npc")
#                          )
#               )
#
# # This could be it! :
# simp +
#   coord_cartesian(clip = "off") + # use clip = "off" to get things to show up in the margins
#   annotation_custom(g)





#' @title Ggplot2 Raster Logo as Geom Layer
#'
#' @param logo Any R object that can be coerced to a raster object. Ex. class of "magick-image"
#' @param x_npc x positioning for logo in normalized parent coordinates 0-1 left to right
#' @param y_npc y positioning for logo in normalized parent coordinates 0-1 bottom to top
#' @param logo_height Desired height of the logo
#' @param height_units Units to set the height (and width) for the logo.
#' Default is NPC i.e. a fraction of the plot panel height
#' @param ... Extra control options passed to ggplot2::annotation_custom()
#'
#' @return Returns a  ggplot geom layer
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(cyl, hp)) + geom_point() + geom_logo(logo_height = 0.06)
geom_logo <- function(logo = NULL, x_npc = 0.9, y_npc = 0.925, logo_height = 0.06, height_units = "npc", ...){


  # Default logo is gmri_logo saved within the package
  if(is.null(logo)){
    logo_path <- paste0(system.file("stylesheets", package = "gmRi"), "/gmri_logo.png")
    logo <- magick::image_read(logo_path)
  }

  # Get image dimensions - to preserve aspect ratio:
  w <- magick::image_info(logo)$width
  h <- magick::image_info(logo)$height

  # Get the desired output height in npc units
  # npc = normalized parent coordinates
  npc_height <- logo_height
  npc_width  <- (npc_height * w) / h


  # Make the grob
  g <- grid::grobTree(
    grid::rasterGrob(image = logo,
                     x = grid::unit(x_npc, "npc"),
                     y = grid::unit(y_npc, "npc"),
                     height = grid::unit(npc_height, height_units),
                     width = grid::unit(npc_width, height_units)))

  # Make annotation geom
  geom_logo <- ggplot2::annotation_custom(g, ...)

  # Return the geom
  return(geom_logo)


}

# # Testing:
# simp + geom_logo(x_npc = 0.1, y_npc = -.1) + coord_cartesian(clip = "off")



