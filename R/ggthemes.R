####  GGPlot themes using {bslib}, {thematic}, and {showtext}
# reference video: https://www.rstudio.com/resources/rstudioglobal-2021/theming-shiny-and-rmarkdown-with-thematic-and-bslib/
#

# To add gmri font to Rmarkdown simply use:
# showtext::shotext_auto() in an rmarkdown document following the addition of the stylesheet



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




# ####  Font Loading Testing:  ####
#
# # getting path to fonts from gmRi package, should work for any user:
# gmri_font_paths <- paste0(system.file("stylesheets", package = "gmRi"), "/Fonts/")
#
# # Adding  the fonts is another challenge after that
# sysfonts::font_add("http://fast.fonts.net/t/1.css?apiType=css&projectid=806f61f6-d695-4965-a878-820b50bc0269")
#
# # doesn't work
# extrafont::ttf_import(paths = gmri_font_paths)
#
#




# #' @title
# #'
# #' @description
# #'
# #' @param var_1
# #' @param var_2
# #'
# #' @return
# #' @export
# #'
# #' @examples
# my_function <- function(var_1, var_2){
#   print(var_1)
#
# }



####  Things to Add  ####
# Watermarks: gmri logo, gmri hex sticker
# map theme

