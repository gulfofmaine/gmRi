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



