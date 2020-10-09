## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(gmRi)

## ---- eval = FALSE-------------------------------------------------------
#  <img src='www/gmri_logo.png' align="right" height="44" />

## ---- echo = FALSE, eval = FALSE-----------------------------------------
#  # Add Header with gmRi Logo
#  gmRi::insert_gmri_header(header_file = "gmri_logo_header.html")

## ------------------------------------------------------------------------

# Access GMRI CSS Style
gmRi::use_gmri_style_rmd(css_file = "gmri_rmarkdown.css")

## ---- fig.height=6, fig.width=6------------------------------------------
mtcars$car <- rownames(mtcars) 
mtcars$cyl <- factor(mtcars$cyl)

ggplot(mtcars, aes(cyl, mpg, color = cyl)) +
  geom_boxplot() +
  scale_color_manual(
    values = setNames(gmri_cols(c("gmri blue", "orange", "teal"), as_char = TRUE),
                      c("4", "6", "8"))
  )

## ---- fig.height=6, fig.width=6------------------------------------------

ggplot(mtcars, aes(mpg, car, fill = car)) +
  geom_col() +
  scale_fill_gmri() +
  labs(y = NULL)

## ------------------------------------------------------------------------
# footer
gmRi::insert_gmri_footer(footer_file = "akemberling_gmri_footer.html")

