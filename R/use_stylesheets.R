# For Accessing GMRI Stylesheets


#' Use CSS File from gmRi package files - Rmd Implementation
#'
#' @param css_file String indicating desired css filename.
#'
#' @return
#' @export
#'
#' @examples # Not Run use_gmri_style(gmri_rmarkdown.css)
use_gmri_style_rmd <- function(css_file = "gmri_rmarkdown.css"){
  # css file
  file_name <- paste0("/", css_file)
  file_path <- paste0(system.file("stylesheets", package = "gmRi"), file_name)
  shiny::includeCSS(path = file_path)
}


#' Insert HTML Header File from gmRi package files - Rmd Implementation
#'
#' @param header_file String indicating desired css filename.
#'
#' @return
#' @export
#'
#' @examples # Not Run use_gmri_header(gmri_rmarkdown.css)
insert_gmri_header <- function(header_file = "gmri_logo_header.html"){
  # css file
  file_name <- paste0("/", header_file)
  file_path <- paste0(system.file("stylesheets", package = "gmRi"), file_name)
  shiny::includeHTML(path = file_path)
}


#' Insert HTML Header File from gmRi package files - Rmd Implementation
#'
#' @param footer_file String indicating desired css filename.
#'
#' @return
#' @export
#'
#' @examples # Not Run use_gmri_footer(gmri_rmarkdown.css)
insert_gmri_footer <- function(footer_file = "akemberling_gmri_footer.html"){
  # css file
  file_name <- paste0("/", footer_file)
  file_path <- paste0(system.file("stylesheets", package = "gmRi"), file_name)
  shiny::includeHTML(path = file_path)
}




#' Access GMRI Stylesheets - Shiny App Implementation
#'
#' @param stylesheet String identifying which stylesheet to attach to Rmarkdown or Shiny App
#' @param header Optional string identifying html footer to attach, default is NULL, "gmri logo right" example given
#' @param footer Optional string identifying htmml footer to attach, default is NULL, "akemberling" example included
#'
#' @return Function prints the css and html elements attached
#' @export
#'
#' @examples # Not Run
use_gmri_style_shiny <- function(stylesheet = "gmri rmarkdown", header = "none", footer = "none") {

  # Add the resource path to the stylesheets contained in the gmRi package
  # Allows access to stylesheets folder contents from shiny app using prefix: styles
  shiny::addResourcePath(
    prefix = "styles",
    directoryPath = system.file("stylesheets", package = "gmRi")
  )


  ####  Selecting Stylesheets  ####

  # Attach the gmri rmarkdown stylesheet
  if(stylesheet == "gmri rmarkdown"){
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = "styles/gmri_rmarkdown.css")
    )
    message("GMRI Rmarkdown CSS attached")
  }


  ####  Attaching a Footer  ####
  if(header == "gmri logo right"){
    shiny::tags$header(
      shiny::tags$link(rel = "header", href = "styles/gmri_logo_header.html")
    )
    message("GMRI Logo Right header attached")
  }

  # Attach a custon footer for things like personal github details
  # Each user would then add their own custom footer here, which will be attached if the footer option is chosen
  if(footer == "akemberling"){
    shiny::tags$footer(
      shiny::tags$link(rel = "footer", href = "styles/akemberling_gmri_footer.html")
    )
    message("AKemberling Footer attached")
  }

}
