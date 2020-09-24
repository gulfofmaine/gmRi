# For Accessing GMRI Stylesheets

#' Access GMRI Stylesheets
#'
#' @param stylesheet String identifying which stylesheet to attach to Rmarkdown or Shiny App
#' @param header Optional string identifying html footer to attach, default is NULL, "gmri logo right" example given
#' @param footer Optional string identifying htmml footer to attach, default is NULL, "akemberling" example included
#'
#' @return Function prints the css and html elements attached
#' @export
#'
#' @examples # Not Run
use_gmri_stylesheets <- function(stylesheet = "gmri rmarkdown", header = NULL, footer = NULL) {

  # Add the resource path to the stylesheets contained in the gmRi package
  shiny::addResourcePath(
    "styles",
    system.file("stylesheets", package = "gmRi")
  )


  ####  Selecting Stylesheets  ####

  # Attach the gmri rmarkdown stylesheet
  if(stylesheet == "gmri rmarkdown"){
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = "gmri_rmarkdown.css")
    )
    print("GMRI Rmarkdown CSS attached")
  }


  ####  Attaching a Footer  ####
  if(header == "gmri logo right"){
    shiny::tags$header(
      shiny::tags$link(rel = "stylesheet", href = "gmri_logo_header.html")
    )
    print("GMRI Logo Right header attached")
  }

  # Attach a custon footer for things like personal github details
  # Each user would then add their own custom footer here, which will be attached if the footer option is chosen
  if(footer == "akemberling"){
    shiny::tags$footer(
      shiny::tags$link(rel = "stylesheet", href = "akemberling_gmri_footer.html")
    )
    print("AKemberling Footer attached")
  }

}
