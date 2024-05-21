#### NEFSC Trawl Data Access  ####
####
#### NEFSC Trawl Data - Size Spectra Build
#### 3/24/2021
####
#### Objective:
#### Load and clean any survdat pull, without loss of columns. Additional steps to add LW information
#### And to get area expanded (stratified) abundances and biomasses




####  _____Load Packages____  ####





######################################################_

#' @title  Tidy the Survdat Dataset
#' @description Processing function to tidy/prepare the "survdat" groundfish survey dataset received
#' from the Northeast Fisheries Science Center. This function performs all common steps done when
#' preparing the data for any analyses that rely on abundance or biomass by species and the details
#' of where/when they were caught.
#'
#' This function will by default load the most up-to-date version of the dataset that has been
#' received from the NEFSC using survdat = NULL. Optionally, users may provide a dataframe
#' from the environment to be prepared using the same steps.
#'
#' The processing steps performed by this function include:
#'
#'  - loading a specific survdat dataset: "most recent" loads the most current and complete dataset.
#'  "bigelow" returns data sampled only by the RV bigelow, in its raw form, with no adjustments to
#'  catch that transform numbers to be more consistent with the RV albatross. "bio" loads the
#'  biological dataset, which contains additional details that require follow-up lab procedures like
#'  age information
#'
#'  - Flag and create any columns that are missing or inconsistent with how the dataset has been
#'  sent over time. Messages will appear in the terminal to accompany any columns created or modified
#'
#'  -  Perform column formatting: length and biomass are renamed to be unit specific length_cm &
#'  biomass_kg. Survey stratum numbers are pulled from the longer stratum field, these are used to
#'  match up to the fields of the shapefiles for them. comname values are converted to be all
#'  lowercase. The id field is formatted to not read as scientific, svspp is treated as a string.
#'
#'  - Perform row filtering: eliminate stratum that are no longer sampled or sampled inconsistently
#'  (values less than 01010 or greater than 01760 removed, in addition to 1310, 1320, 1330, 1350,
#'  1410, 1420, & 1490). Any rows without abundance or biomass information are dropped.
#'  Select species codes are also removed (0, 285-299, 305, 306, 307, 316, 323, 910-915, 955-961,
#'  978, 979, 980, 998)
#'
#'  - Perform spatial filters: Data is kept for all strata within these major regional definitions:
#'  "Georges Bank" = 13-23, "Gulf of Maine" = 24-40, "Southern New England"  01-12,
#'  "Mid-Atlantic Bight" = 61-76.
#'
#'  - Perform numlen (numbers at length) adjustment: numlen is not adjusted to correct for the
#'  change in survey vessels and gear that happened in 2008. These values consequently are not
#'  equal to the overall abundance of a species, nor total biomass of a species which are
#'  systematically adjusted to adjust for the gear change.
#'
#'  Because of this and also some instances of bad data, there are cases where more/less fishes are
#'  measured than initially tallied* in the abundance field. This section ensures that the numlen
#'  totals for a station & species are equal to abundance column (which has been adjusted already
#'  for the gear change.)
#'
#'  - Remove any duplicate records: One final step is the verification that any duplicated records
#'  are removed.
#'
#'
#' @param survdat optional starting dataframe in the R environment to run through size spectra build.
#' @param survdat_source String indicating which survdat file to load from box
#' @param box_location String indicating value to pass to `boxpath_switch`
#'
#' @return Returns a dataframe filtered and tidy-ed for size spectrum analysis.
#' @export
#'
#' @examples
#' # not run
#' # gmri_survdat_prep(survdat_source = "most recent")
gmri_survdat_prep <- function(survdat = NULL, survdat_source = "most recent", box_location = "root|cloudstorage"){

  # Switch for mac users with different box storage location
  path_fun <- boxpath_switch(box_location = box_location)

  ####  Resource Paths
  mills_path  <- path_fun("mills")
  nmfs_path   <- path_fun("res", "NMFS_trawl")



  ####  1. Import SURVDAT File  ####

  # Testing:
  #survdat_source <- "bigelow"     ; survdat <- NULL
  #survdat_source <- "most recent" ; survdat <- NULL

  # convenience change to make it lowercase
  survdat_source <- tolower(survdat_source)


  # Build Paths to survdat for standard options
  survdat_path <- switch(
    EXPR = survdat_source,
    "2016"        = paste0(mills_path, "Projects/WARMEM/Old survey data/Survdat_Nye2016.RData"),
    "2019"        = paste0(nmfs_path,  "SURVDAT_archived/Survdat_Nye_allseason.RData"),
    "2020"        = paste0(nmfs_path,  "SURVDAT_archived/Survdat_Nye_Aug 2020.RData"),
    "2021"        = paste0(nmfs_path,  "SURVDAT_archived/survdat_slucey_01152021.RData"),
    "bigelow"     = paste0(nmfs_path,  "SURVDAT_current/survdat_Bigelow_slucey_01152021.RData"),
    "most recent" = paste0(nmfs_path,  "SURVDAT_current/NEFSC_BTS_all_seasons_03032021.RData"),
    "bio"         = paste0(nmfs_path,  "SURVDAT_current/NEFSC_BTS_2021_bio_03192021.RData") )



  # If providing a starting point for survdat pass it in:
  if(is.null(survdat) == FALSE){
    survdat <- as.data.frame(survdat)
    trawldat <- janitor::clean_names(survdat)
  } else if(is.null(survdat) == TRUE){

    # If not then load using the correct path
    load(survdat_path)
    survdat <- as.data.frame(survdat)


    # Bigelow data doesn't load in as "survdat"
    if(survdat_source == "bigelow"){
      survdat <- survdat.big
      survdat <- as.data.frame(survdat)
      rm(survdat.big)}

    # Most recent pulls load a list containing survdat
    if(survdat_source %in% c("bio", "most recent")){
      survdat <- survey$survdat
      survdat <- as.data.frame(survdat)}

    # clean names up for convenience
    trawldat <- janitor::clean_names(survdat)
  }

  # remove survdat once the data is in
  rm(survdat)



  #### 2.  Column Detection  ####

  ####__ a. Missing column flags  ####

  # Flags for missing columns that need to be merged in or built
  has_comname  <- "comname"     %in% names(trawldat)
  has_id_col   <- "id"          %in% names(trawldat)
  has_towdate  <- "est_towdate" %in% names(trawldat)
  has_month    <- "est_month"   %in% names(trawldat)

  # Flags for renaming or subsetting the data due to presence/absence of columns
  has_year      <- "est_year"      %in% names(trawldat)
  has_catchsex  <- "catchsex"      %in% names(trawldat)
  has_decdeg    <- "decdeg_beglat" %in% names(trawldat)
  has_avg_depth <- "avgdepth"      %in% names(trawldat)







  ####__ b. Missing comname  ####

  # Use SVSPP to get common names for species
  if(has_comname == FALSE){
    message("no comnames found, merging records in with spp_keys/sppclass.csv")

    # Load sppclass codes and common names
    spp_classes <- readr::read_csv(
      paste0(nmfs_path, "spp_keys/sppclass.csv"),
      col_types = readr::cols())
    spp_classes <- janitor::clean_names(spp_classes)
    spp_classes <- dplyr::mutate(
      .data = spp_classes,
      comname  = stringr::str_to_lower(common_name),
      scientific_name = stringr::str_to_lower(scientific_name))
    spp_classes <- dplyr::distinct(spp_classes, svspp, comname, scientific_name)


    # Add the common names over and format for rest of build
    trawldat <- dplyr::mutate(trawldat, svspp = stringr::str_pad(svspp, 3, "left", "0"))
    trawldat <- dplyr::left_join(trawldat, spp_classes, by = "svspp")

  }


  ####__ c. Missing ID  ####
  if(has_id_col == FALSE) {
    message("creating station id from cruise-station-stratum fields")
    # Build ID column
    trawldat <- dplyr::mutate(
      .data = trawldat,
      cruise6 = stringr::str_pad(cruise6, 6, "left", "0"),
      station = stringr::str_pad(station, 3, "left", "0"),
      stratum = stringr::str_pad(stratum, 4, "left", "0"),
      id      = stringr::str_c(cruise6, station, stratum))}


  ####__ d. Field renaming  ####

  # Rename select columns for consistency
  if(has_year == FALSE)      {
    message("renaming year column to est_year")
    trawldat <- dplyr::rename(trawldat, est_year = year) }
  if(has_decdeg == FALSE) {
    message("renaming lat column to decdeg_beglat")
    trawldat <- dplyr::rename(trawldat, decdeg_beglat = lat) }
  if(has_decdeg == FALSE) {
    message("renaming lon column to decdeg_beglon")
    trawldat <- dplyr::rename(trawldat, decdeg_beglon = lon) }
  if(has_avg_depth == FALSE)      {
    message("renaming depth column to avgdepth")
    trawldat <- dplyr::rename(trawldat, avgdepth = depth) }



  ####____ d. build date structure for quick grab of date components
  if(has_towdate == TRUE) {
    message("building month/day columns from est_towdate")
    trawldat <- dplyr::mutate(
      .data = trawldat,
      est_month = stringr::str_sub(est_towdate, 6,7),
      est_month = as.numeric(est_month),
      est_day   = stringr::str_sub(est_towdate, -2, -1),
      est_day   = as.numeric(est_day), .before = season)}



  #### 4. Column Changes  ####


  # a. Text Formatting
  trawldat <- dplyr::mutate(
    .data = trawldat,
    comname = tolower(comname),
    id      = format(id, scientific = FALSE),
    svspp   = as.character(svspp),
    svspp   = stringr::str_pad(svspp, 3, "left", "0"),
    season  = stringr::str_to_title(season),

    # Format Stratum number,
    # exclude leading and trailing codes for inshore/offshore,
    # used for matching to stratum areas
    strat_num = stringr::str_sub(stratum, 2, 3))


  # b. Rename to make units more clear
  trawldat <- dplyr::rename(
    .data = trawldat,
    biomass_kg = biomass,
    length_cm  = length)

  # c. Replace 0's that must be greater than 0
  trawldat <- dplyr::mutate(
    .data = trawldat,
    biomass_kg = ifelse(biomass_kg == 0 & abundance > 0, 0.0001, biomass_kg),
    abundance  = ifelse(abundance == 0 & biomass_kg > 0, 1, abundance))






  #### 5. Row Filtering  ####

  # Things filtered:
  # 1. Strata
  # 2. Seasons
  # 3. Year limits
  # 4. Vessels
  # 5. Species Exclusion

  # Eliminate Canadian Strata and Strata No longer in Use
  trawldat <- dplyr::filter(
    .data = trawldat,
    stratum >= 01010,
    stratum <= 01760,
    stratum != 1310,
    stratum != 1320,
    stratum != 1330,
    stratum != 1350,
    stratum != 1410,
    stratum != 1420,
    stratum != 1490)


  # Drop NA Biomass and Abundance Records
  trawldat <- dplyr::filter(
    .data = trawldat,
    !is.na(biomass_kg),
    !is.na(abundance))

  # Exclude the Skrimps
  trawldat <-  dplyr::filter(
    .data = trawldat,
    svspp %not in% c(285:299, 305, 306, 307, 316, 323, 910:915, 955:961))

  # Exclude the unidentified fish
  trawldat <- dplyr::filter(trawldat, svspp %not in% c(0, 978, 979, 980, 998))


  # # Restrict to only the Albatross and Henry Bigelow? - eliminates 1989-1991
  # trawldat_t <- dplyr::filter(trawldat, svvessel %in% c("AL", "HB"))






  #### 6. Spatial Filtering - Stratum  ####

  # This section merges stratum area info in
  # And drops stratum that are not sampled or in Canada
  # these are used to relate catch/effort to physical areas in km squared

  # Stratum Area Key for which stratum correspond to larger regions we use
  strata_key <- list(
    "Georges Bank"          = as.character(13:23),
    "Gulf of Maine"         = as.character(24:40),
    "Southern New England"  = stringr::str_pad(as.character(1:12),
                                               width = 2, pad = "0", side = "left"),
    "Mid-Atlantic Bight"    = as.character(61:76))


  # Add the labels to the data
  trawldat <- dplyr::mutate(
    trawldat,
    survey_area =  dplyr::case_when(
      strat_num %in% strata_key$`Georges Bank`         ~ "GB",
      strat_num %in% strata_key$`Gulf of Maine`        ~ "GoM",
      strat_num %in% strata_key$`Southern New England` ~ "SNE",
      strat_num %in% strata_key$`Mid-Atlantic Bight`   ~ "MAB",
      TRUE                                             ~ "stratum not in key"))


  # Use strata_select to pull the strata we want individually
  strata_select <- c(strata_key$`Georges Bank`,
                     strata_key$`Gulf of Maine`,
                     strata_key$`Southern New England`,
                     strata_key$`Mid-Atlantic Bight`)


  # Filtering areas using strata_select
  trawldat <- dplyr::filter(trawldat, strat_num %in% strata_select)
  trawldat <- dplyr::mutate(trawldat, stratum = as.character(stratum))





  #### 7. Adjusting NumLength  ####

  # NOTE:
  # numlen is not adjusted to correct for the change in survey vessels and gear
  # these values consequently do not equal abundance, nor biomass which are adjusted

  # Because of this and also some instances of bad data,
  # there are cases of more/less measured than initially tallied* in abundance
  # this section ensures that numlen totals out to be the same as abundance


  # If catchsex is not a column then total abundance is assumed pooled
  if(has_catchsex == TRUE){
    abundance_groups <- c("id", "comname", "catchsex", "abundance")
  } else {
    message("catchsex column not found, ignoring sex for numlen adjustments")
    abundance_groups <- c("id", "comname", "abundance")}


  # Get the abundance value for each sex
  # arrived at by summing across each length
  abundance_check <- dplyr::group_by(trawldat, !!!rlang::syms(abundance_groups))
  abundance_check <- dplyr::summarise(
    .data = abundance_check,
    abund_actual = sum(numlen),
    n_len_class  = dplyr::n_distinct(length_cm),
    .groups      = "drop")


  # Get the ratio between the original abundance column
  # and the sum of numlen we just grabbed
  conv_factor <- dplyr::distinct(trawldat, !!!rlang::syms(abundance_groups), length_cm)
  conv_factor <- dplyr::inner_join(conv_factor, abundance_check, by = abundance_groups)
  conv_factor <- dplyr::mutate(conv_factor, convers = abundance / abund_actual)



  # Merge back and convert the numlen field
  # original numlen * conversion factor = numlength adjusted
  survdat_processed <- dplyr::left_join(trawldat, conv_factor, by = c(abundance_groups, "length_cm"))
  survdat_processed <- dplyr::mutate(survdat_processed, numlen_adj = numlen * convers, .after = numlen)
  survdat_processed <- dplyr::select(survdat_processed, -c(abund_actual, convers))


  # remove conversion factor from environment
  rm(abundance_check, conv_factor, strata_key, strata_select)





  #### 8. Distinct Station & Species Length Info   ####

  # For each station we need unique combinations of
  # station_id, species, catchsex, length_cm, adjusted_numlen
  # to capture what and how many of each length fish is caught

  # Record of unique station catches:
  # One row for every species * sex * length_cm, combination in the data
  trawl_lens <- dplyr::filter(
    .data = survdat_processed,
    is.na(length_cm) == FALSE,
    is.na(numlen) == FALSE,
    numlen_adj > 0)


  # Do we want to just keep all the station info here as well?
  # question to answer is whether any other columns repeat,
  # or if these are the only ones
  trawl_clean <- dplyr::distinct(
    .data = trawl_lens,
    id, svspp, comname, catchsex, abundance, n_len_class,
    length_cm, numlen, numlen_adj, biomass_kg, .keep_all = TRUE)




  # Return the dataframe
  # Contains 1 Row for each length class of every species caught
  return(trawl_clean)

}




######################################################_

# Test
# test_survdat <- gmri_survdat_prep(survdat_source = "most recent")



######################################################__####

######################################################_

#' @title Add species length weight information to cleaned SURVDAT trawl data
#'
#' @description calculate expected biomass-at-length for species based on
#' published length-weight relationships.
#'
#' Species are matched against a spreadsheet containing length and weight information from 2
#' sources. The first source is the length-weight relationships detailed in Wigley et al. 2003:
#' "Length-weight relationships for 74 fish species collected during NEFSC research vessel bottom
#' trawl surveys, 1992-99".
#'
#' The second source for matching growth details to species is fishbase. These values are known to
#' be potentially less accurate or less regionally specific.
#'
#' Pairings are first checked against the Wigley paper, and then by fishbase, to provide preference
#' to the more regionally focused source.
#'
#' @param survdat_clean Survdat data, after usual preparations are completed.
#' These include removal of old strata, labeling of areas of interest, and inclusion
#' of the annual effort in each.
#' @param cutoff Flag for whether to remove species that have shown poor fit
#' when compared to biomass data from the nmfs survey.
#' @param box_location String indicating value to pass to `boxpath_switch`
#'
#' @return survdat dataframe containing paired length-weight relationship details for species
#' @export
#'
#' @examples
#' # not run
#' # add_lw_info(survdat_clean = survdat_clean, cutoff = T)
add_lw_info <- function(survdat_clean, cutoff = FALSE, box_location = "root|cloudstorage"){


  #### 1. Match Species to LW Coefficients  ####

  # Switch for mac users with different box storage location
  path_fun <- boxpath_switch(box_location = box_location)

  ####  Resource Paths
  nmfs_path   <- path_fun(box_group = "RES_Data", subfolder = "NMFS_trawl")
  lw_key_path <- paste0(nmfs_path, "length_weight_keys/fishbase_wigley_combined_key.csv")

  # This table is a combined table of wigley and fishbase L-W coefficients
  lw_combined <- readr::read_csv(lw_key_path, col_types = readr::cols())
  lw_combined <- dplyr::mutate(lw_combined,
                               svspp = stringr::str_pad(svspp, 3, "left", "0"),
                               season = stringr::str_to_title(season))


  # Do a priority pass with the dplyr::filter(lw_combined, source == "wigley)
  # merge on comname, season, and catchsex
  wigley_coefficients <- dplyr::filter(lw_combined, source == "wigley")
  wigley_coefficients <- dplyr::select(wigley_coefficients,
                                       source, season, svspp, comname, scientific_name, spec_class,
                                       hare_group, fishery, catchsex, a, b, ln_a)


  # Do a second pass with the dplyr::filter(lw_combined, source == "fishbase")
  # merge on common names only
  fishbase_coefficients <- dplyr::filter(lw_combined, source == "fishbase")
  fishbase_coefficients <- dplyr::select(fishbase_coefficients,
                                         source, -svspp, comname, scientific_name, spec_class,
                                         hare_group, fishery, a, b, ln_a)


  # Mismatched svspp codes
  wigley_lookup <- function(x){unique(wigley_coefficients$svspp[which(wigley_coefficients$comname == x)])}
  survdat_clean <- dplyr::mutate(
    .data = survdat_clean,
    svspp = dplyr::if_else(comname == "scup", wigley_lookup("scup"), svspp))




  # First Pass - Wigley
  # Join just by svspp to account for name changes
  pass_1 <- dplyr::select(survdat_clean, -comname)
  pass_1 <- dplyr::inner_join(pass_1, wigley_coefficients)


  # Second Pass - Fishbase, for the stragglers if any
  pass_2 <- dplyr::filter(survdat_clean,
                          comname %not in% wigley_coefficients$comname,
                          svspp %not in% pass_1$svspp)
  pass_2 <- dplyr::inner_join(pass_2, fishbase_coefficients)


  # Join them with bind rows (implicitly drops things that don't have growth coefs)
  trawl_weights <- dplyr::bind_rows(pass_1, pass_2)
  trawl_weights <- dplyr::arrange(trawl_weights, est_year, season)
  trawl_weights <- dplyr::mutate(trawl_weights,
                                 b             = as.numeric(b),
                                 a             = as.numeric(a),
                                 a             = ifelse(is.na(a) & !is.na(ln_a), exp(ln_a), a),
                                 ln_a          = ifelse(is.na(ln_a), log(a), ln_a),  # log of a used if ln_a is isn't already there (some fish just had ln_a reported)
                                 llen          = log(length_cm),
                                 ind_log_wt    = ln_a + (b * llen),
                                 ind_weight_kg = exp(ind_log_wt),                # weight of an individual in size class
                                 sum_weight_kg = ind_weight_kg * numlen_adj)     # Individual weight * adjusted numlen
  trawl_weights <- tidyr::drop_na(trawl_weights, ind_weight_kg)
  trawl_weights <- dplyr::select(trawl_weights, -ind_log_wt, -llen)


  # clean up environment
  rm(pass_1, pass_2, fishbase_coefficients, wigley_coefficients)




  ####  2. Use Coefficients to Re-calculate Biomass  ####

  # calculate total biomass again using weights from key
  # make a key for the length weight coefficient sources
  survdat_weights <- dplyr::arrange(trawl_weights, est_year, season, comname, length_cm)
  survdat_weights <- dplyr::mutate(survdat_weights, lw_group = stringr::str_c(comname, season, catchsex))






  ####  3. Filter Bad Fits  ####
  # Drop comnames that don't align well with BIOMASS

  # Length weight biomasses were checked against survdat$biomass using data prior to the vessel change
  # 15% difference in either direction were flagged for removal
  # code: github.com/adamkemberling/nefsc_trawl/R/qa_qc_reports/stratification_validation
  # list updated : 8/27/2021
  cutoff_15 <- c(
    "acadian redfish", "american plaice",
    "american shad",
    "atlantic angel shark",
    "atlantic cod",
    "atlantic croaker",
    "atlantic halibut",
    "atlantic herring",
    "atlantic mackerel",
    "atlantic sharpnose shark",
    "atlantic spadefish",
    "atlantic sturgeon",
    "atlantic thread herring",
    "atlantic wolffish",
    "barndoor skate",
    "black sea bass",
    "blackbelly rosefish",
    "blueback herring",
    "bluefish",
    "buckler dory",
    "bullnose ray",
    "butterfish",
    "chain dogfish",
    "clearnose skate",
    "cownose ray",
    "cunner",
    "cusk",
    "fawn cusk-eel",
    "fourspot flounder",
    "goosefish",
    "greater amberjack",
    "haddock",
    "little skate",
    "longhorn sculpin",
    "northern kingfish",
    "northern searobin",
    "ocean pout",
    "offshore hake",
    "pollock",
    "red hake",
    "rosette skate",
    "roughtail stingray",
    "round herring",
    "sand tiger",
    "sandbar shark",
    "scup",
    "sea raven",
    "silver hake",
    "smooth butterfly ray",
    "smooth dogfish",
    "smooth skate",
    "southern kingfish",
    "spanish mackerel",
    "spanish sardine",
    "spiny butterfly ray",
    "spiny dogfish",
    "spot",
    "spotted hake",
    "striped bass",
    "summer flounder",
    "thorny skate",
    "weakfish",
    "white hake",
    "windowpane flounder",
    "winter flounder",
    "winter skate",
    "witch flounder",
    "yellowtail flounder")

  # these species were dropped, for the record:
  cutoff_15_dropped <-c(
    "alewife", "atlantic torpedo", "bluntnose stingray",
    "southern stingray", "tautog", "vermillion snapper")

  # Filter to use species that meet cutoff criteria
  # source: 02_survdat_stratification_validation
  if(cutoff == TRUE){
    survdat_weights <- dplyr::filter(survdat_weights, comname %in% cutoff_15)
  }




  return(survdat_weights)
}





######################################################_
# test
# test_lw        <- add_lw_info(survdat_clean = test_survdat)
# test_lw_cutoff <- add_lw_info(survdat_clean = test_survdat, cutoff = T)



######################################################__####

######################################################_


# testing data
# survdat_clean <- survdat_prep(survdat_source = "2020")


######################################################__####

######################################################_



#' @title Add EPU Details to Station Info
#'
#' @description Uses Sean Lucey's Poststrat function to add the correct EPU designation
#' to the survey station locations.
#'
#' @param trawldat Survdat data with decdeg_beglat and decdeg_beglon coordinates
#' @param box_location String indicating value to pass to `boxpath_switch`
#'
#' @description Uses Sean Lucey's "poststrat" function to overlay EPU's. Tried to
#' replace it with sf steps, but it wasn't worth the effort. This function exists to
#' reduce clutter in the area stratification function
#'
#' @return survdat dataframe containing epu overlap information
#' @export
#'
#' @examples
#' #' # not run
#' # add_epu_info(trawldat = survdat_lw)
add_epu_info <- function(trawldat, box_location = "root|cloudstorage"){


  # Switch for mac users with different box storage location
  path_fun <- boxpath_switch(box_location = box_location)

  #### EPU assignment for survdat stations - function from Sean Luceys "RSurvey" repo
  # This section assigns EPU's via overlay using Sean Lucey's code,
  # Area stratification code from Sean Lucey's Repo, renamed to signify what it is
  nmfs_path <- path_fun(box_group = "RES_Data", subfolder = "NMFS_trawl")
  source(paste0(nmfs_path, "slucey_functions/slucey_survdat_functions.R"))


  # EPU shapefiles can be loaded from ecodata package
  epu_sf <- ecodata::epu_sf
  epu_sp <- suppressWarnings(sf::as_Spatial(epu_sf))


  # Rename columns to match expected formats for Sean Lucey's poststrat()
  trawldat <- dplyr::rename(trawldat,
                  CRUISE6 = cruise6,
                  STATION = station,
                  STRATUM = stratum,
                  LAT     = decdeg_beglat,
                  LON     = decdeg_beglon)

  # Post stratify the station positions using EPU polygons
  trawldat <- poststrat(survdat = trawldat,
              stratum = epu_sp,
              strata.col = "EPU")

  # re-name everything back to where it was
  trawldat <- dplyr::rename(trawldat,
    epu           = newstrata,
    cruise6       = CRUISE6,
    station       = STATION,
    stratum       = STRATUM,
    decdeg_beglat = LAT,
    decdeg_beglon = LON)

  return(trawldat)




  # # Begin my take
  #
  # # explain what is taking so long:
  # message("Joining EPU Fields for Area Stratification.")
  #
  # # Put EPU pairing here:
  # # preferably using our code and not Sean's so debugging is easier
  #
  # # Load EPU - CRS known from ecodata::epu_sf
  # epu_path <- path_fun(box_group = "RES_Data", subfolder = "Shapefiles/EPU")
  # epu_sf <- read_sf(paste0(epu_path, "EPU_extended.shp")) %>% st_set_crs(4269)
  # epu_sf <- ecodata::epu_sf
  #
  # # Convert Survdat to sf
  # survdat_weights_sf <- st_as_sf(survdat_weights,
  #                                coords = c("decdeg_beglat", "decdeg_beglon"),
  #                                crs = 4326,
  #                                remove = FALSE)
  #
  #
  # # Convert both to lambert for overlay
  #
  # #Lambert Conformal Conic
  # lcc <- st_crs("+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-72 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 ")
  #
  # survdat_nad <- st_transform(survdat_weights_sf, crs = 4269)
  # survdat_lambert <- st_transform(survdat_nad, crs = lcc)
  # epu_lambert     <- st_transform(epu_sf, crs = lcc)
  #
  #
  # # Use overlay to assign EPU
  # in_epu <- st_intersects(survdat_lambert, epu_lambert)
}


# testing
# test_epu <- add_epu_info(trawldat = test_lw)



# Add area stratified biomass function
#' @title Add Survey Area Stratified Abundances and Biomasses
#'
#' @description Take the survdat data which has been cleaned with gmRi::gmri_survdat_prep, that is
#' paired with length weight relationships using gmRi::add_lw_info and estimate the area stratified
#' catch rates and their expected abundances and biomasses when those catch rates are applied to
#' the total areas of their respective strata.
#'
#' Area-stratified catch rates are calculated independently for each species, every year, within
#' each strata, and by each season. i.e. 1982 spring cpue of acadian redfish informs the
#' area-stratified catch of acadian redfish in spring of 1982.
#'
#' Constants for the area-towed and the catchability coefficient are as followed:
#' - Area covered by an albatross standard tow in km2 = 0.0384
#' - Catchability coefficient - ideally should change for species guilds: q = 1
#'
#' @param survdat_weights Input dataframe, produced by add_lw_info
#' @param include_epu Flag for calculating the EPU rates in addition to the stratum regions we use.
#' @param box_location String indicating value to pass to `boxpath_switch`
#'
#' @return survdat df with fields for area stratified values
#' @export
#'
#' @examples
#' # not run
#' # add_area_stratification(survdat_weights = survdat_lw, include_epu = F)
add_area_stratification <- function(survdat_weights, include_epu = F, box_location = "root|cloudstorage"){

  # https://noaa-edab.github.io/survdat/articles/calc_strat_mean.html

  # Switch for mac users with different box storage location
  path_fun <- boxpath_switch(box_location = box_location)


  ####  1. Import supplemental files  ####
  nmfs_path <- path_fun(box_group = "RES_Data", subfolder = "NMFS_trawl")

  # Stratum Area Information
  stratum_area_path <- stringr::str_c(nmfs_path, "Metadata/strata_areas_km2.csv")
  stratum_area      <- readr::read_csv(stratum_area_path, col_types = readr::cols())
  stratum_area      <- dplyr::mutate(stratum_area, stratum = as.character(stratum))



  ####  2. Set Constants:  ####

  # Area covered by an albatross standard tow in km2
  alb_tow_km2 <- 0.0384

  # catchability coefficient - ideally should change for species guilds
  q <- 1


  ####  Get Annual Stratum Effort, and Area Ratios
  # number of tows in each stratum by year
  # area of a stratum relative to total area of all stratum sampled that year


  ####  3. Stratum Area & Effort Ratios  ####

  # Merge in the area of strata in km2
  # (excludes ones we do not care about via left join)
  survdat_weights <- dplyr::left_join(survdat_weights, stratum_area, by = "stratum")
  survdat_weights <- dplyr::arrange(survdat_weights, survdat_weights, id)


  # Get Total area of all strata sampled in each year
  total_stratum_areas <- dplyr::group_by(survdat_weights, est_year)
  total_stratum_areas <- dplyr::distinct(total_stratum_areas, stratum, .keep_all = T)
  total_stratum_areas <- dplyr::summarise(total_stratum_areas,
    tot_s_area =  sum(s_area_km2, na.rm = T),
    .groups = "keep")
  total_stratum_areas <- dplyr::ungroup(total_stratum_areas)


  # Calculate strata area relative to total area i.e. stratio or stratum weights
  survdat_weights <- dplyr::left_join(survdat_weights, total_stratum_areas, by = "est_year")
  survdat_weights <- dplyr::mutate(survdat_weights, st_ratio = s_area_km2 / tot_s_area)


  # We have total areas, now we want effort within each
  # Number of unique tows per stratum, within each season
  yr_strat_effort <- dplyr::group_by(survdat_weights, est_year, season, stratum)
  yr_strat_effort <- dplyr::summarise(yr_strat_effort, strat_ntows = dplyr::n_distinct(id), .groups = "keep")
  yr_strat_effort <- dplyr::ungroup(yr_strat_effort)



  # Add those yearly effort counts back for later
  # (area stratified abundance)
  survdat_weights <- dplyr::left_join(survdat_weights, yr_strat_effort, by = c("est_year", "season", "stratum"))




  ####  4. Derived Stratum Area Estimates ####

  # a. Catch / tow, for that year & season
  survdat_weights <-  dplyr::mutate(survdat_weights,

      # Abundance
      abund_tow_s   = numlen_adj / strat_ntows,

      # All size biomass
      # Biomass is repeated across length classes at each station by species
      # the number of length classes is tallied where the conversion factor is done
      biom_per_lclass = (biomass_kg / n_len_class),
      biom_tow_s = biom_per_lclass / strat_ntows,

      # Length specific biomass
      lwbio_tow_s = sum_weight_kg / strat_ntows)


  # b. Stratified Mean Catch Rates
  survdat_weights <-  dplyr::mutate(survdat_weights,

      # Stratified mean abundance CPUE, weighted by the stratum areas
      strat_mean_abund_s = abund_tow_s * st_ratio,

      # Stratified mean BIOMASS CPUE
      strat_mean_biom_s = biom_tow_s * st_ratio,

      # Stratified mean LW Biomass
      strat_mean_lwbio_s = lwbio_tow_s * st_ratio)


  # c. Stratified Total Abundance/Biomass
  # convert from catch rate by area swept to total catch for entire stratum
  survdat_weights <-  dplyr::mutate(survdat_weights,

      # Total Abundance
      strat_total_abund_s = round((strat_mean_abund_s * tot_s_area / alb_tow_km2) / q),

      # Total BIOMASS from the biomass of all lengths
      strat_total_biom_s = (strat_mean_biom_s * tot_s_area / alb_tow_km2) / q,

      # Two options for to estimate lw biomass
      # Result is the same 4/20/2021
      # Option 1: Individual LW Biomass * expanded abundance at length
      strat_total_lwbio_s = ind_weight_kg * strat_total_abund_s,


      # # Option 2: Size specific lw biomass / tow, expanded to total area
      # strat_total_lwbio_s  = (strat_mean_lwbio_s * tot_s_area / alb_tow_km2) / q
  )



  ####  5. BONUS: EPU Stratifications  ####
  if(include_epu == TRUE){

    # Explain what is taking so long:
    message("Joining EPU Fields for Area Stratification.")

    # Add EPU details using sp::over() and Sean's code
    survdat_epu <- add_epu_info(survdat_weights)

    # EPU area information: source slucey_survdat_functions.R and {ecodata}
    epu_area_path <- stringr::str_c(nmfs_path, "Metadata/EPU_areas_km2.csv")
    epu_areas     <- readr::read_csv(epu_area_path, col_types = readr::cols())

    # Join to the file containing EPU areas in km2
    survdat_epu <-  dplyr::left_join(survdat_epu, epu_areas, by = "epu")


    # Get total area of EPU's sampled in each year
    total_epu_areas <- dplyr::group_by(survdat_epu, est_year)
    total_epu_areas <- dplyr::distinct(total_epu_areas, epu, .keep_all = T)
    total_epu_areas <- dplyr::summarise(total_epu_areas, tot_epu_area = sum(epu_area_km2, na.rm = T),
                       .groups = "keep")
    total_epu_areas <- dplyr::ungroup(total_epu_areas)



    # Calculate epu area relative to total area i.e. epu_ratio or area weights
    survdat_epu <- dplyr::left_join(survdat_epu, total_epu_areas, by = "est_year")
    survdat_epu <- dplyr::mutate(survdat_epu, epu_ratio  = epu_area_km2 / tot_epu_area)

    # Number of unique tows per EPU
    yr_epu_effort <- dplyr::group_by(survdat_epu, est_year, season, epu)
    yr_epu_effort <- dplyr::summarise(yr_epu_effort, epu_ntows = dplyr::n_distinct(id), .groups = "keep")
    yr_epu_effort <- dplyr::ungroup(yr_epu_effort)

    # join back
    survdat_epu <- dplyr::left_join(survdat_epu, yr_epu_effort, by = c("est_year", "season", "epu"))


    # Process Area Stratified values
    survdat_weights <- dplyr::mutate(survdat_epu,

        # Catch / tow
        abund_tow_epu = numlen_adj / epu_ntows,
        biom_tow_epu  = biom_per_lclass / epu_ntows,
        lwbio_tow_epu = sum_weight_kg / epu_ntows,
        # Stratified Mean Catch/tow
        strat_mean_abund_epu = abund_tow_epu * epu_ratio,
        strat_mean_biom_epu  = biom_tow_epu * epu_ratio,
        strat_mean_lwbio_epu = lwbio_tow_epu * epu_ratio,
        # Stratified Total Catch
        strat_total_abund_epu = round((strat_mean_abund_epu * tot_epu_area/ alb_tow_km2) / q),
        strat_total_biom_epu  = (strat_mean_biom_epu * tot_epu_area/ alb_tow_km2) / q,
        strat_total_lwbio_epu = ind_weight_kg * strat_total_abund_epu)}

  return(survdat_weights)
}






###################################################### _

# # Tests
# strat_biomass <- add_area_stratification(survdat_weights = test_lw, include_epu = F)
# strat_bio_epu <- add_area_stratification(survdat_weights = test_lw, include_epu = T)

###################################################### __####

###################################################### _

#' @title Get unique tows from cleaned SURVDAT trawl data
#'
#' @description extract the unique tows (lat, long, time) from the cleaned SURVDAT traw data, which can then be enhanced with other environmental covariates (e.g., OISST).
#'
#' @param survdat_clean Survdat data, after usual preparations are completed.
#' These include removal of old strata, labeling of areas of interest, and inclusion
#' of the annual effort in each.
#' @param box_location String indicating value to pass to `boxpath_switch`
#'
#' @return survdat trawl dataframe containing one row for every unique tow
#' @export
#'
#' @examples
#' # not run
#' # get_survdat_tows(survdat_clean = survdat_clean)
get_survdat_tows <- function(survdat_clean) {


  #### 1. Filter SURVDAT to unique tows and keep columns of interest   ####

  # Get unique tows
  survdat_tows <- survdat_clean %>%
    dplyr::distinct(id, est_towdate, est_year, est_month, est_day, season, svvessel, decdeg_beglat, decdeg_beglon, survey_area, avgdepth, surftemp, surfsalin, bottemp, botsalin) %>%
    dplyr::filter(!is.na(decdeg_beglat) & !is.na(decdeg_beglon))

  # Return it
  return(survdat_tows)
}

###################################################### _

# # Tests
# survdat_tows <- get_survdat_tows(survdat_clean = survdat_clean)

###################################################### __####

###################################################### _
#' @title make SURVDAT tidy occupancy dataset
#'
#' @description make a tidy occupancy dataset from the cleaned SURVDAT dataset, where each row represents a unique tow - species - occupancy record for every species in species_keep. In creating this occupancy dataset, we take the total biomass or abundance across sex/sizes and impute "absence" observations, such that there is a record for every species in `species_keep` at every unique tow.
#'
#' @param survdat_clean Survdat data, after usual preparations are completed.
#' These include removal of old strata, labeling of areas of interest, and inclusion
#' of the annual effort in each.
#' @param species_keep Vector of character species names or vector of numeric species codes to filter SURVDAT['comname'] (if character), or SURVDAT['svspp'] (if numeric)
#'
#' @return tidy occupancy dataframe containing one row for every unique tow-species
#' @export
#'
#' @examples
#' # not run
#' # make_survdat_occu(survdat_clean = survdat_clean, species_keep = c("atlantic cod", "offshore hake"))
make_survdat_occu <- function(survdat_clean, species_keep){


  #### 1. Filter SURVDAT to species of interest based on character or numeric vector `species_keep` and aggregate abundance and biomass data for these species at each tow location   ####
  if (all(is.character(species_keep))) {
    presence_data <- survdat_clean %>%
      dplyr::filter(., comname %in% species_keep)
  } else {
    presence_data <- survdat_clean %>%
      dplyr::filter(., comname %in% species_keep)
  }
  
  presence_data<- presence_data %>%
      dplyr::group_by(., id, svspp, comname) %>%
      dplyr::summarise(
        sum_abundance = sum(abundance),
        sum_biomass_kg = sum(unique(biomass_kg))
      ) %>%
      dplyr::mutate(presence = ifelse(sum_abundance > 0, 1, 0)) %>% # should all be 1s, presence = 1 if abundance >=1, presence = 0 if abundance = 0
        dplyr::select(id, svspp, comname, presence, sum_abundance, sum_biomass_kg) %>%
        dplyr::ungroup()
      
  
  #### 2. Create dataframe with all possible tow/species combinations   ####
  # Create a dataframe of all possible survey ID/species combinations
  all_spp <- survdat_clean %>% distinct(comname, svspp)
  all_id_spec_possibilites <- survdat_clean %>%
   tidyr::expand(id, comname) %>%
   left_join(all_spp, by = "comname") %>%
   dplyr::filter(., comname %in% presence_data$comname)

  
  #### 3. Join all possible tow/species dataframe with presence data and impute absences   ####
  survdat_occu<- all_id_spec_possibilites %>% 
    dplyr::left_join(presence_data, by = c("id", "svspp", "comname")) %>%                           
    #populate "possibilities" dataset with presence data                       
    dplyr::mutate(presence = ifelse(is.na(presence) == T, 0, presence)) %>%     
    dplyr::mutate(sum_biomass_kg = ifelse(is.na(sum_biomass_kg) == T, 0.000, sum_biomass_kg)) %>%  
    dplyr::mutate(sum_abundance = ifelse(is.na(sum_abundance) == T, 0, sum_abundance)) %>%  
    dplyr::select(id, svspp, comname, presence, sum_abundance, sum_biomass_kg) 
 
  # Return it
  return(survdat_occu)
  
}

######################################################_

# # Tests
# survdat_occu_chr <- make_survdat_occu(survdat_clean = survdat_clean, species_keep = c("atlantic cod", "american lobster"))
# survdat_occu_num<- make_survdat_occu(survdat_clean = survdat_clean, species_kep = c(105, 73))

######################################################__####

######################################################_