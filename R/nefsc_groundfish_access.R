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

######################################################__####

######################################################_

#' @title  Load survdat file with standard data filters, keep all columns
#'
#'
#' @description Processing function to prepare survdat data for size spectra analyses.
#' Options to select various survdat pulls, or provide your own available.
#'
#'
#' @param survdat optional starting dataframe in the R environment to run through size spectra build.
#' @param survdat_source String indicating which survdat file to load from box
#'
#' @return Returns a dataframe filtered and tidy-ed for size spectrum analysis.
#' @export
#'
#' @examples
#' # not run
#' # gmri_survdat_prep(survdat_source = "most recent")
gmri_survdat_prep <- function(survdat = NULL, survdat_source = "most recent"){

  ####  Resource Paths
  box_paths   <- research_access_paths(os.use = "unix")
  mills_path  <- box_paths$mills
  res_path    <- box_paths$res
  nmfs_path   <- paste0(res_path, "NMFS_trawl/")



  ####  1. Import SURVDAT File  ####

  # Testing:
  #survdat_source <- "bigelow"     ; survdat <- NULL
  #survdat_source <- "most recent" ; survdat <- NULL

  # convenience change to make it lowercase
  survdat_source <- tolower(survdat_source)


  # Build Paths to survdat for standard options
  survdat_path <- switch(EXPR = survdat_source,
                         "2016"        = paste0(mills_path, "Projects/WARMEM/Old survey data/Survdat_Nye2016.RData"),
                         "2019"        = paste0(nmfs_path,  "prior_survdat_data/Survdat_Nye_allseason.RData"),
                         "2020"        = paste0(nmfs_path,  "prior_survdat_data/Survdat_Nye_Aug 2020.RData"),
                         "2021"        = paste0(nmfs_path,  "prior_survdat_data/survdat_slucey_01152021.RData"),
                         "bigelow"     = paste0(nmfs_path,  "2021_survdat/survdat_Bigelow_slucey_01152021.RData"),
                         "most recent" = paste0(nmfs_path,  "2021_survdat/NEFSC_BTS_all_seasons_03032021.RData"),
                         "bio"         = paste0(nmfs_path,  "2021_survdat/NEFSC_BTS_2021_bio_03192021.RData") )



  # If providing a starting point for survdat pass it in:
  if(is.null(survdat) == FALSE){
    trawldat <- survdat %>% janitor::clean_names()
  } else if(is.null(survdat) == TRUE){

    # If not then load using the correct path
    load(survdat_path)


    # Bigelow data doesn't load in as "survdat"
    if(survdat_source == "bigelow"){
      survdat <- survdat.big
      rm(survdat.big)}

    # Most recent pulls load a list containing survdat
    if(survdat_source %in% c("bio", "most recent")){
      survdat <- survey$survdat }

    # clean names up for convenience
    trawldat <- survdat %>% janitor::clean_names()
  }

  # remove survdat once the data is in
  rm(survdat)



  #### 2.  Column Detection  ####

  ####__ a. Missing column flags  ####

  # Flags for missing columns that need to be merged in or built
  has_comname  <- "comname" %in% names(trawldat)
  has_id_col   <- "id" %in% names(trawldat)
  has_towdate  <- "est_towdate" %in% names(trawldat)
  has_month    <- "est_month" %in% names(trawldat)

  # Flags for renaming or subsetting the data due to presence/absence of columns
  has_year      <- "est_year" %in% names(trawldat)
  has_catchsex  <- "catchsex" %in% names(trawldat)
  has_decdeg    <- "decdeg_beglat" %in% names(trawldat)
  has_avg_depth <- "avgdepth" %in% names(trawldat)







  ####__ b. Missing comname  ####

  # Use SVSPP to get common names for species
  if(has_comname == FALSE){
    message("no comnames found, merging records in with spp_keys/sppclass.csv")
    # Load sppclass codes and common names
    spp_classes <- readr::read_csv(
        paste0(nmfs_path, "spp_keys/sppclass.csv"),
        col_types = cols()) %>%
      janitor::clean_names() %>%
      dplyr::mutate(comname  = stringr::str_to_lower(common_name),
             scientific_name = stringr::str_to_lower(scientific_name)) %>%
      dplyr::distinct(svspp, comname, scientific_name)


    # Add the common names over and format for rest of build
    trawldat <- trawldat %>%
      dplyr::mutate(svspp = stringr::str_pad(svspp, 3, "left", "0")) %>%
      dplyr::left_join(spp_classes, by = "svspp")

  }


  ####__ c. Missing ID  ####
  if(has_id_col == FALSE) {
    message("creating station id from cruise-station-stratum fields")
    # Build ID column
    trawldat <- trawldat %>%
      dplyr::mutate(cruise6 = stringr::str_pad(cruise6, 6, "left", "0"),
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
    trawldat <- dplyr::mutate(trawldat,
                              est_month = stringr::str_sub(est_towdate, 6,7),
                              est_month = as.numeric(est_month),
                              est_day   = stringr::str_sub(est_towdate, -2, -1),
                              est_day   = as.numeric(est_day))}



  #### 4. Column Changes  ####
  trawldat <- trawldat %>%
    dplyr::mutate(

      # Text Formatting
      comname = tolower(comname),
      id      = format(id, scientific = FALSE),
      svspp   = as.character(svspp),
      svspp   = stringr::str_pad(svspp, 3, "left", "0"),
      season  = tolower(season),

      # Format Stratum number,
      # exclude leading and trailing codes for inshore/offshore,
      # used for matching to stratum areas
      strat_num = stringr::str_sub(stratum, 2, 3)) %>%

    # Replace NA's where there is some biomass/abundance
    dplyr::mutate(biomass   = ifelse(biomass == 0 & abundance > 0, 0.0001, biomass),
                  abundance = ifelse(abundance == 0 & biomass > 0, 1, abundance))






  #### 5. Row Filtering  ####

  # Things filtered:
  # 1. Strata
  # 2. Seasons
  # 3. Year limits
  # 4. Vessels
  # 5. Species Exclusion

  # Eliminate Canadian Strata and Strata No longer in Use
  trawldat <- trawldat %>%
    dplyr::filter(stratum >= 01010,
                  stratum <= 01760,
                  stratum != 1310,
                  stratum != 1320,
                  stratum != 1330,
                  stratum != 1350,
                  stratum != 1410,
                  stratum != 1420,
                  stratum != 1490)

  # Filter to just Spring and Fall
  trawldat <- trawldat %>%
    dplyr::filter(season %in% c("spring", "fall"))

  # Filter years
  trawldat <- trawldat %>%
    dplyr::filter(est_year >= 1970,
                  est_year < 2020)

  # Drop NA Biomass and Abundance Records
  trawldat <- trawldat %>%
    dplyr::filter(!is.na(biomass),
                  !is.na(abundance))

  # Exclude the Skrimps
  trawldat <- trawldat %>%
    dplyr::filter(svspp %not in% c(285:299, 305, 306, 307, 316, 323, 910:915, 955:961))

  # Exclude the unidentified fish
  trawldat <- trawldat %>%
    dplyr::filter(svspp %not in% c(0, 978, 979, 980, 998))

  # Only the Albatross and Henry Bigelow
  #svvessel %in% c("AL", "HB"),






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
  trawldat <- trawldat %>%
    dplyr::mutate(
      survey_area =  case_when(
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
  trawldat <- trawldat %>%
    dplyr::filter(strat_num %in% strata_select) %>%
    dplyr::mutate(stratum = as.character(stratum))





  #### 8. Adjusting NumLength  ####

  # Sometimes there are more/less measured than initially tallied* in abundance
  # This section ensures that numlen totals out to be the same


  # If catchsex is not a column then total abundance is assumed pooled
  if(has_catchsex == TRUE){
    abundance_groups <- c("id", "comname", "catchsex", "abundance")
  } else {
    message("catchsex column not found, ignoring sex for numlen adjustments")
    abundance_groups <- c("id", "comname", "abundance")}


  # Get the abundance value for each sex
  # arrived at by summing across each length
  abundance_check <- trawldat %>%
    dplyr::group_by(!!!rlang::syms(abundance_groups)) %>%
    dplyr::summarise(
      abund_actual = sum(numlen),
      n_len_class  = dplyr::n_distinct(length),
      .groups      = "keep") %>%
    dplyr::ungroup()


  # Get the ratio between the original abundance column
  # and the sum of numlen we just grabbed
  conv_factor <- trawldat %>%
    dplyr::distinct(!!!rlang::syms(abundance_groups), length) %>%
    dplyr::inner_join(abundance_check, by = abundance_groups) %>%
    dplyr::mutate(convers = abundance / abund_actual)



  # Merge back and convert the numlen field
  # original numlen * conversion factor = numlength adjusted
  survdat_processed <- trawldat %>%
    dplyr::left_join(conv_factor, by = c(abundance_groups, "length")) %>%
    dplyr::mutate(numlen_adj = numlen * convers, .after = numlen) %>%
    dplyr::select(-c(abund_actual, convers))

  # remove conversion factor from environment
  rm(abundance_check, conv_factor, strata_key, strata_select)





  #### 9. Distinct Station & Species Length Info   ####

  # For each station we need unique combinations of
  # station_id, species, catchsex, length, adjusted_numlen
  # to capture what and how many of each length fish is caught

  # Record of unique station catches:
  # One row for every species * sex * length, combination in the data
  trawl_lens <- survdat_processed %>%
    dplyr::filter(is.na(length) == FALSE,
                  is.na(numlen) == FALSE,
                  numlen_adj > 0)


  # Do we want to just keep all the station info here as well?
  # question to answer is whether any other columns repeat,
  # or if these are the only ones
  trawl_spectra <- trawl_lens %>%
    dplyr::distinct(id, svspp, comname, catchsex, abundance, n_len_class,
                    length, numlen, numlen_adj, biomass, .keep_all = TRUE)




  # Return the dataframe
  # Contains 1 Row for each length class of every species caught
  return(trawl_spectra)

}




######################################################_

# Test
# test_survdat <- survdat_prep_nodrop(survdat_source = "most recent")



######################################################__####

######################################################_


# testing data
# survdat_clean <- survdat_prep(survdat_source = "2020")

#' @title Add species length weight information, calculate expected biomass
#'
#' @param survdat_clean Survdat data, after usual preparations are completed.
#' These include removal of old strata, labeling of areas of interest, and inclusion
#' of the annual effort in each.
#' @param cutoff Flag for whether to remove species that have shown poor fit
#' when compared to biomass data from the nmfs survey.
#'
#' @return
#' @export
#'
#' @examples
#' # not run
#' # add_lw_info(survdat_clean = survdat_clean, cutoff = T)
add_lw_info <- function(survdat_clean, cutoff = FALSE){


  #### 1. Match Species with Growth Coefficients  ####

  # This table is a combined table of wigley and fishbase L-W coefficients
  nmfs_path <- shared.path(group = "RES_Data", folder = "NMFS_trawl")
  lw_key_path <- paste0(nmfs_path, "length_weight_keys/fishbase_wigley_combined_key.csv")
  lw_combined <- readr::read_csv(lw_key_path, col_types = cols()) %>%
    dplyr::mutate(svspp = stringr::str_pad(svspp, 3, "left", "0"),
                  season = tolower(season))


  # Do a priority pass with the dplyr::filter(lw_combined, source == "wigley)
  # merge on comname, season, and catchsex
  wigley_coefficients <- dplyr::filter(lw_combined, source == "wigley") %>%
    dplyr::select(source, season, svspp, comname, scientific_name, spec_class,
                  hare_group, fishery, catchsex, a, b, ln_a)


  # Do a second pass with the dplyr::filter(lw_combined, source == "fishbase")
  # merge on common names only
  fishbase_coefficients <- dplyr::filter(lw_combined, source == "fishbase") %>%
    dplyr::select(source, -svspp, comname, scientific_name, spec_class,
                  hare_group, fishery, a, b, ln_a)


  # First Pass - Wigley
  # Join just by svspp to account for name changes
  pass_1 <- survdat_clean %>%
    dplyr::select(-comname) %>%
    dplyr::inner_join(wigley_coefficients)


  # Second Pass - Fishbase, for the stragglers if any
  pass_2 <- survdat_clean %>%
    dplyr::filter(comname %not in% wigley_coefficients$comname,
                  svspp %not in% pass_1$svspp) %>%
    dplyr::inner_join(fishbase_coefficients)


  # Join them with bind rows (implicitly drops things that don't have growth coefs)
  trawl_weights <- bind_rows(pass_1, pass_2) %>%
    dplyr::arrange(est_year, season) %>%
    dplyr::mutate(
      b             = as.numeric(b),
      a             = as.numeric(a),
      a             = ifelse(is.na(a) & !is.na(ln_a), exp(ln_a), a),
      ln_a          = ifelse(is.na(ln_a), log(a), ln_a),  # log of a used if ln_a is isn't already there (some fish just had ln_a reported)
      llen          = log(length),
      ind_log_wt    = ln_a + (b * llen),
      ind_weight_kg = exp(ind_log_wt),                    # weight of an individual in size class
      sum_weight_kg = ind_weight_kg * numlen_adj) %>%     # Individual weight * adjusted numlen
    tidyr::drop_na(ind_weight_kg) %>%
    dplyr::select(-ind_log_wt, -llen)


  # clean up environment
  rm(pass_1, pass_2, fishbase_coefficients, wigley_coefficients)




  ####  2. Use Coefficients to Re-calculate Biomass  ####

  # calculate total biomass again using weights from key
  # make a key for the length weight coefficient sources
  survdat_weights <- trawl_weights %>%
    dplyr::arrange(est_year, season, comname, length) %>%
    dplyr::mutate(lw_group = stringr::str_c(comname, season, catchsex))






  ####  3. Filter Bad Fits  ####
  # Drop comnames that don't align well with BIOMASS

  # these species were dropped at 50% mismatch threshold
  # code: 02_survdat_stratification_validation
  # list updated : 3/24/2021
  cutoff_25 <- c(
    "acadian redfish"          ,      "alewife"                 ,
    "american plaice"          ,      "american shad"           ,
    "atlantic angel shark"     ,      "atlantic cod"            ,
    "atlantic croaker"         ,      "atlantic halibut"        ,
    "atlantic herring"         ,      "atlantic mackerel"       ,
    "atlantic sharpnose shark" ,      "atlantic sturgeon"       ,
    "atlantic wolffish"        ,      "barndoor skate"          ,
    "black sea bass"           ,      "bluefish"                ,
    "bluntnose stingray"       ,      "buckler dory"            ,
    "bullnose ray"             ,      "butterfish"              ,
    "chain dogfish"            ,      "clearnose skate"         ,
    "cownose ray"              ,      "cunner"                  ,
    "cusk"                     ,      "fawn cusk-eel"           ,
    "fourspot flounder"        ,      "haddock"                 ,
    "little skate"             ,      "longhorn sculpin"        ,
    "northern kingfish"        ,      "northern searobin"       ,
    "ocean pout"               ,      "offshore hake"           ,
    "pollock"                  ,      "red hake"                ,
    "rosette skate"            ,      "roughtail stingray"      ,
    "round herring"            ,      "sandbar shark"           ,
    "sea raven"                ,      "silver hake"             ,
    "smooth butterfly ray"     ,      "smooth dogfish"          ,
    "smooth skate"             ,      "southern stingray"       ,
    "spanish mackerel"         ,      "spiny butterfly ray"     ,
    "spiny dogfish"            ,      "spot"                    ,
    "spotted hake"             ,      "striped bass"            ,
    "summer flounder"          ,      "tautog"                  ,
    "thorny skate"             ,      "weakfish"                ,
    "white hake"               ,      "windowpane flounder"     ,
    "winter flounder"          ,      "winter skate"            ,
    "witch flounder"           ,      "yellowtail flounder"
  )

  # Filter to use species that meet cutoff criteria
  # source: 02_survdat_stratification_validation
  if(cutoff == TRUE){
    survdat_weights <- survdat_weights %>% dplyr::filter(comname %in% cutoff_25)
  }




  return(survdat_weights)
}





######################################################_
# test
# test_lw        <- add_lw_info(survdat_clean = test_survdat)
# test_lw_cutoff <- add_lw_info(survdat_clean = test_survdat, cutoff = T)

######################################################__####

######################################################_



#' @title Add EPU Details to Station Info
#'
#' @description Uses Sean Lucey's Poststrat function to add the correct EPU designation
#' to the survey station locations.
#'
#' @param trawldat Survdat data with decdeg_beglat and decdeg_beglon coordinates
#'
#' @description Uses Sean Lucey's "poststrat" function to overlay EPU's. Tried to
#' replace it with sf steps, but it wasn't worth the effort. This function exists to
#' reduce clutter in the area stratification function
#'
#' @return
#' @export
#'
#' @examples
#' #' # not run
#' # add_epu_info(trawldat = survdat_lw)
add_epu_info <- function(trawldat){


  #### EPU assignment for survdat stations - function from Sean Luceys "RSurvey" repo
  # This section assigns EPU's via overlay using Sean Lucey's code,
  # Area stratification code from Sean Lucey's Repo, renamed to signify what it is
  nmfs_path <- shared.path(group = "RES_Data", folder = "NMFS_trawl")
  source(paste0(nmfs_path, "slucey_functions/slucey_survdat_functions.R"))


  # EPU shapefiles can be loaded from ecodata package
  epu_sf <- ecodata::epu_sf
  epu_sp <- suppressWarnings(as_Spatial(epu_sf))


  # Rename columns to match expected formats for Sean Lucey's poststrat()
  trawldat <- trawldat %>%
    dplyr::rename(CRUISE6 = cruise6,
                  STATION = station,
                  STRATUM = stratum,
                  LAT     = decdeg_beglat,
                  LON     = decdeg_beglon)

  # Post stratify the station positions using EPU polygons
  trawldat <- trawldat %>%
    poststrat(survdat = .,
              stratum = epu_sp,
              strata.col = "EPU")

  # re-name everything back to where it was
  trawldat <- trawldat %>%
    dplyr::rename(epu           = newstrata,
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
  # # preferrably using our code and not Sean's so debugging is easier
  #
  # # Load EPU - CRS known from ecodata::epu_sf
  # epu_path <- shared.path(group = "RES_Data", folder = "Shapefiles/EPU")
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







# Add area stratified biomass function
#' @title Add Survey Area Stratified Abundances and Biomasses
#'
#' @description Take the survdat data paired with length weight relationships and
#' return estimates of area stratified catch rates and their expected abundances and
#' biomasses when applied to the total areas of stratum.
#'
#' @param survdat_weights Input dataframe, produced by add_lw_info
#' @param include_epu Flag for calculating the EPU rates in addition to the stratum regions we use.
#'
#' @return
#' @export
#'
#' @examples
#' # not run
#' # add_area_stratification(survdat_weights = survdat_lw, include_epu = F)
add_area_stratification <- function(survdat_weights, include_epu = F){



  ####  1. Import supplemental files  ####
  nmfs_path <- shared.path(group = "RES_Data", folder = "NMFS_trawl")

  # Stratum Area Information
  stratum_area_path <- str_c(nmfs_path, "Metadata/strata_areas_km2.csv")
  stratum_area      <- readr::read_csv(stratum_area_path, col_types = cols())  %>%
    dplyr::mutate(stratum = as.character(stratum))



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
  survdat_weights <- survdat_weights %>%
    dplyr::left_join(stratum_area, by = "stratum") %>%
    dplyr::arrange(survdat_weights, id)


  # Get Total area of all strata sampled in each year
  total_stratum_areas <- survdat_weights %>%
    dplyr::group_by(est_year) %>%
    dplyr::distinct(stratum, .keep_all = T) %>%
    dplyr::summarise(tot_s_area =  sum(s_area_km2, na.rm = T),
                     .groups = "keep") %>%
    dplyr::ungroup()


  # Calculate strata area relative to total area i.e. stratio or stratum weights
  survdat_weights <- survdat_weights %>%
    dplyr::left_join(total_stratum_areas, by = "est_year") %>%
    dplyr::mutate(st_ratio = s_area_km2 / tot_s_area)


  # We have total areas, now we want effort within each
  # Number of unique tows per stratum
  yr_strat_effort <- survdat_weights %>%
    dplyr::group_by(est_year, stratum) %>%
    dplyr::summarise(strat_ntows = dplyr::n_distinct(id),
                     .groups = "keep") %>%
    dplyr::ungroup()



  # Add those yearly effort counts back for later
  #(area stratified abundance)
  survdat_weights <- survdat_weights %>%
    dplyr::left_join(yr_strat_effort, by = c("est_year", "stratum"))




  ####  4. Derived Stratum Area Estimates ####
  survdat_weights <- survdat_weights  %>%
    dplyr::mutate(
      # Abundance / ntows for the year within that strata/epu
      abund_tow_s   = numlen_adj / strat_ntows,

      # Biomass is repeated across length classes at each station by species
      # the number of length classes is tallied where the conversion factor is done
      biom_per_lclass = (biomass / n_len_class),

      # Mean biomass/tow for the "biomass" column
      biom_tow_s = biom_per_lclass / strat_ntows,

      # Stratified mean abundance, weighted by the stratum areas
      wt_abund_s = abund_tow_s * st_ratio,

      # Stratified mean BIOMASS
      wt_biom_s = biom_tow_s * st_ratio,

      # convert from catch rate by area swept to total catch for entire stratum
      # So catch/tow times the total area, divided by how many tows would cover that area
      expanded_abund_s = round((wt_abund_s * tot_s_area / alb_tow_km2) / q),

      # Total BIOMASS from the weighted biomass
      expanded_biom_s = (wt_biom_s * tot_s_area / alb_tow_km2) / q,

      # Two options for lw biomass
      # Result is the same 4/20/2021

      # Option 1: Individual LW Biomass * expanded abundance at length
      expanded_lwbio_s = ind_weight_kg * expanded_abund_s,

      # # Option 2: Size specific lw biomass / tow, expanded to total area
      # lwbio_tow_s       = sum_weight_kg / strat_ntows,
      # wt_lwbio_s        = lwbio_tow_s * st_ratio,
      # expanded_lwbio_s  = (wt_lwbio_s * tot_s_area / alb_tow_km2) / q
    )





  ####  5. BONUS: EPU Stratifications  ####
  if(include_epu == TRUE){

    # Explain what is taking so long:
    message("Joining EPU Fields for Area Stratification.")


    # Add EPU details using sp::over() and Sean's code
    survdat_epu <- add_epu_info(survdat_weights)


    # EPU area information: source slucey_survdat_functions.R and {ecodata}
    epu_area_path <- str_c(nmfs_path, "Metadata/EPU_areas_km2.csv")
    epu_areas     <- readr::read_csv(epu_area_path, col_types = cols())

    # Join to the file containing EPU areas in km2
    survdat_epu <- survdat_epu %>%
      dplyr::left_join(epu_areas, by = "epu")


    # Get total area of EPU's sampled in each year
    total_epu_areas <- survdat_epu %>%
      dplyr::group_by(est_year) %>%
      dplyr::distinct(epu, .keep_all = T) %>%
      dplyr::summarise(tot_epu_area =  sum(epu_area_km2, na.rm = T),
                       .groups = "keep") %>% dplyr::ungroup()



    # Calculate epu area relative to total area i.e. epu_ratio or area weights
    survdat_epu <- survdat_epu %>%
      dplyr::left_join(total_epu_areas, by = "est_year") %>%
      dplyr::mutate(epu_ratio  = epu_area_km2 / tot_epu_area)

    # Number of unique tows per EPU
    yr_epu_effort <-  survdat_epu %>%
      dplyr::group_by(est_year, epu) %>%
      dplyr::summarise(epu_ntows = dplyr::n_distinct(id), .groups = "keep") %>%
      dplyr::ungroup()

    # join back
    survdat_epu <- survdat_epu %>%
      dplyr::left_join(yr_epu_effort, by = c("est_year", "epu"))


    # Process Area Stratified values
    survdat_weights <- survdat_epu  %>%
      dplyr::mutate(
        # Abundance per tow
        abund_tow_epu = numlen_adj / epu_ntows,
        # Mean biomass/tow for the BIOMASS column
        biom_tow_epu = biom_per_lclass / epu_ntows,
        # Stratified mean abundances, weighted by the stratum areas
        wt_abund_epu = abund_tow_epu * epu_ratio,
        # Stratified mean BIOMASS
        wt_biom_epu = biom_tow_epu * epu_ratio,
        # Total catch for entire stratum
        expanded_abund_epu = round((wt_abund_epu * tot_epu_area/ alb_tow_km2) / q),
        # Total Biomass from the weighted biomass
        expanded_biom_epu = (wt_biom_epu * tot_epu_area/ alb_tow_km2) / q,
        # LW Biomass from Expanded abundances: Biomass = abundance * lw_weight
        expanded_lwbio_epu  = ind_weight_kg * expanded_abund_epu)}


  # Remove instances where there were fish that were measured but not weighed
  # survdat_weights <- survdat_weights %>% dplyr::filter(expanded_lwbio_s != -Inf)

  return(survdat_weights)
}






######################################################_

# # Tests
# strat_biomass <- add_area_stratification(survdat_weights = test_lw, include_epu = F)
# strat_bio_epu <- add_area_stratification(survdat_weights = test_lw, include_epu = T)

