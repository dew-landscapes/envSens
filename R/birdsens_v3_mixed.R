#' Score bird sensitivity based on ecological traits
#'
#' Calculates composite sensitivity indices for bird species based on
#' specialisation, life-history constraints, and adaptability traits.
#' The function cleans input data, computes trait-level scores, aggregates
#' them into thematic indices, and optionally writes the full scored table
#' to disk.
#'
#' Sensitivity dimensions include:
#' \itemize{
#'   \item Climate specialisation (range size, climate breadth, elevation)
#'   \item Habitat breadth
#'   \item Diet breadth
#'   \item Life-history constraints (migration, generation length, range restriction)
#'   \item Adaptability to modified environments
#' }
#'
#' All indices are scaled between 0 and 1, where higher values indicate
#' greater sensitivity.
#'
#' @param mapped A data frame containing mapped bird traits.
#'   Must include all variables referenced in the scoring rules.
#' @param outpath Character string giving the directory where the scored
#'   CSV file will be written.
#' @param return Character string specifying what to return:
#'   \code{"raw"} returns the original data with indices only;
#'   \code{"scored"} returns only ID columns and index variables.
#'
#' @return A tibble with bird sensitivity indices, depending on \code{return}.
#'
#' @author Eryn
#' @export
#'
#' @examples
#' \dontrun{
#' scored <- score_bird_sensitivity(
#'   mapped = bird_traits,
#'   outpath = "outputs/",
#'   return = "scored"
#' )
#' }
birdsens_v3_mixed <- function(
    mapped,
    outpath,
    return = c("raw", "scored")
) {
  
  return <- match.arg(return)
  
  scored <- mapped %>%
    
    # ---- Clean values and enforce types ----------------------------------
  dplyr::mutate(
    dplyr::across(where(is.character), ~ na_if(.x, "NAV"))
  ) %>%
    readr::type_convert() %>%
    
    # ---- Trait scoring ----------------------------------------------------
  dplyr::mutate(
    
    #### SPECIALISATION ----
    
    # Climate
    score_clim_rangesize  = 1 - bl_eoo_log10Scaled,
    score_clim_breadth    = 1 - rec_stern_dehoedt_2000_minor_simpson,

    index_climate = (score_clim_rangesize + score_clim_breadth)/2,
    
    # Habitat
    score_habitat_bl = 1 - (0.6*bl_scaledHB_L1 + 0.4*bl_logscaledHBscore_L2),
    score_habitat_bb = 1/bb_Hb,
    
    index_habitat = (score_habitat_bl + score_habitat_bb)/2,
    
    # Diet
    index_diet = 1 - bb_db_simpson,
    
    #### LIFE-HISTORY CONSTRAINTS ----
    
    # Migration
    index_cons_mig = scales::rescale(bb_mig_score, to = c(0, 1)),
    
    # Generation length
    index_cons_genlength = bl_genlen_logScaled,
    
    # Restricted range
    index_cons_restrictedrange = bb_Rr,

    #### ADAPTABILITY ----
    
    # Modified environment use
    
    index_adapt = aub_scaled_adapt,
  ) %>%
    
    # ---- Final sensitivity index -----------------------------------------
  dplyr::mutate(
    n_indices = base::ncol(dplyr::select(., dplyr::starts_with("index_"))),
    
    sensitivity_index = rowSums(dplyr::across(dplyr::contains("index_")), 
                                na.rm = TRUE)/ n_indices
  ) %>% 
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), round, 2))
  
  # ---- Write output -------------------------------------------------------
  readr::write_csv(
    scored,
    file.path(outpath, "bird_sensitivity_v3.csv")
  )
  
  # ---- Return object ------------------------------------------------------
  if (return == "raw") {
    scored %>%
      dplyr::select(
        search_term,
        common,
        !dplyr::starts_with("score_"),
        !dplyr::starts_with("index_")
      )
  } else {
    scored %>%
      dplyr::select(
        search_term,
        common,
        dplyr::starts_with("score_"),
        dplyr::contains("index_")
      )
  }
}
