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
score_bird_sensitivity <- function(
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
    score_clim_elevation  = 1 - bl_log10ElevScaled,
    
    index_climate = rowMeans(
      dplyr::pick(dplyr::starts_with("score_clim_")),
      na.rm = TRUE
    ),
    
    # Habitat
    index_habitat = 1 - (0.6*bl_scaledHB_L1 + 0.4*bl_logscaledHBscore_L2),
    
    # Diet
    index_diet = 1 - bb_db_simpson,
    
    #### LIFE-HISTORY CONSTRAINTS ----
    
    # Migration
    score_cons_mig = dplyr::case_when(
      bl_MigratoryStatus == "Full migrant" ~ 3,
      bl_MigratoryStatus == "Altitudinal migrant" ~ 2,
      TRUE ~ 1
    ) / 3,
    
    # Generation length
    score_cons_genlength = bl_genlen_logScaled,
    
    # Restricted range
    score_cons_restrictedrange = bb_Rr,
    
    # Raptor 
    score_cons_raptor = if_else(
      bl_Family %in% c(
        "Barn-owls",
        "Typical Owls",
        "Hawks, Eagles",
        "Kites",
        "Falcons, Caracaras"
      ),
      1L, 0L),
    
    
    index_constraint = (score_cons_mig + 
                          0.5*score_cons_genlength +
                          score_cons_restrictedrange + 
                          0.5*score_cons_raptor)/3,
    
    #### ADAPTABILITY ----
    
    # Modified environment use
    
    index_adapt = 1 - bl_anthro_LogHabitat_scaled
  ) %>%
    
    # ---- Final sensitivity index -----------------------------------------
  dplyr::mutate(
    n_indices = base::ncol(dplyr::select(., dplyr::starts_with("index_"))),
    
    sensitivity_index =
      rowSums(dplyr::select(., dplyr::starts_with("index_")), na.rm = TRUE),
    
    sensitivity_index_averaged =
      sensitivity_index / n_indices
  ) %>% 
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), round, 2))
  
  # ---- Write output -------------------------------------------------------
  readr::write_csv(
    scored,
    file.path(outpath, "scored_bird_sensitivity.csv")
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
