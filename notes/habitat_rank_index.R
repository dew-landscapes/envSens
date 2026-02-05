#' Calculate habitat specialisation index for species
#'
#' This function quantifies habitat specialisation using a hierarchical
#' habitat classification:
#'   - Level 1: broad habitat types
#'   - Level 2: subtypes nested within Level 1
#'
#' Specialisation is calculated at two scales:
#'   1) Between Level 1 habitats
#'   2) Within Level 1 habitats (subtypes)
#'
#' The final index is a weighted combination of Level 1 and Level 2
#' specialisation scores.
#'
#' @param df A data frame containing species–habitat associations.
#' @param species_cols Character vector of species identifier columns
#'   (default: c("ScientificName", "CommonName")).
#' @param habitat_l1 Column name for habitat level 1.
#' @param habitat_l2 Column name for habitat level 2.
#' @param subtype_availability A data frame with columns:
#'   - HabitatsLevel1
#'   - n (number of available subtypes per Level 1 habitat)
#' @param w_l1 Weight for Level 1 specialisation (default = 0.6).
#' @param w_l2 Weight for Level 2 specialisation (default = 0.4).
#'
#' @return A tibble with one row per species containing:
#'   - L1_breadth
#'   - L1_specialisation
#'   - L2_breadth
#'   - L2_specialisation
#'   - habitat_specialisation_index
#'
#' @examples
#' habitat_specialisation_index(
#'   df = aa,
#'   subtype_availability = subtype_counts,
#'   w_l1 = 0.6,
#'   w_l2 = 0.4
#' )
#'
#' @export
habitat_rank_index <- function(
    df,
    species_cols = c("ScientificName", "CommonName"),
    habitat_l1 = "HabitatsLevel1",
    habitat_l2 = "HabitatsLevel2",
    subtype_availability,
    w_l1 = 0.6,
    w_l2 = 0.4
) {
  
  # ---- checks ----
  if (!all(species_cols %in% names(df))) {
    stop("Some species identifier columns are missing from df.")
  }
  if (!all(c(habitat_l1, habitat_l2) %in% names(df))) {
    stop("Habitat level columns are missing from df.")
  }
  if (!all(c("HabitatsLevel1", "n") %in% names(subtype_availability))) {
    stop("subtype_availability must contain 'HabitatsLevel1' and 'n'.")
  }
  if (!isTRUE(all.equal(w_l1 + w_l2, 1))) {
    stop("w_l1 and w_l2 must sum to 1.")
  }
  
  total_l1_available <- dplyr::n_distinct(subtype_availability$HabitatsLevel1)
  
  # ---- L1 breadth & specialisation ----
  l1_summary <- df %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(c(species_cols, habitat_l1)))) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(species_cols))) %>%
    dplyr::summarise(
      n_l1_used = dplyr::n(),
      L1_breadth = n_l1_used / total_l1_available,
      L1_specialisation = 1 - L1_breadth,
      .groups = "drop"
    )
  
  # ---- L2 breadth & specialisation ----
  l2_summary <- df %>%
    dplyr::distinct(
      dplyr::across(dplyr::all_of(c(species_cols, habitat_l1, habitat_l2)))
    ) %>%
    dplyr::count(
      dplyr::across(dplyr::all_of(c(species_cols, habitat_l1))),
      name = "n_subtypes_used"
    ) %>%
    dplyr::left_join(
      subtype_availability,
      by = c(habitat_l1 = "HabitatsLevel1")
    ) %>%
    dplyr::mutate(
      subtype_breadth = n_subtypes_used / n
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(species_cols))) %>%
    dplyr::summarise(
      L2_breadth = mean(subtype_breadth, na.rm = TRUE), # specify SUM or MEAN for level 2
      L2_specialisation = 1 - L2_breadth,
      .groups = "drop"
    )
  
  # ---- combine & weighted index ----
  l1_summary %>%
    dplyr::left_join(l2_summary, by = species_cols) %>%
    dplyr::mutate(
      habitat_specialisation_index =
        w_l1 * L1_specialisation + w_l2 * L2_specialisation
    )
}
