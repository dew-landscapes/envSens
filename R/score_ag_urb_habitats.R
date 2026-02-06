#' Score breeding and feeding in agricultural and urban landscapes
#'
#' Assigns a score based on combinations of breeding and feeding in agricultural and urban habitats.
#'
#' Scoring rules:
#' 0 - Breed in both ag & urban AND feed in at least one, OR feed in both AND breed in at least one  
#' 1 - Breed in either ag or urban (but not both)  
#' 2 - Feed in either ag or urban, with no breeding in either  
#' 3 - Do not breed or feed in ag or urban
#'
#' @param df A data frame containing the four habitat columns.
#' @param breed_ag Column name for breeding in agricultural landscapes (default: "aub_Breeding_habitat_Agricultural_lands_9")
#' @param breed_urb Column name for breeding in urban landscapes (default: "aub_Breeding_habitat_Urban_9")
#' @param feed_ag Column name for feeding in agricultural landscapes (default: "aub_Feeding_habitat_Agricultural_landscapes_9")
#' @param feed_urb Column name for feeding in urban landscapes (default: "aub_Feeding_habitat_Urban_landscapes_9")
#'
#' @return Data frame with a new column `ag_urb_score` containing the scores 0–3.
#' @export
score_ag_urb_habitats <- function(
    df,
    breed_ag = "aub_BreedingHabitatAgriculturalLands9",
    breed_urb = "aub_BreedingHabitatUrban9",
    feed_ag = "aub_FeedingHabitatAgriculturalLandscapes9",
    feed_urb = "aub_FeedingHabitatUrbanLandscapes9"
) {
  
  # Check columns exist
  all_cols <- c(breed_ag, breed_urb, feed_ag, feed_urb)
  missing_cols <- setdiff(all_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing columns in df: ", paste(missing_cols, collapse = ", "))
  }
  
  df <- df %>% 
    dplyr::mutate(
      x_breedadapt = dplyr::coalesce(.data[[breed_ag]], 0) +
        dplyr::coalesce(.data[[breed_urb]], 0),
      
      x_feedadapt  = dplyr::coalesce(.data[[feed_ag]], 0) +
        dplyr::coalesce(.data[[feed_urb]], 0),
      
      x_scoreadapt = dplyr::case_when(
        (x_breedadapt + x_feedadapt) > 2 ~ 0,
        x_breedadapt > 0               ~ 1,
        x_feedadapt > 0                ~ 2,
        TRUE                          ~ 3
      ),
      
      aub_scaled_adapt = x_scoreadapt/3
    ) %>% 
    dplyr::select(!contains("x_"))
  
  return(df)
}
