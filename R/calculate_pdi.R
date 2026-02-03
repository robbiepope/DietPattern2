#' compute_twinsuk_pdi
#' 
#' The compute_twinsuk_pdi function is a wrapper of calculate_pdi to compute the 
#' Plant-based Dietary Index (PDI) and it's healthful (hPDI) or unhealthful (uPDI) 
#' derivations using TwinsUK specific parameters and food groupings.
#' PDI computation as described by Satija et al. (2017).
#' TwinsUK specific FFQ line item groupings as described by Asnicar et al. (2021).
#' 
#' @param food.intakes Pre-processed TwinsUK estimated intake .csv file (grams per day).
#'        the first column should be the unique identifier for each questionnaire.
#' @param perc proportion of distribution for each bin, float between 0 and 1 (default 0.2 for quintiles).
#' @return A df containing the calculated PDI, hPDI and uPDI scores and estimated intakes 
#'         of the amalgamated food groups using TwinsUK standard groupings.
#'         \describe{
#'         \item{PDI}{Overall PLant-based Dietary Index}
#'         \item{hPDI}{Healthful Plant-based Dietary Index}
#'         \item{uPDI}{Unhealthful Plant-based Dietary Index}
#'         \item{FooGroups}{Individual est. intakes for 18 food groups}
#'         }
#' @references Satija, A. et al. (2017) *Healthful and unhealthful plant-based diets and the risk of  coronary heart disease in US adults.* J Am Coll Cardiol 70(4):411–422.
#'             Asnicar et al. (2021) *Microbiome connections with host metabolism and habitual diet from 1,098 deeply phenotyped individuals.* Nature Medicine 27(2):321-332.
#' @author Robbie Pope
#' @export 
#' 
compute_twinsuk_pdi <- function(food.intakes, 
                                perc = 0.2) {
  
  # Create aggregate food groups for PDI computation
  df_food_groups <- get_food_bev_groups(food.intakes = food.intakes, 
                                        stnd_groups = "Y")
  
  # Calculate pdi using TwinsUK food groupings
  df_pdi <- calculate_pdi(df_food_groups,
                          perc = perc)

  return(df_pdi)
}

#' calculate_pdi
#' 
#' Compute the Plant-based Dietary Index (PDI) and it's healthful (hPDI) or 
#' unhealthful (uPDI) derivations. PDI computation as described by Satija et al. (2017).
#' The calculate_pdi function is a wrapper to perform all necessary steps to compute
#' each derivation of the score. 
#' 
#' @param df_food_groups A df of estimated intakes for the 18 amalgamated food groups for the PDI score (wide format). 
#' @param perc proportion of distribution for each bin, float between 0 and 1 (default 0.2).
#' @return A df containing the calculated PDI, hPDI and uPDI scores:
#'         \describe{
#'         \item{PDI}{Overall PLant-based Dietary Index}
#'         \item{hPDI}{Healthful Plant-based Dietary Index}
#'         \item{uPDI}{Unhealthful Plant-based Dietary Index}
#'         }
#' @references Satija, A. et al. (2017) *Healthful and unhealthful plant-based diets and the risk of coronary heart disease in US adults.* J Am Coll Cardiol 70(4):411–422.
#' @author Robbie Pope
#' @export 
#' 
calculate_pdi <- function(df_food_groups,
                          perc = 0.2) {
  # Assign food groups into healthy/less healthy/animal categories
  long_df <- assign_health_groups(df_food_groups)
  
  # Assign percentile of intake (rank within each food group)
  long_df <- assign_percentile(long_df, perc = perc)
  
  # Assign scores for PDI, hPDI, uPDI
  long_df <- assign_pdi_scores(long_df)
  
  #  Sum scores per individual to get total indices
  df_scores <- sum_pdi_scores(long_df)
  
  # Return final data frame with all three scores
  return(df_scores)
}

#' define_health_groups 
#' 
#' Define the healthy plant, less healthy plant and animal based food groups for 
#' the PDI, hPDI and uPDI calculations (Satija et al. (2017)).
#' 
#' @return named list of healthy plant, less healthy plant and animal based food 
#'         groupings.
#' @author Robbie Pope
#' @keywords internal
#' 
define_health_groups <- function() {
  health_groups <- list(
    healthy_plant = c('fruit', 'legumes', 'nuts_seeds',  'tea_coffee', 'vegetables', 
                      'vegetable_oil', 'whole_grain'),
    less_healthy_plant = c('fruit_juice', 'potatoes', 'refined_grains', 
                           'sweetened_beverage', 'sweet_dessert'),
    animal_foods = c('animal_fat', 'dairy', 'eggs', 'fish_seafood', 'meat', 'misc_animal')
  )
  return(health_groups)
  }

#' assign_health_groups
#' 
#' Based on the PDI groupings assign a healthy plant, less healthy plant or animal 
#' based food label for each of the 18 food groups (Satija et al. (2017)).
#' 
#' @param df Dataframe of PDI food groups created by get_food_bev_groups.
#' @return df in long format of PDI food groups and healthful label.
#' @author Robbie Pope
#' @keywords internal
#' 
assign_health_groups <- function(df) {
  # Get unique identifiers, first column of the df
  unq_id_values <- df[[1]]
  
  # Define health groups
  health_groups <- define_health_groups()
  
  # Create long df of estimated intakes for PDI groups
  long_df <- data.frame(
    FFQ_ID = rep(unq_id_values, times = ncol(df) - 1),
    FoodGroup = rep(names(df)[-1], each = nrow(df)),
    Estimated_Intake = as.vector(as.matrix(df[, -1]))
    )
  
  # Initialise long_df HealthfulGroup as "other"
  long_df$HealthfulGroup <- "other"
  
  # Iterate over named list of health groups and assign healthful group label
  for (group_name in names(health_groups)) {
    members <- health_groups[[group_name]]
    long_df$HealthfulGroup[long_df$FoodGroup %in% members] <- group_name
  }
  
  # Remove the other category (Alcoholic Beverages & Margaine) as not included in PDI calculation
  long_df <- long_df[long_df$HealthfulGroup != "other", ]
  
  # Return long_df for pdi calculation
  return(long_df)
}


#' assign_percentile 
#' 
#' Split study population into percentiles of intake for each food group,
#' assigning the percentile number in a new column (lowest percentile = lowest intake).
#' 
#' @param long_df long df output of assign_health_groups (must contain FoodGroup and Estimated_Intake).
#' @param perc proportion of distribution for each bin, float between 0 and 1 (default 0.2).
#' @return long df with an additional integer column "Percentile" (1 = lowest).
#' @author Robbie Pope
#' @keywords internal
#' @importFrom stats quantile
#' 
assign_percentile <- function(long_df, perc = 0.2) {
  
  # Required columns
  if (!all(c("FoodGroup", "Estimated_Intake") %in% names(long_df))) {
    stop("Data frame must contain 'FoodGroup' and 'Estimated_Intake'")
    }
  
  # Validate perc
  if (perc <= 0 || perc > 1) {
    stop("perc must be a float in (0, 1], e.g. 0.2 for quintiles")
    }
  
  # Number of bins 
  k <- as.integer(1 / perc)
  
  # Pre-allocate
  Percentile <- rep(NA_integer_, nrow(long_df))
  
  # Split by food group
  idx_by_group <- split(seq_len(nrow(long_df)), long_df$FoodGroup)
  
  for (grp in names(idx_by_group)) {
    
    idx <- idx_by_group[[grp]]
    x <- long_df$Estimated_Intake[idx]
    
    # Skip if all missing
    if (all(is.na(x))) {
      Percentile[idx] <- NA_integer_
      next
      }
    
    # Compute intake-based quantiles using the linear interpolation of the 
    # empirical cumulative distribution function (type=7)
    probs <- seq(0, 1, length.out = k + 1)
    cuts <- unique(stats::quantile(x, probs = probs, na.rm = TRUE, type = 7))
    
    # If all values identical (e.g. all zero) assign percentile 1
    if (length(cuts) == 1) {
      Percentile[idx] <- 1L
      next
      }
    
    # Assign quantile categories
    q <- cut(
      x,
      breaks = cuts,
      include.lowest = TRUE,
      labels = FALSE
      )
    
    Percentile[idx] <- as.integer(q)
    }
  
  long_df$Percentile <- Percentile
  return(long_df)
}
#' score_logic
#' 
#' Split study population into percentiles of intake for each food group,
#' assigning the percentile number in a new column (lowest percentile = lowest intake).
#' 
#' @param long_df long df with assigned ranked percentiles of intakes. Output from assign_percentile.
#' @param positive_groups vector of groups to be scored positively
#' @param negative_groups vector of groups to be scored negatively
#' @param score_name description
#' @return long_df with additional percentile scoring in an additional column for the type of pdi score
#' @author Robbie Pope
#' @keywords internal
#' 
score_logic <- function(long_df, 
                        positive_groups, 
                        negative_groups, 
                        score_name) {
  # Define the which health groups are scored either positively or negatively
  PosNegLogic <- ifelse(long_df$HealthfulGroup %in% positive_groups, "positive",
                        ifelse(long_df$HealthfulGroup %in% negative_groups, "negative", NA_character_))
  
  # Assign the score by percentile of intake. 
  # Positive = Percentile of intake, so highest consumers score 5
  # Negative = 6-percentile of intake, so highest consumers score 1
  long_df[[score_name]] <- ifelse(PosNegLogic == "positive", long_df$Percentile,
                                  ifelse(PosNegLogic == "negative", 6 - long_df$Percentile, NA_integer_))
  return(long_df)
}

#' assign_pdi_scores
#' 
#' Assign PDI, hPDI, and uPDI scores to each food group observation based on
#' percentile and group classification.
#' 
#' @param long_df long df with assigned ranked percentiles of intakes. Output from assign_percentile.
#' @return Long df with added columns: PDI_score, hPDI_score, uPDI_score with rank based percentile 
#'         score for each food group for PDI, hPDI and uPDI.
#' @author Robbie Pope
#' @keywords internal
#' 
assign_pdi_scores <- function(long_df) {
  # Score for PDI
  long_df <- score_logic(long_df,
                         positive_groups = c("healthy_plant", "less_healthy_plant"),
                         negative_groups = c("animal_foods"),
                         score_name = "PDI_score")
  # Score for hPDI
  long_df <- score_logic(long_df,
                         positive_groups = c("healthy_plant"),
                         negative_groups = c("less_healthy_plant", "animal_foods"),
                         score_name = "hPDI_score")
  # Score for uPDI
  long_df <- score_logic(long_df,
                         positive_groups = c("less_healthy_plant"),
                         negative_groups = c("healthy_plant", "animal_foods"),
                         score_name = "uPDI_score")
  return(long_df)
}

#' sum_pdi_scores 
#' 
#' Sum the ranked percentile scores for each derivation of the PDI.
#' 
#' @param long_df long df with assigned ranked percentile scores. Output from assign_pdi_scores.
#' @return df with summed PDI, hPDI and uPDI scores for each indivdiual.
#' @author Robbie Pope
#' @keywords internal
#' 
#' @importFrom dplyr group_by summarise left_join
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' 
sum_pdi_scores <- function(long_df) {
  # Compute PDI, hPDI, and uPDI scores for each FFQ
  score_df <- long_df |>
    dplyr::group_by(FFQ_ID) |>
    dplyr::summarise(
      PDI  = sum(PDI_score,  na.rm = TRUE),
      hPDI = sum(hPDI_score, na.rm = TRUE),
      uPDI = sum(uPDI_score, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Summarise estimated intake for each PDI food group per FFQ
  intake_df <- long_df |>
    dplyr::group_by(FFQ_ID, FoodGroup) |>
    dplyr::summarise(
      Estimated_Intake = sum(Estimated_Intake, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = FoodGroup,
      values_from = Estimated_Intake,
      values_fill = 0
    )
  
  # Join summed PDI scores and food group intakes
  final_df <- dplyr::left_join(score_df, intake_df, by = "FFQ_ID")
  final_df <- final_df %>% dplyr::rename(FFQ_ID = FFQ_ID)
  return(final_df)
}
