#' compute_twinsuk_dash
#' 
#' The compute_twinsuk_dash function is a wrapper of calculate_dash to compute the 
#' DASH diet score using TwinsUK specific parameters and food groupings.
#' DASH computation as described by Fung et al. (2008).
#' TwinsUK specific FFQ line item & nutrient components as described by Pope et al. (2021).
#' 
#' @param est.intakes Pre-processed TwinsUK estimated intake .csv file (grams per day).
#'        the first column should be the unique identifier for each questionnaire.
#' @param nutr.intakes Pre-processed TwinsUK estimated nutrient intakes .csv file,
#'        the first column should be the unique identifier for each questionnaire.
#' @param merge_col_name_est.intakes name of column to merge estimated food and 
#'        nutrient intakes for food grams per day .csv file. Default is 'FFQ_ID'.
#' @param merge_col_name_nutr.intakes name of column to merge estimated food and 
#'        nutrient intakes for nutrient .csv file. Default is 'FFQ_ID'.
#'        the first column should be the unique identifier for each questionnaire.
#' @param perc proportion of distribution for each bin, float between 0 and 1 (default 0.2 for quintiles).
#' @return A df containing the calculated DASH scores and estimated intakes of individual DASH components 
#'         using TwinsUK Standard groupings. 
#'         \describe{
#'         \item{DASH}{Overall PLant-based Dietary Index}
#'         \item{DASH_Components}{Individual est. intakes for 8 DASH components}
#'         }
#' @references Fung, T. et al. (2008)
#'             *Adherence to a DASH-Style Diet and Risk of Coronary Heart Disease 
#'             and Stroke in Women*
#'             Archives of Internal Medicine 168(7):713-720.
#'             
#'             Pope et al. (2025)
#'             *Faecal metabolites as a readout of habitual diet capture dietary 
#'             interactions with the gut microbiome*
#'             Nature Communications 16(1):10051.
#' @author Robbie Pope
#' @export 
#' 
compute_twinsuk_dash <- function(est.intakes, 
                                 nutr.intakes,
                                 merge_col_name_est.intakes = 'FFQ_ID',
                                 merge_col_name_nutr.intakes = 'FFQ_ID',
                                 perc = 0.2) {
  
  # Create aggregate food groups for PDI computation
  df_dash_components <- get_dash_components(est.intakes = est.intakes, 
                                            nutr.intakes = nutr.intakes, 
                                            merge_col_name_est.intakes = merge_col_name_est.intakes,
                                            merge_col_name_nutr.intakes = merge_col_name_nutr.intakes)
  
  # Calculate pdi using TwinsUK food groupings
  df_dash <- calculate_dash(df_dash_components,
                            perc = perc)
  
  return(df_dash)
}

#' calculate_dash
#' 
#' Compute the DASH diet score using TwinsUK specific parameters and components.
#' DASH computation as described by Fung et al. (2008).
#' The calculate_dash function is a wrapper to perform all necessary steps to compute
#' each derivation of the score. 
#' 
#' @param df_dash_components A df of estimated intakes for the 8 amalgamated food & nutrient components for the DASH score (wide format). 
#' @param perc proportion of distribution for each bin, float between 0 and 1 (default 0.2 for quintiles).
#' @return A df containing the calculated DASH scores and estimated intakes of individual DASH components 
#'         using TwinsUK Standard groupings. 
#'         \describe{
#'         \item{DASH}{Overall PLant-based Dietary Index}
#'         \item{DASH_Components}{Individual est. intakes for 8 DASH components}
#'         }
#' @references Fung, T. et al. (2008)
#'             *Adherence to a DASH-Style Diet and Risk of Coronary Heart Disease 
#'             and Stroke in Women*
#'             Archives of Internal Medicine 168(7):713-720.
#'             
#' @author Robbie Pope
#' @export 
#' 
calculate_dash <- function(df_dash_components,
                           perc = 0.2) {
  # Assign food groups into positive / negative categories
  long_df <- assign_dash_health_groups(df_dash_components)
  
  # Assign percentile of intake (rank within each food group)
  long_df <- assign_percentile(long_df, perc = perc)
  
  # Assign scores for DASH
  long_df <- assign_dash_scores(long_df)
  
  #  Sum scores per individual to get total indices
  df_scores <- sum_dash_scores(long_df)
  
  # Return final data frame with all three scores
  return(df_scores)
}

#' get_dash_components
#'
#' Take pre-prepared TwinsUK grams per day and nutrient estimate .csv files and compute 
#' the estimated daily intakes for amalgamated food, beverage and nutrient components of 
#' the DASH diet score. Standard groupings (Pope et al. (2025)), or custom groupings for
#' each component can be used. 
#'
#' @param est.intakes Pre-processed TwinsUK estimated intake .csv file (grams per day).
#'        the first column should be the unique identifier for each questionnaire.
#' @param nutr.intakes Pre-processed TwinsUK estimated nutrient intakes .csv file,
#'        the first column should be the unique identifier for each questionnaire.
#' @param merge_col_name_est.intakes name of column to merge estimated food and 
#'        nutrient intakes for food grams per day .csv file. Default is 'FFQ_ID'.
#' @param merge_col_name_nutr.intakes name of column to merge estimated food and 
#'        nutrient intakes for nutrient .csv file. Default is 'FFQ_ID'.
#'        the first column should be the unique identifier for each questionnaire.
#' @param stnd_components Character either "Y" (yes) or "N" (no) to determine if 
#'        standard Pope et al. (2025) groupings for DASH components are used.
#' @param alt_components Named list of custom food and nutrient component definitions, 
#'        used only when stnd_components = "N". Each list element name should 
#'        be a one of the DASH components, and each value a character vector of column names.
#'
#' @return Data frame with summed daily intakes for each food, beverage or nutrient 
#'         component of the DASH diet score.
#' @author Robbie Pope
#' @export
#'
get_dash_components <- function(est.intakes, 
                                nutr.intakes, 
                                merge_col_name_est.intakes = 'FFQ_ID',
                                merge_col_name_nutr.intakes = 'FFQ_ID',
                                stnd_components = "Y",
                                alt_components = NULL
                                ) {
  
  # Restrict inputs for stnd_groups
  stnd_components <- match.arg(stnd_components, choices = c("Y", "N"))
  
  # Extract unique identifier column (must be first column of grams per day .csv)
  id_col <- est.intakes[[1]]
  
  # Use standard or alternative food and beverage groupings
  if (stnd_components == "Y") {
    food_groups <- dash_standard_groupings()
  } else {
    if (is.null(alt_groups)) {
      stop("alt_groups: a named list must be provided when stnd_groups = 'N'.", call. = FALSE)
    }
    food_groups <- alt_groups
  }
  
  # Summarise by food group (skipping missing columns)
  summed_cols <- sum_dash_components(est.intakes = est.intakes,
                                     nutr.intakes = nutr.intakes,
                                     named_list = food_groups,
                                     merge_col_name_est.intakes = 'FFQ_ID',
                                     merge_col_name_nutr.intakes = 'FFQ_ID')
  
  # Combine into final data frame — first col is ID
  summed_df <- data.frame(
    UniqueKey = id_col,
    as.data.frame(summed_cols, check.names = FALSE),
    stringsAsFactors = FALSE
  )
  
  # Assign the correct group names
  colnames(summed_df)[-1] <- names(food_groups)
  
  # Check lengths must match
  stopifnot(length(summed_df$UniqueKey) == nrow(summed_df))
  
  return(summed_df)
  }

#' dash_standard_groupings 
#' 
#' Define the nine DASH amalgamated food, beverage and nutrient components 
#' Methodology as described by Fung et al. (2008)
#' FFQ line item and nutrient groupings according to Pope et al. (2025).
#' 
#' @return named list of DASH food & nutrient components for TwinsUK EPIC FFQ
#' @author Robbie Pope
#' @keywords internal
#' 
dash_standard_groupings <- function() {
  food_groups <- list(
    vegetables = c('AVOCADO', 'BEANSPROUTS', 'BEETROOT', 'BROCCOLI_KALE', 'CABBAGE',
                   'CARROTS', 'CAULIFLOWER', 'COLESLAW', 'GARLIC', 'GREEN_SALAD_CUCUMBER_CELERY',
                   'LEEKS', 'COURGETTE_MARROW', 'MUSHROOMS', 'ONIONS', 'PARSNIPS_TURNIPS',
                   'PICKLES_CHUTNEY', 'PEPPERS', 'SPINACH', 'SPROUTS', 'SWEETCORN', 
                   'TOMATOES', 'SOUP_VEGETABLE', 'WATERCRESS'),
    fruit = c('APPLES', 'BANANAS', 'BERRIES', 'DRIED_FRUIT', 'FRUIT_JUICE', 'GRAPEFRUIT', 
              'GRAPES', 'MELONS', 'ORANGES_MANDARINS', 'PEACHES_PLUMS_APRICOTS', 
              'PEARS', 'SMOOTHIES', 'TINNED_FRUIT'),
    nuts_legumes = c('BEANS_BAKED', 'GREEN_BEANS_BROAD_BEANS', 'NUTS_SALTED', 
                     'NUTS_UNSALTED', 'PEANUT_BUTTER', 'PEAS', 'PULSES_LENTILS_BEANS_PEAS', 
                     'SEEDS', 'TOFU_MEAT_SUBSTITUTE'),
    whole_grain = c('BROWN_BREAD', 'BROWN_RICE', 'CEREAL_HIGH_FIBRE', 'PORRIDGE_OATS', 
                    'WHOLEMEAL_BREAD', 'WHOLEMEAL_PASTA'),
    lf_dairy = c('COTTAGE_CHEESE', 'LOWFAT_CHEESE', 'LOWFAT_YOGURT'),
    sugar_drink = c('COCOA_HOT_CHOCOLATE', 'COFFEE_WHITENER', 'FIZZY_DRINKS', 'FRUIT_SQUASH',
                    'HORLICKS', 'LOWFAT_COCOA_HOT_CHOCOLATE', 'LOWCAL_FIZZY_DRINKS'),
    meat = c('BACON_GAMMON', 'BEEF', 'BEEFBURGER', 'CORNED_BEEF', 'HAM_CURED_MEATS', 
             'LAMB', 'LASAGNE_MOUSSAKA', 'LIVER_OFFAL', 'PORK', 'SAUSAGES', 'SOUP_MEAT'),
    sodium = c('Sodium_mg')
  )
  return(food_groups)
}

#' sum_dash_components
#' 
#' Define standard amalgamated food and nutrient components of DASH diet score 
#' for TwinsUK EPIC FFQ
#' FFQ line items (Pope et al. (2025)).
#' 
#' @param est.intakes Pre-processed TwinsUK estimated intake .csv file (grams per day).
#'        the first column should be the unique identifier for each questionnaire.
#' @param nutr.intakes Pre-processed TwinsUK estimated nutrient intakes .csv file,
#'        the first column should be the unique identifier for each questionnaire.
#' @param named_list Named list of food and nutrient group components, 
#'        Each list element name should be a food/nutrient component of the DASH diet, 
#'        and each value a character vector of column names.
#' @param merge_col_name_est.intakes name of column to merge estimated food and 
#'        nutrient intakes for food grams per day .csv file. Default is 'FFQ_ID'.
#' @param merge_col_name_nutr.intakes name of column to merge estimated food and 
#'        nutrient intakes for nutrient .csv file. Default is 'FFQ_ID'.
#' @return A list of numeric vectors (each of length equal to `nrow(data)`), where
#'         each vector represents the summed intake for a food or nutrient component.
#' @author Robbie Pope
#' @keywords internal
#'         
sum_dash_components <- function(est.intakes,
                                nutr.intakes, 
                                named_list,
                                merge_col_name_est.intakes = 'FFQ_ID',
                                merge_col_name_nutr.intakes = 'FFQ_ID'
                                ) {
  
  df_merge <- merge(est.intakes, nutr.intakes, 
                    by.x = merge_col_name_est.intakes,
                    by.y = merge_col_name_nutr.intakes)
  
  summed_groups <- lapply(names(named_list), function(group_name) {
    cols <- named_list[[group_name]]
    cols_present <- cols[cols %in% colnames(df_merge)]
    if (length(cols_present) == 0) {
      return(rep(0, nrow(df_merge)))
    }
    rowSums(df_merge[, cols_present, drop = FALSE], na.rm = TRUE)
  })
  
  return(summed_groups)
}

#' dash_health_groups 
#' 
#' Define the healthy or less-healthy groups for the DASH diet score. 
#' Methodology as described by Fung et al. (2008)
#' 
#' @return named list of healthy and less healthy DASH components for scoring
#' @author Robbie Pope
#' @keywords internal
#' 
dash_health_groups <- function() {
  health_groups <- list(
    positive = c('nuts_legumes', 'whole_grain', 'fruit', 'vegetables', 'lf_dairy'),
    negative = c('meat', 'sodium', 'sugar_drink')
  )
  return(health_groups)
}

#' assign_dash_health_groups
#' 
#' Based on the DASH components as described by Fung et al. (2008).
#' 
#' @param df Dataframe of DASH score components. Output of get_dash_components
#' @return df in long format of DASH components and healthful label.
#' @author Robbie Pope
#' @keywords internal
#' 
assign_dash_health_groups <- function(df) {
  # Get unique identifiers, first column of the df
  unq_id_values <- df[[1]]
  
  # Define health groups
  health_groups <- dash_health_groups()
  
  # Create long df of estimated intakes for PDI groups
  long_df <- data.frame(
    UniqueKey = rep(unq_id_values, times = ncol(df) - 1),
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

#' assign_pdi_scores
#' 
#' Assign DASH scores to each component based on percentile and group classification.
#' 
#' @param long_df long df with assigned ranked percentiles of intakes. Output from assign_percentile.
#' @return Long df with added column for DASH_score.
#' 
#' @author Robbie Pope
#' @keywords internal
#' 
assign_dash_scores <- function(long_df) {
  # Score for PDI
  long_df <- score_logic(long_df,
                         positive_groups = c("positive"),
                         negative_groups = c("negative"),
                         score_name = "DASH_score")
  return(long_df)
}

#' sum_dash_scores 
#' 
#' Sum the ranked percentile scores for DASH score
#' 
#' @param long_df long df with assigned ranked percentile scores. Output from assign_pdi_scores.
#' @return df with summed DASH score for each individual FFQ and estimated intake for each component.
#' @author Robbie Pope
#' @keywords internal
#' 
#' @importFrom dplyr group_by summarise left_join
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' 
sum_dash_scores <- function(long_df) {
  # Compute PDI, hPDI, and uPDI scores for each FFQ
  score_df <- long_df |>
    dplyr::group_by(UniqueKey) |>
    dplyr::summarise(
      DASH  = sum(DASH_score,  na.rm = TRUE),
      .groups = "drop"
    )
  
  # Summarise estimated intake for each PDI food group per FFQ
  intake_df <- long_df |>
    dplyr::group_by(UniqueKey, FoodGroup) |>
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
  final_df <- dplyr::left_join(score_df, intake_df, by = "UniqueKey")
  final_df <- final_df %>% dplyr::rename(FFQ_ID = UniqueKey)
  return(final_df)
}