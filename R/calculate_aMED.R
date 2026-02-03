#' compute_twinsuk_aMED
#' 
#' The compute_twinsuk_aMED function is a wrapper of calculate_aMED to compute the 
#' aMED diet score using TwinsUK specific parameters and food groupings.
#' aMED computation as described by Fung et al. (2005).
#' TwinsUK specific FFQ line item & nutrient components as described by Asnicar et al. (2021).
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
#' @param sex_col Name of the column containing sex information in \code{est.intakes} (default: 'Sex').
#' @param female_alcohol Numeric vector of length 2 specifying the lower and upper alcohol intake limits for females (default: c(5, 15) g/day).
#' @param male_alcohol Numeric vector of length 2 specifying the lower and upper alcohol intake limits for males (default: c(5, 25) g/day).
#' @param return_medians Logical, whether to return population medians along with final aMED scores (default: TRUE).
#' @return If \code{return_medians = TRUE}, a named list containing:
#'   \describe{
#'     \item{df_scores}{Data frame of final aMED scores and estimated intakes for each individual.}
#'     \item{df_medians}{Data frame of population medians for each aMED component.}
#'   }
#'   If \code{return_medians = FALSE}, a single data frame of final aMED scores and estimated intakes.
#'
#' @references Fung, T. et al. (2005) *Diet-quality scores and plasma concentrations of markers of inflammation and endothelial dysfunction* The American Journal of Clinical Nutrition 82(1):163-173.

#'             Asnicar et al. (2021) *Microbiome connections with host metabolism and habitual diet from 1,098 deeply phenotyped individuals.* Nature Medicine 27(2):321-332.
#' @author Robbie Pope
#' @export 
#' 
compute_twinsuk_aMED <- function(est.intakes, 
                                 nutr.intakes,
                                 merge_col_name_est.intakes = 'FFQ_ID',
                                 merge_col_name_nutr.intakes = 'FFQ_ID',
                                 sex_col = 'Sex',
                                 female_alcohol = c(5, 15),
                                 male_alcohol = c(5, 25),
                                 return_medians = TRUE) {
  
  # Create aggregate food groups for PDI computation
  df_aMED_components <- get_aMED_components(est.intakes = est.intakes, 
                                            nutr.intakes = nutr.intakes, 
                                            merge_col_name_est.intakes = merge_col_name_est.intakes,
                                            merge_col_name_nutr.intakes = merge_col_name_nutr.intakes,
                                            stnd_components = 'Y'
                                            )
  
  # Calculate aMED using TwinsUK defined groupings for aMED components
  df_amed <- calculate_aMED(df_aMED_components,
                            est.intakes,
                            id_col = merge_col_name_est.intakes,
                            sex_col = sex_col,
                            female_alcohol = female_alcohol,
                            male_alcohol = male_alcohol,
                            return_medians = return_medians)
  return(df_amed)
}

#' calculate_aMED
#' 
#' Compute the aMED diet score using TwinsUK specific parameters and components.
#' aMED computation as described by Fung et al. (2005).
#' The calculate_aMED function is a wrapper to perform all necessary steps to compute
#' the score. 
#' 
#' @param df_aMED_components A data frame of estimated intakes for the 9 aMED components (wide format).
#'        The first column must be a unique identifier for each FFQ.
#' @param est.intakes A data frame containing the unique identifier, sex, and estimated intakes for each individual.
#' @param id_col Name of the column containing the unique identifier (default: 'FFQ_ID').
#' @param sex_col Name of the column containing sex information (default: 'Sex').
#' @param female_alcohol Numeric vector of length 2 specifying the lower and upper alcohol intake limits
#'        for females (default: c(5, 15) grams/day).
#' @param male_alcohol Numeric vector of length 2 specifying the lower and upper alcohol intake limits
#'        for males (default: c(5, 25) grams/day).
#' @param return_medians Logical, whether to return population medians along with final scores (default: TRUE).
#'
#' @return If \code{return_medians = TRUE}, a named list containing:
#'   \describe{
#'     \item{df_scores}{Data frame of final aMED scores and estimated intakes for each individual.}
#'     \item{df_medians}{Data frame of population medians for each aMED component.}
#'   }
#'   If \code{return_medians = FALSE}, a single data frame containing only the final aMED scores and intakes.
#'
#' @details
#' The scoring logic follows Fung et al. (2005). Positive components (vegetables, fruit, nuts, whole grains, legumes, fish, MUFA:SFA ratio)
#' are scored 1 if intake is at or above the population median, otherwise 0. Negative components (meat) are scored 1 if intake
#' is below the population median, otherwise 0. Alcohol is scored based on sex-specific ranges.
#' 
#' @references Fung, T. et al. (2005) *Diet-quality scores and plasma concentrations of markers of inflammation and endothelial dysfunction* The American Journal of Clinical Nutrition 82(1):163-173.
#' @author Robbie Pope
#' @export
#' 
calculate_aMED <- function(df_aMED_components,
                           est.intakes,
                           id_col = 'FFQ_ID',
                           sex_col = 'Sex',
                           female_alcohol = c(5, 15),
                           male_alcohol = c(5, 25),
                           return_medians = TRUE) {
  
  # Check for correct parameters
  if (!is.logical(return_medians) || length(return_medians) != 1) {
    stop("`return_medians` must be a single logical value: TRUE or FALSE.")
  }
  
  # Check alcohol ranges
  if (!is.numeric(female_alcohol) || length(female_alcohol) != 2) {
    stop("`female_alcohol` must be a numeric vector of length 2.")
  }
  if (!is.numeric(male_alcohol) || length(male_alcohol) != 2) {
    stop("`male_alcohol` must be a numeric vector of length 2.")
  }
  if (female_alcohol[1] > female_alcohol[2]) {
    stop("`female_alcohol` lower limit cannot be greater than upper limit.")
  }
  if (male_alcohol[1] > male_alcohol[2]) {
    stop("`male_alcohol` lower limit cannot be greater than upper limit.")
  }
  
  # Check ID and Sex columns exist
  if (!id_col %in% colnames(est.intakes)) {
    stop(paste("Specified ID column:", id_col, "not found in est.intakes"))
  }
  if (!sex_col %in% colnames(est.intakes)) {
    stop(paste("Specified Sex column:", sex_col, "not found in est.intakes"))
  }
  
  # Extract sex-specific alcohol ranges
  female_low <- female_alcohol[1]
  female_high <- female_alcohol[2]
  male_low <- male_alcohol[1]
  male_high <- male_alcohol[2]
  
  # Assign aMED components into positive / negative categories
  long_df <- assign_aMED_health_groups(df_aMED_components)
  
  # Assign score based on population median intake / alcohol range
  long_df <- assign_aMED_scores(
    long_df,
    est.intakes,
    id_col = id_col,
    sex_col = sex_col,
    female_low = female_low,
    female_high = female_high,
    male_low = male_low,
    male_high = male_high
    )
  
  # Sum aMED scores and reshape component intakes
  df_scores <- sum_aMED_scores(long_df)
  
  # Conditional return medians
  if (return_medians == T) {
    # Calculate population medians for each component
    df_medians <- get_aMED_medians(long_df)
    return(list(
      df_scores = df_scores,
      df_medians = df_medians
    ))
  } else {
    return(df_scores)
  }
  }

#' get_aMED_components
#'
#' Take pre-prepared TwinsUK grams per day and nutrient estimate .csv files and compute 
#' the estimated daily intakes for amalgamated food, beverage and nutrient components of 
#' the aMED diet score. Standard groupings as described by Asnicar et al. (2021), 
#' or custom groupings for each component can be used. 
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
#'        standard Asnicar et al. (2021) groupings for aMED components are used.
#' @param alt_components Named list of custom food and nutrient component definitions, 
#'        used only when stnd_components = "N". Each list element name should 
#'        be a one of the aMED components, and each value a character vector of column names.
#'
#' @return Data frame with summed daily intakes for each food, beverage or nutrient 
#'         component of the aMED diet score.
#' @author Robbie Pope
#' @export
#'
get_aMED_components <- function(est.intakes, 
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
  
  # Calculate MUFA/SFA ratio
  nutr.intakes$mufa_sfa_ratio <- nutr.intakes$MUFA_total_g / nutr.intakes$SFA_total_g
  
  # Use standard or alternative food and beverage groupings
  if (stnd_components == "Y") {
    food_groups <- aMED_standard_groupings()
  } else {
    if (is.null(alt_groups)) {
      stop("alt_groups: a named list must be provided when stnd_groups = 'N'.", call. = FALSE)
    }
    food_groups <- alt_groups
  }
  
  # Summarise by food group (skipping missing columns)
  summed_cols <- sum_aMED_components(est.intakes = est.intakes,
                                     nutr.intakes = nutr.intakes,
                                     named_list = food_groups)
  
  # Combine into final data frame — first col is ID
  summed_df <- data.frame(
    FFQ_ID = id_col,
    as.data.frame(summed_cols, check.names = FALSE),
    stringsAsFactors = FALSE
  )
  
  # Assign the correct group names
  colnames(summed_df)[-1] <- names(food_groups)
  
  # Check lengths must match
  stopifnot(length(summed_df$FFQ_ID) == nrow(summed_df))
  
  return(summed_df)
  }

#' aMED_standard_groupings 
#' 
#' Define the nine aMED amalgamated food, beverage and nutrient components 
#' Methodology as described by Fung et al. (2005)
#' FFQ line item and nutrient groupings according to Asnicar et al. (2021).
#' 
#' @return named list of aMED food & nutrient components for TwinsUK EPIC FFQ
#' @author Robbie Pope
#' @keywords internal
#' 
aMED_standard_groupings <- function() {
  food_groups <- list(
    vegetables = c('AVOCADO', 'BEANSPROUTS', 'BEETROOT', 'BROCCOLI_KALE', 'CABBAGE',
                   'CARROTS', 'CAULIFLOWER', 'GARLIC', 'GREEN_SALAD_CUCUMBER_CELERY',
                   'LEEKS', 'COURGETTE_MARROW', 'MUSHROOMS', 'ONIONS', 'PARSNIPS_TURNIPS',
                   'PEPPERS', 'SPINACH', 'SPROUTS', 'SWEETCORN', 'TOMATOES', 
                   'SOUP_VEGETABLE', 'WATERCRESS'),
    fruit = c('APPLES', 'BANANAS', 'BERRIES', 'DRIED_FRUIT', 'FRUIT_JUICE', 'GRAPEFRUIT', 
              'GRAPES', 'MELONS', 'ORANGES_MANDARINS', 'PEACHES_PLUMS_APRICOTS', 
              'PEARS', 'TINNED_FRUIT'),
    whole_grain = c('BROWN_BREAD', 'BROWN_RICE', 'CEREAL_HIGH_FIBRE', 'CRISPBREAD', 'MUESLI',
                    'PORRIDGE_OATS', 'WHOLEMEAL_BREAD', 'WHOLEMEAL_PASTA'),
    nuts = c('NUTS_SALTED', 'NUTS_UNSALTED', 'PEANUT_BUTTER'),
    meat = c('BACON_GAMMON', 'BEEF', 'BEEFBURGER', 'CORNED_BEEF', 'HAM_CURED_MEATS', 
             'LAMB', 'LASAGNE_MOUSSAKA', 'LIVER_OFFAL', 'PORK', 'SAUSAGES', 
             'SAVOURY_PIES', 'SOUP_MEAT'),
    legumes = c('BEANS_BAKED', 'GREEN_BEANS_BROAD_BEANS', 'PEAS', 'PULSES_LENTILS_BEANS_PEAS'),
    fish_seafood = c('FISH_FINGERS_CAKES', 'FISH_ROE', 'FRIED_FISH', 'OILY_FISH', 'SHELLFISH',
                     'WHITE_FISH'),
    fa_ratio = c('mufa_sfa_ratio'),
    alcohol = c('Alcohol_g')
  )
  return(food_groups)
  }

#' sum_aMED_components
#' 
#' Define standard amalgamated food and nutrient components of aMED diet score 
#' for TwinsUK EPIC FFQ
#' FFQ line items (Asnicar et al. (2021)).
#' 
#' @param est.intakes Pre-processed TwinsUK estimated intake .csv file (grams per day).
#'        the first column should be the unique identifier for each questionnaire.
#' @param nutr.intakes Pre-processed TwinsUK estimated nutrient intakes .csv file,
#'        the first column should be the unique identifier for each questionnaire.
#' @param named_list Named list of food and nutrient group components, 
#'        Each list element name should be a food/nutrient component of the aMED diet, 
#'        and each value a character vector of column names.
#' @return A list of numeric vectors (each of length equal to `nrow(data)`), where
#'         each vector represents the summed intake for a food or nutrient component.
#' @author Robbie Pope
#' @keywords internal
#'         
sum_aMED_components <- function(est.intakes,
                                nutr.intakes, 
                                named_list
                                ) {
  
  # Get unique identifier
  id_col <- colnames(est.intakes)[1]
  df_merge <- merge(est.intakes, nutr.intakes, 
                    by.x = id_col,
                    by.y = id_col)

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

#' aMED_health_groups 
#' 
#' Define the healthy or less-healthy groups for the aMED diet score. 
#' Methodology as described by Fung et al. (2005)
#' 
#' @return names list of healthy and less healthy aMED components for scoring
#' @author Robbie Pope
#' @keywords internal
#' 
aMED_health_groups <- function() {
  health_groups <- list(
    positive = c('vegetables', 'fruit', 'nuts', 'whole_grain', 
                   'legumes', 'fish_seafood', 'fa_ratio'),
    negative = c('meat'),
    alcohol = c('alcohol')
  )
  return(health_groups)
  }

#' assign_aMED_health_groups
#' 
#' Based on the aMED components as described by Fung et al. (2005).
#' 
#' @param df Dataframe of aMED score components. Output of get_aMED_components
#' @return df in long format of aMED components and healthful label.
#' @author Robbie Pope
#' @keywords internal
#' 
assign_aMED_health_groups <- function(df) {
  # Get unique identifiers, first column of the df
  unq_id_values <- df[[1]]
  
  # Define health groups
  health_groups <- aMED_health_groups()
  
  # Create long df of estimated intakes for aMED groups
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
  
  # Remove the other category as not included in calculation
  long_df <- long_df[long_df$HealthfulGroup != "other", ]
  
  # Return long_df for pdi calculation
  return(long_df)
  }

#' get_aMED_medians 
#' 
#' Compute median intake for each component of the aMED score. 
#' 
#' @param long_df long df of etimated intakes of aMED components. Output from assign_aMED_health_groups
#' @return medians_df, a df of median intakes from the long_df
#' @author Robbie Pope
#' @keywords internal
#' @importFrom stats aggregate median
#' 

get_aMED_medians <- function(long_df) {

  
  # Compute medians for study population
  medians_df <- aggregate(
    Estimated_Intake ~ FoodGroup,
    data = long_df,
    FUN = median,
    na.rm = TRUE
  )
  
  # Rename median column
  colnames(medians_df)[2] <- "Median_Intake"
  
  return(medians_df)
  }

#' aMED_score_logic
#' 
#' Below median intake is scored 0 for positive foods whereas equal to or above 
#' intake is scored 1 for negative foods. For alcohol the score is sex specific 
#' and based on a range. Default or Females is a score of 1 if between 5-15g d-1, 
#' otherwise 0. For Males, 1 if between 5-25g d-1, otherwise 0. Range can be updated 
#' in the function parameters. 
#' 
#' @param row row of long_df to score based on population median
#' @param medians_df df of study population median intakes, output of get_aMED_medians
#' @param female_low Lower limit of healthy female alcohol range.
#' @param female_high Upper limit of healthy female alcohol range.
#' @param male_low Lower limit of healthy male alcohol range.
#' @param male_high Upper limit of healthy male alcohol range.
#' @return score of 1 or 0 depending on aMED scoring logic.
#' @author Robbie Pope
#' @keywords internal
#' 
aMED_score_logic <- function(row,
                             medians_df,
                             female_low = female_low,
                             female_high = female_high,
                             male_low = male_low,
                             male_high = male_high) {
  
  # Extract relevant information from the input row and median list
  food_group <- row$FoodGroup
  favour <- row$HealthfulGroup
  sex <- row$Sex
  portion <- row$Estimated_Intake
  median_value <- medians_df[medians_df$FoodGroup == food_group, ][, 2]
  
  # Separate alcohol component due to sex specific scoring approach
  if (favour == "alcohol") {
    if (sex == "F") {
      if (portion > female_low && portion <= female_high) {
        return(1)
      } else {
        return(0)
      }
    } else if (sex == 'M') {
      if (portion > male_low && portion <= male_high) {
        return(1)
      } else {
        return(0)
      }
    }
    
    # For all positive components, score 1 for greater than median
    } else if (favour == 'positive') {
      if (portion < median_value) {
      return(0)
        } else {
          return(1)
          }
      # For all negative components, score 1 for less than median
      } else if (favour == 'negative') {
        if (portion < median_value) {
          return(1)
          } else {
            return(0)
            }
        # If none of the above return NA value
        } else {
          return(NA)
        }
  }

#' assign_aMED_scores
#' 
#' Assign aMED scores to each component based on if intake is above or below study
#' population median or within specified alcohol range. 
#' 
#' @param long_df long df of etimated intakes of aMED components. Output from assign_aMED_health_groups 
#' @param est.intakes Pre-processed TwinsUK estimated intake .csv file (grams per day).
#'        the first column should be the unique identifier for each questionnaire.
#' @param id_col Name of the column containing the unique identifier (default: 'FFQ_ID').
#' @param sex_col Name of the column containing sex information (default: 'Sex').
#' @param female_low Lower limit of female alcohol range.
#' @param female_high Upper limit of female alcohol range.
#' @param male_low Lower limit of male alcohol range.
#' @param male_high Upper limit of male alcohol range.
#' @return Long df with added column for aMED_score.
#' 
#' @author Robbie Pope
#' @keywords internal
#' 
assign_aMED_scores <- function(long_df,
                               est.intakes,
                               id_col = 'FFQ_ID',
                               sex_col = 'Sex',
                               female_low = 5,
                               female_high = 15,
                               male_low = 5,
                               male_high = 25) {
  
  # Get medians
  medians_df <- get_aMED_medians(long_df)
  
  # Merge to get Sex column
  df_sex <- est.intakes[, c(id_col, sex_col)]
  df_merge <- merge(long_df, df_sex, by=id_col)
  
  # Score each component of the aMED diet index
  df_merge$aMED_component <- sapply(1:nrow(df_merge), function(i) aMED_score_logic(df_merge[i, ], 
                                                                                   medians_df,
                                                                                   female_low,
                                                                                   female_high,
                                                                                   male_low,
                                                                                   male_high
                                                                                   )
                          )
  return(df_merge)
  }

#' sum_aMED_scores 
#' 
#' Sum the median based score for each aMED component to give the final aMED score
#' 
#' @param long_df long df with scored components. Output from assign_aMED_scores.
#' @return df with summed aMED score for each individual FFQ and estimated intake for each component.
#' @author Robbie Pope
#' @keywords internal
#' 
#' @importFrom dplyr group_by summarise left_join
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' 
sum_aMED_scores <- function(long_df) {
  # Compute aMED score for each FFQ
  score_df <- long_df %>%
    dplyr::group_by(FFQ_ID) %>%
    dplyr::summarise(
      aMED = sum(aMED_component, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Summarise estimated intake for each food group per FFQ
  intake_df <- long_df %>%
    dplyr::group_by(FFQ_ID, FoodGroup) %>%
    dplyr::summarise(
      Estimated_Intake = sum(Estimated_Intake, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = FoodGroup,
      values_from = Estimated_Intake,
      values_fill = 0
    )
  
  # Join total aMED scores with food group intakes
  final_df <- dplyr::left_join(score_df, intake_df, by = "FFQ_ID")
  return(final_df)
}