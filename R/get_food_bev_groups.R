#' get_food_bev_groups
#'
#' Take pre-prepared TwinsUK grams per day .csv files and compute the estimated
#' daily intakes for amalgamated food and beverage groups. Can be standard groupings
#' (Asnicar et al. (2021)), or custom. 
#'
#' @param food.intakes Pre-processed TwinsUK estimated intake .csv file (grams per day).
#'        the first column should be the unique identifier for each questionnaire.
#' @param stnd_groups Character either "Y" (yes) or "N" (no) to determine if 
#'        standard Asnicar et al. (2021) groupings are used.
#' @param alt_groups Named list of custom food group definitions, 
#'        used only when stnd_groups = "N". Each list element name should 
#'        be a food group, and each value a character vector of column names.
#'
#' @return Data frame with summed daily intakes for each food or beverage group.
#' @author Robbie Pope
#' @export
#'
get_food_bev_groups <- function(food.intakes, 
                                stnd_groups = "Y",
                                alt_groups = NULL
                                ) {
  
  # Restrict inputs for stnd_groups
  stnd_groups <- match.arg(stnd_groups, choices = c("Y", "N"))
  
  # Extract unique identifier column (must be first column of grams per day .csv)
  id_col <- food.intakes[[1]]
  
  # Use standard or alternative food and beverage groupings
  if (stnd_groups == "Y") {
    food_groups <- standard_groupings()
  } else {
    if (is.null(alt_groups)) {
      stop("alt_groups: a named list must be provided when stnd_groups = 'N'.", call. = FALSE)
    }
    food_groups <- alt_groups
  }
  
  # Summarise by food group (skipping missing columns)
  summed_cols <- sum_food_groups(food.intakes, food_groups)
  
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

#' standard_groupings 
#' 
#' Define standard amalgamated food and beverage groups based on TwinsUK EPIC 
#' FFQ line items (Asnicar et al. (2021)).
#' 
#' @return named list of standard food groupings based on TwinsUK EPIC FFQ
#' @author Robbie Pope
#' @keywords internal
#' 
standard_groupings <- function() {
  food_groups <- list(
    animal_fat = c('BUTTER', 'REDUCED_FAT_BUTTER'),
    dairy = c('CHEESE', 'COTTAGE_CHEESE', 'DAIRY_DESSERT', 'DOUBLE_CREAM', 
              'FULLFAT_YOGURT', 'ICE_CREAM', 'LOWFAT_CHEESE', 'LOWFAT_YOGURT', 
              'MILK', 'MILK_PUDDINGS', 'SINGLE_CREAM'),
    eggs = c('EGGS'),
    fish_seafood = c('FISH_FINGERS_CAKES', 'FRIED_FISH', 'OILY_FISH', 
                     'FISH_ROE', 'SHELLFISH', 'WHITE_FISH'),
    fruit_juice = c('FRUIT_JUICE', 'SMOOTHIES'),
    fruit = c('APPLES', 'BANANAS', 'DRIED_FRUIT', 'GRAPEFRUIT', 'GRAPES',
              'MELONS', 'ORANGES_MANDARINS', 'PEACHES_PLUMS_APRICOTS', 
              'PEARS', 'BERRIES', 'TINNED_FRUIT'),
    legumes = c('BEANS_BAKED', 'PULSES_LENTILS_BEANS_PEAS', 'TOFU_MEAT_SUBSTITUTE'),
    meat = c('BACON_GAMMON', 'BEEF', 'BEEFBURGER', 'CHICKEN_POULTRY', 
             'CORNED_BEEF', 'HAM_CURED_MEATS', 'LAMB', 'LASAGNE_MOUSSAKA', 
             'LIVER_OFFAL', 'SOUP_MEAT', 'PORK', 'SAUSAGES'),
    misc_animal = c('LOWFAT_SALAD_CREAM', 'MAYONAISE_FULLFAT_SALAD_CREAM', 
                    'PIZZA', 'QUICHE', 'SAVOURY_PIES'),
    nuts_seeds = c('NUTS', 'NUTS_SALTED', 'NUTS_UNSALTED', 
                   'PEANUT_BUTTER', 'SEEDS'),
    potatoes = c('CHIPS', 'CHIPS_ROAST_POTATOES', 'CRISPS', 
                 'POTATOES_BOILED_MASHED', 'ROAST_POTATOES', 'POTATO_SALAD'),
    refined_grains = c('CRACKERS', 'CRISPBREAD', 'CEREAL_BREAKFAST', 'MUESLI', 
                       'NAAN_POPPADOM_TORTILLAS', 'WHITE_BREAD', 'WHITE_PASTA', 'WHITE_RICE'),
    sweetened_beverage = c('COCOA_HOT_CHOCOLATE', 'COFFEE_WHITENER', 'FIZZY_DRINKS', 
                           'FRUIT_SQUASH', 'HORLICKS', 'LOWFAT_COCOA_HOT_CHOCOLATE', 'LOWCAL_FIZZY_DRINKS'),
    sweet_dessert = c('BAKERY_HOMEBAKED', 'BAKERY_READYMADE', 'BISCUIT_CHOCOLATE', 
                      'BISCUIT_PLAIN', 'CAKE_HOMEBAKED', 'CAKE_READYMADE', 'CEREAL_BAR', 
                      'CEREAL_SUGAR_TOPPED', 'CHOCOLATES', 'CHOCOLATE_BARS', 
                      'CHOCOLATE_DARK', 'CHOCOLATE_MILK_WHITE', 'FRUIT_PIES_HOMEBAKED', 
                      'FRUIT_PIES_READYMADE', 'JAM_HONEY', 'REDUCED_FAT_BISCUIT', 
                      'SPONGES_HOMEBAKED', 'SPONGES_READYMADE', 'SUGAR', 'SWEETS_TOFFEES'),
    tea_coffee = c('COFFEE_GROUND_INSTANT', 'COFFEE_DECAFF', 'TEA', 'TEA_FRUIT', 'TEA_GREEN'),
    vegetable_oil = c('FRENCH_DRESSING', 'OTHER_DRESSING', 'SPREAD_OLIVE_OIL'),
    vegetables = c('AVOCADO', 'BEANSPROUTS', 'BEETROOT', 'BROCCOLI_KALE', 'CABBAGE', 
                   'CARROTS', 'CAULIFLOWER', 'COLESLAW', 'COURGETTE_MARROW', 'GARLIC', 
                   'GREEN_BEANS_BROAD_BEANS', 'GREEN_SALAD_CUCUMBER_CELERY', 'KETCHUP', 'LEEKS', 
                   'MARMITE', 'MUSHROOMS', 'ONIONS', 'PARSNIPS_TURNIPS', 'PEAS', 'PEPPERS', 
                   'PICKLES_CHUTNEY', 'SAUCES', 'SOUP_VEGETABLE', 'SPINACH', 'SPROUTS', 
                   'SWEETCORN', 'TOMATOES', 'WATERCRESS'),
    whole_grain = c('BROWN_BREAD', 'BROWN_RICE', 'CEREAL_HIGH_FIBRE', 'PORRIDGE_OATS', 
                    'WHOLEMEAL_BREAD', 'WHOLEMEAL_PASTA'),
    margarine = c('MARGARINE_HARD', 'MARGARINE_OTHER', 'MARGARINE_POLYUNSATURATED', 
                  'SPREAD_CHOLESTEROL_LOWERING', 'LOWFAT_SPREAD', 'VERY_LOWFAT_SPREAD'),
    alcoholic_beverage = c('BEER_CIDER', 'PORT', 'SPIRITS', 'WINE', 'WINE_RED', 'WINE_WHITE')
  )
  return(food_groups)
}

#' sum_food_groups
#' 
#' Define standard amalgamated food and beverage groups based on TwinsUK EPIC 
#' FFQ line items (Asnicar et al. (2021)).
#' 
#' @param food.intakes Pre-processed TwinsUK estimated intake .csv file (grams per day).
#'        the first column should be the unique identifier for each questionnaire.
#' @param named_list Named list of food and beverage group definitions, 
#'        Each list element name should be a food group, and each value a 
#'        character vector of column names.
#' @return A list of numeric vectors (each of length equal to `nrow(data)`), where
#'         each vector represents the summed intake for a food or beverage group.
#' @author Robbie Pope
#' @keywords internal
#'         
sum_food_groups <- function(food.intakes, 
                            named_list
                            ) {
  summed_groups <- lapply(names(named_list), function(group_name) {
    cols <- named_list[[group_name]]
    cols_present <- cols[cols %in% colnames(food.intakes)]
    if (length(cols_present) == 0) {
      return(rep(0, nrow(food.intakes)))
    }
    rowSums(food.intakes[, cols_present, drop = FALSE], na.rm = TRUE)
  })
  return(summed_groups)
}

