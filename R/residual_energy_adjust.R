#' residual_energy_adjust
#'
#' Nutrient density, residual method. Take raw estimated daily nutrient or food 
#' group intakes and adjust based on energy intake. (Willet et al. 1997). Recommended
#' to adjust raw nutrien estiamtes or summarised food groups. To get summarised 
#' food groups use function get_food_bev_groups with stnd_groups argument == 'Y'.
#' Energy variables (e.g. Energy_kcal, Energy_kj) are not adjusted.
#'
#' @param intakes Pre-processed TwinsUK estimated food/nutrient intake .csv file 
#'                (grams per day). The first column should be the unique identifier 
#'                for each questionnaire.
#' @param energy_col Numeric index of the energy intake column used for adjustment.
#' @param start_col Numeric index of the first nutrient or food group column.
#' @param end_col Numeric index of the final nutrient or food group column.
#'
#' @return Data frame containing:
#'         \describe{
#'         \item{metadata}{Original metadata columns}
#'         \item{unadjusted variables}{Original nutrient/food group intakes}
#'         \item{adjusted variables}{Energy-adjusted variables with suffix '_adj'}
#'         }
#'
#' @references Willett et al. (1997) *Adjustment for total energy intake in epidemiologic studies.* Am J Clin Nutr 65(4):1220S-1228S.
#'
#' @author Robbie Pope
#' @importFrom stats lm predict reformulate resid setNames
#' @export
#' 
residual_energy_adjust <- function(intakes,
                                   energy_col,
                                   start_col,
                                   end_col) {
  
  # Validate inputs
  if (!is.numeric(energy_col) || length(energy_col) != 1) {
    stop("energy_col must be a single numeric column index.")
  }
  
  if (!is.numeric(start_col) || length(start_col) != 1) {
    stop("start_col must be a single numeric column index.")
  }
  
  if (!is.numeric(end_col) || length(end_col) != 1) {
    stop("end_col must be a single numeric column index.")
  }
  
  if (energy_col < 1 || energy_col > ncol(intakes)) {
    stop("energy_col is out of range.")
  }
  
  if (start_col < 1 || start_col > ncol(intakes)) {
    stop("start_col is out of range.")
  }
  
  if (end_col < 1 || end_col > ncol(intakes)) {
    stop("end_col is out of range.")
  }
  
  if (start_col > end_col) {
    stop("start_col must be less than or equal to end_col.")
  }
  
  
  df_adj <- intakes
  
  # Extract energy (kcal) and calcualte mean energy intake for population
  energy_name <- names(intakes)[energy_col]
  mean_energy <- mean(intakes[[energy_col]], na.rm = TRUE)
  
  for (col in start_col:end_col) {
    
    col_name <- names(intakes)[col]
    
    # Skip specified energy column
    if (col == energy_col) {
      next
    }
    
    # Skip any energy-related variables
    if (grepl("energy", col_name, ignore.case = TRUE)) {
      next
    }
    
    # Skip non-numeric variables
    if (!is.numeric(intakes[[col]])) {
      next
    }
    
    # Fit model
    fit <- lm(
      reformulate(energy_name, response = col_name),
      data = intakes
    )
    
    # Predict
    pred_mean <- predict(
      fit,
      newdata = setNames(
        data.frame(mean_energy),
        energy_name
      )
    )
    
    # Residualise
    df_adj[[paste0(col_name, "_adj")]] <-
      resid(fit) + as.numeric(pred_mean)
  }
  
  return(df_adj)
  }