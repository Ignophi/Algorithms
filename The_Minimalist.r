# Pseudo-code: The Minimalist
# Objective: 
#   Given two items and a list of features for each, the model will 
#   return which one has a larger criteria (i.e. larger outcome)
#
# Inputs:
#   Input is expected to be a dataframe with two rows, one for each
#   item, and the following columns:
#   - id: item name/identifier column (specified by user, default "Name")
#   - recognized (optional): binary column with 1 indicating the item
#     is recognized and 0 that it is not. If not present, recognition
#     heuristic is skipped.
#   - features: one or more columns (cues) assumed to be informative on
#     the outcome (criteria of interest). Features can be numeric or binary.
#     Higher values indicate higher expected outcome.
#   - na_heuristic: boolean argument (default TRUE). If TRUE, NA values 
#     are treated as discriminating (item with known value is predicted 
#     higher). If FALSE, features with any NA values are skipped 
#     (non-discriminating).
#
# Outputs: 
#   Model returns a list with:
#   - name: the name/identifier of the item predicted to have higher outcome
#   - cues: vector of feature names that were checked to make the decision
#
# Assumptions:
# 1. No Feature Hierarchy (Equal Importance):
#    - Features are sampled randomly with equal probability
#    - No feature is assumed to be more predictive or important than others
#    - This differs from models like "Take The Best" which rank features by 
#      validity
#
# 2. Ordinal Comparison Only (Magnitude Irrelevant):
#    - Only the direction of difference matters (which item is larger)
#    - The magnitude of difference between items on a feature is not considered
#    - A feature where items differ by 0.1 is treated the same as one where 
#      they differ by 10
#
# 3. First-Discriminating-Cue Stopping Rule:
#    - Decision is made on the first feature that discriminates between items
#    - No integration or aggregation of information across multiple features
#    - Once a discriminating cue is found, all other features are ignored
#
# 4. Recognition as a Valid Predictor (if recognition data available):
#    - Assumes recognized items tend to have higher criteria values than 
#      unrecognized items
#    - Recognition is checked before any features
#
# 5. Missing Data as Signal (if na_heuristic = TRUE):
#    - Assumes items with known feature values rank higher than items with 
#      missing values
#    - Treats data unavailability as informationally meaningful
#
# 6. Binary Choice Framework:
#    - Model always predicts one item over another (no ties or uncertainty)
#    - Random choice is used when no information discriminates
#
# Algorithm:
# Step 1: Initialize
#   - checked = empty vector to track checked features
#   - features = all column names except id column
#
# Step 2: Recognition Heuristic (if available)
#   - If "recognized" column exists in data:
#     - Add "recognized" to checked list
#     - If exactly one item has recognized = 1:
#       - Return that item's name and checked list
#   - Otherwise, proceed to Step 3
#
# Step 3: Feature Comparison Loop
#   - While unchecked features remain:
#     - Randomly sample one unchecked feature
#     - Add feature to checked list
#     - Find item(s) with maximum value for this feature:
#       - If na_heuristic = TRUE: NAs are ignored, non-NA value wins
#       - If na_heuristic = FALSE: if any NA exists, feature is skipped
#     - If exactly one item has the maximum value (feature discriminates):
#       - Return that item's name and checked list
#     - Else (both have same value OR both NA when na_heuristic=FALSE):
#       - Continue to next feature
#
# Step 4: Random Choice (if no feature discriminated)
#   - If all features checked without finding discrimination:
#     - Randomly sample one of the two item names
#     - Return sampled name and checked list

the_minimalist <- function(data = data, id = "Name", na_heuristic = TRUE) {
    # Name of recognition column
    recognized <- "recognized"
    # Preprocessing
    data[[id]] <- as.character(data[[id]])

    # Step 1 - Initialize checked list
    checked = c()
    features = colnames(data)[colnames(data) != id]

    # Step 2 - Recognition heuristic
    # Check if recognized column provided
    if (recognized %in% colnames(data)) {
        # Add feature to checked list
        checked <- c(checked, recognized)
        # Check recognition cue
        recon <- data[[recognized]] == 1
        # If only one item recognized
        if (sum(recon, na.rm = T) == 1) {
            return(list("name" = data[[id]][which(recon)],
                        "cues" = checked))
        }
    }

    # Step 3 - While unchecked features remain
    while(length(checked) != length(features)) {
        # Randomly choose an unchecked feature
        feature = sample(features[!features %in% checked], size = 1)
        # Add feature to checked list
        checked <- c(checked, feature)
        # Get item with max cue
        item_max <- 
            which(data[[feature]] == suppressWarnings(max(data[[feature]],
                                                          na.rm = na_heuristic)))
        # If na_heuristic is false and there is at least 1 NA
        # length(item_max) == 0
        # If items has same feature value, then length(item_max) == 2
        if(length(item_max) != 0 && length(item_max) != 2) {
            return(list("name" = data[[id]][item_max],
                        "cues" = checked))
        }
    }

    # Step 4 - if all features are checked
    # Sample randomly one of the 2 items
    return(list("name" = sample(data[[id]], size = 1),
                "cues" = checked))
}


library(heuristica)
data(city_population)
data(highschool_dropout)

# Prepare data
data <- city_population[1:2,]
# Exclude true outcome column
outcome <- data[, colnames(data) %in% c("Running_Number")]
# Add recognition column
data$recognized <- c(0, 0)

#data[1, 3] <- NA
the_minimalist(data, na_heuristic = T)