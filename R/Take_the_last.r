# Pseudo-code: Take The Last
# Objective: 
#   Given two items and a list of features for each, the model will 
#   return which one has a larger criterion (i.e. larger outcome)
#
# Inputs:
#   Inputs are:
#   - data: a dataframe with two rows (one per item) and the following columns:
#       - id: item name/identifier column (specified by user, default "Name")
#       - recognized (optional): binary column with 1 indicating the item
#         is recognized and 0 that it is not. If not present, recognition
#         heuristic is skipped.
#       - one or more columns (cues) assumed to be informative on
#         the outcome (criterion of interest). Features can be numeric or binary.
#         Higher values indicate higher expected outcome.
#   - na_heuristic: boolean argument (default TRUE). If TRUE, NA values 
#     are treated as discriminating (item with known value is predicted 
#     higher). If FALSE, features with any NA values are skipped 
#     (non-discriminating).
#   - f_order: ordered vector of feature names ordered by recency of successful 
#     discrimination (i.e., the f_order returned by a previous take_the_last()
#     call). On the first call, you may provide an initial order; if none is 
#     provided, the function initializes it using the feature column order in data.
#
# Outputs: 
#   Model returns a list with:
#   - name: the name/identifier of the item predicted to have higher outcome
#   - cues: vector of feature names that were checked to make the decision
#   - f_order: updated feature order after the decision (move-to-front update: 
#     the discriminating feature is moved to position 1, and all other features
#     keep their relative order).
#
# Assumptions:
# 1. Recency-based Feature Priority (Unequal Importance):
#    - Features are checked in an order determined by how recently they successfully 
#      discriminated in prior comparisons (most recent success first).
#    - Learning is limited to reordering cues (no cue weights/validities are estimated).
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
#    - Assumes recognized items tend to have higher criterion than 
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
#   - f_order:
#     - If f_order is not provided: initialize using the feature column 
#       order in data, excluding the id and the recognized columns
#     - If f_order is provided, restrict it to cue columns in data (drop 
#       id and recognized, and remove any features not present in data).
#
# Step 2: Recognition Heuristic (if available)
#   - If "recognized" column exists in data:
#     - Add "recognized" to checked list
#     - If exactly one item has recognized = 1:
#       - Return that item's name, checked list and f_order
#   - Otherwise, proceed to Step 3
#
# Step 3: Feature Comparison Loop
#   - For each feature in f_order (from first to last):
#     - Add feature to checked list
#     - Find item(s) with maximum value for this feature:
#       - If na_heuristic = TRUE: Non-NA value wins
#       - If na_heuristic = FALSE: if any NA, feature is skipped
#     - If exactly one item has the maximum value (feature discriminates):
#       - Move the discriminating feature to the front of f_order
#       - Return that item's name, checked list and f_order
#     - Else (both have same value OR both NA when na_heuristic=FALSE):
#       - Continue to next feature
#
# Step 4: Random Choice (if no feature discriminated)
#   - If all features checked without finding discrimination:
#     - Randomly sample one of the two item names
#     - Return sampled name, checked list and f_order

take_the_last <- function(data = data, id = "Name", na_heuristic = TRUE, f_order = NULL) {
    # Name of recognition column
    recognized <- "recognized"
    # Preprocessing
    data[[id]] <- as.character(data[[id]])

    # Step 1 - Initialize
    checked = c()
    features = colnames(data)[!colnames(data) %in% c(id, recognized)]
    if (is.null(f_order)) {
        f_order = features
    } else {
        f_order = f_order[f_order %in% features]
    }

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
                        "cues" = checked,
                        "f_order" = f_order))
        }
    }

    # Step 3 - Check features in an order determined by how recently 
    # they successfully discriminated in prior comparisons
    for(feature in f_order) {
        feature <- f_order[1]
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
            # Move the discriminating feature to the front of f_order
            f_order <- c(feature, setdiff(f_order, feature))
            return(list("name" = data[[id]][item_max],
                        "cues" = checked,
                        "f_order" = f_order))
        }
    }

    # Step 4 - if all features are checked
    # Sample randomly one of the 2 items
    return(list("name" = sample(data[[id]], size = 1),
                "cues" = checked,
                "f_order" = f_order))
}