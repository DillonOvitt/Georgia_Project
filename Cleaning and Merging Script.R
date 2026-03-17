library(dplyr)

# 1. Load the files
# Using check.names = FALSE allows us to clean them manually in the next step
revenue <- read.csv("WebRevenue.csv", check.names = FALSE, colClasses = "character")
expenditure <- read.csv("WebExpenditure.csv", check.names = FALSE, colClasses = "character")
fips_mapping <- read.csv("FIPS mapping_final.csv", check.names = FALSE, colClasses = "character")

# 2. Clean Names: Convert spaces/periods to underscores
# This function handles the "empty name" error and the underscore preference
clean_my_columns <- function(df) {
  # make.names handles empty/duplicate headers by adding .1, .2, etc.
  new_names <- make.names(names(df), unique = TRUE)
  # Replace all periods with underscores
  new_names <- gsub("\\.", "_", new_names)
  # Clean up if any double underscores __ were created
  new_names <- gsub("_+", "_", new_names)
  # Remove trailing underscores
  new_names <- gsub("_$", "", new_names)
  names(df) <- new_names
  return(df)
}

revenue <- clean_my_columns(revenue)
expenditure <- clean_my_columns(expenditure)
fips_mapping <- clean_my_columns(fips_mapping)

# 3. Clean the IDs (Removes any leading/trailing spaces)
# Note: Since we used the function above, "System ID" is now "System_ID"
revenue$System_ID <- trimws(revenue$System_ID)
expenditure$System_ID <- trimws(expenditure$System_ID)
fips_mapping$School_ID <- trimws(fips_mapping$School_ID)

# 4. Merge Revenue and Expenditure
# Joining on the three shared columns to keep the file clean
merged_finances <- inner_join(revenue, expenditure, 
                              by = c("System_ID", 
                                     "System_Name", 
                                     "FTE_Reported_on_2025_QBE_Allotment"))

# 5. Add the FIPS code
# We join System_ID to School_ID
final_dataset <- left_join(merged_finances, 
                           fips_mapping %>% select(School_ID, FIPS), 
                           by = c("System_ID" = "School_ID"))

# 6. Reorder and Save
# This puts FIPS at the front and uses underscores for the saved file
final_dataset <- final_dataset %>%
  select(System_ID, FIPS, everything())

write.csv(final_dataset, "ga_schools_revexp_final.csv", row.names = FALSE)
