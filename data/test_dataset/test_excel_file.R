# Quick test to verify the Excel experiment file works correctly
library(readxl)

# Test reading the Excel file
cat("🔍 Testing Excel experiment file...\n")
excel_file <- "data/test_dataset/MWM_Experiment_File.xlsx"

# Check if file exists
if (file.exists(excel_file)) {
  cat("✅ File exists:", excel_file, "\n")
  
  # Try to read the file
  tryCatch({
    experiment_data <- read_excel(excel_file)
    
    cat("✅ Successfully read Excel file!\n")
    cat("📊 Data summary:\n")
    cat("   Rows:", nrow(experiment_data), "\n")
    cat("   Columns:", ncol(experiment_data), "\n")
    cat("   Column names:", paste(names(experiment_data), collapse = ", "), "\n")
    
    # Check for required columns
    required_cols <- c("_TrackID", "_TargetID", "_Day", "_Trial", "_Arena", "_TrackFile", "_TrackFileFormat")
    missing_cols <- setdiff(required_cols, names(experiment_data))
    
    if (length(missing_cols) == 0) {
      cat("✅ All required columns present!\n")
    } else {
      cat("❌ Missing required columns:", paste(missing_cols, collapse = ", "), "\n")
    }
    
    # Show first few rows
    cat("\n📋 First 3 rows:\n")
    print(head(experiment_data, 3))
    
    cat("\n🎉 Excel file is ready for use with the MWM Analysis application!\n")
    
  }, error = function(e) {
    cat("❌ Error reading Excel file:", e$message, "\n")
  })
  
} else {
  cat("❌ File not found:", excel_file, "\n")
}
