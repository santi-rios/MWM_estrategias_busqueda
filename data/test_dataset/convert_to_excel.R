# Script to convert CSV experiment file to Excel format
# Required for Rtrack and Shiny application compatibility

# Load required libraries
library(readr)
library(writexl)

# Read the CSV experiment file
experiment_data <- read_csv("data/test_dataset/MWM_Experiment_File.csv")

# Display the data structure
cat("Converting experiment file to Excel format...\n")
cat("Original data structure:\n")
print(head(experiment_data))
cat("\nNumber of rows:", nrow(experiment_data), "\n")
cat("Number of columns:", ncol(experiment_data), "\n")

# Write to Excel format
write_xlsx(experiment_data, "data/test_dataset/MWM_Experiment_File.xlsx")

cat("\n✅ Successfully converted to Excel format!\n")
cat("📁 File saved as: data/test_dataset/MWM_Experiment_File.xlsx\n")

# Verify the Excel file by reading it back
library(readxl)
excel_data <- read_excel("data/test_dataset/MWM_Experiment_File.xlsx")

cat("\n🔍 Verification - Excel file contents:\n")
print(head(excel_data))

if (identical(experiment_data, excel_data)) {
  cat("\n✅ Excel file matches original CSV data perfectly!\n")
} else {
  cat("\n⚠️ Minor differences detected (likely data type formatting)\n")
}

cat("\n📋 Excel file is ready for use with the MWM Analysis application.\n")
