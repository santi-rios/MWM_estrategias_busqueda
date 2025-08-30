# MWM Analysis Test Data

This directory contains datasets for testing the Morris Water Maze (MWM) analysis application.

## Available Datasets

### 1. `/example`

A minimal example with basic data:
- `Experiment_Example.csv` - Simple experiment file with a few tracks
- `Track_1.csv` - Single track file for basic testing

### 2. `/test_dataset`

A comprehensive test dataset with:

- `MWM_Experiment_File.xlsx` - Complete experiment file with 32 tracks (Excel format)
- `MWM_Experiment_File.csv` - Same data in CSV format (for reference)
- `Track_1.csv` through `Track_32.csv` - Simulated swimming paths
- Various swimming strategies (direct path, random search, thigmotaxis, etc.)
- 2×2×2 factorial design (Treatment × Stress × Sex)
- 2 days with 4 trials per day
- 4 mice with different characteristics

## Usage Instructions

For testing the MWM Analysis application:

1. Use the **test_dataset** for comprehensive testing of all features
2. Open the application with `source('shiny_app/run_app.R')`
3. Upload `test_dataset/MWM_Experiment_File.xlsx` as the experiment file (Excel format required)
4. Upload all track files from the test_dataset directory
5. Configure analysis parameters and explore the results

For more information, see the README.md file in each dataset directory.
