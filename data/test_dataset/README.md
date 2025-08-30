# Morris Water Maze Test Dataset

This directory contains a complete set of test data for the Morris Water Maze (MWM) analysis tool. These files can be used to test the application's functionality and demonstrate expected results.

## Dataset Contents

1. **MWM_Experiment_File.xlsx** - The experiment metadata file in Excel format (required by Rtrack)
2. **MWM_Experiment_File.csv** - The same data in CSV format (for reference/backup)
3. **Track_1.csv to Track_32.csv** - 32 track files with simulated swimming paths
4. **generate_test_tracks.R** - The R script used to generate these test tracks
5. **convert_to_excel.R** - Script to convert CSV to Excel format

## Dataset Design

This test dataset simulates a complete Morris Water Maze experiment with the following design:

- **4 mice** (Mouse_1 through Mouse_4)
- **2 days** of testing (Day 1 and Day 2)
- **4 trials** per day for each mouse
- **2 experimental groups** (Group_A = Control, Group_B = Treatment)
- **2 stress conditions** (Yes/No)
- **2 sexes** (Male/Female)

This factorial design allows testing various grouping and analysis options.

## Swimming Strategies

The track files contain simulated swimming paths that represent common strategies observed in MWM experiments:

1. **Direct Path (Spatial Strategy)** - Direct navigation to the platform location
2. **Random Search** - Exploratory swimming covering the entire pool area
3. **Thigmotaxis** - Swimming near the walls of the pool (anxiety-related)
4. **Scanning** - Searching in a circular pattern
5. **Chaining** - Swimming at a fixed distance from the center

The probability of each strategy varies by:
- **Day** (Day 2 shows improved learning compared to Day 1)
- **Treatment** (Treatment group shows better performance than Control)

## Usage Instructions

### In the Shiny App

1. Start the application: `source('shiny_app/run_app.R')`
2. Upload **MWM_Experiment_File.xlsx** as the experiment file (Excel format required)
3. Upload all **Track_*.csv** files as track files
4. Configure the arena and analysis parameters
5. Run the analysis to see results

### In the Simple Analysis Script

1. Update the file paths in `simple_analysis.R`:

   ```r
   experiment_file <- "data/test_dataset/MWM_Experiment_File.xlsx"
   tracks_folder <- "data/test_dataset/"
   ```

2. Execute: `source('simple_analysis.R')`

## Expected Results

When analyzing this dataset, you should observe:
- Improved performance on Day 2 versus Day 1
- Better performance in the Treatment group versus Control
- Density maps showing different search patterns between groups
- Strategy classification showing learning progression

## Customization

You can modify the **generate_test_tracks.R** script to create different swimming patterns or experimental designs for additional testing scenarios.

---

This test dataset was generated using simulated data and is intended for demonstration and testing purposes only.
