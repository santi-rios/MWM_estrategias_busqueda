library(Rtrack)


# read df ----


## Primero leemos el experimento, luego llamamos las estrategias y al final guardamos a un dataframe (results)

experiment = Rtrack::read_experiment("./data/example_data/example_data/Experiment.xlsx",
                                     data.dir = "./data/example_data/example_data/Data")

strategies = Rtrack::call_strategy(experiment$metrics)

results = Rtrack::export_results(experiment)
results_df <- results
dplyr::glimpse(results_df)

# Rows: 300
# Columns: 30
# $ Track_ID                 <chr> "Track_1", "Track_2", "Track_3", "Track_4", "Track_5", "Track_6", "Track_7", "Track_8", "Track_9", "Track_10", "Track_11", "Track_12", "Track_13", "Tra…
# $ `_TargetID`              <chr> "B6_9", "B6_7", "B6_8", "B6_6", "B6_10", "B6_9", "B6_7", "B6_8", "B6_6", "B6_10", "B6_9", "B6_7", "B6_8", "B6_6", "B6_10", "B6_9", "B6_7", "B6_8", "B6_…
# $ `_Day`                   <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2…
# $ `_Trial`                 <dbl> 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5…
# $ `_Arena`                 <chr> "Arena_SW", "Arena_SW", "Arena_SW", "Arena_SW", "Arena_SW", "Arena_SW", "Arena_SW", "Arena_SW", "Arena_SW", "Arena_SW", "Arena_SW", "Arena_SW", "Arena_…
# $ Strain                   <chr> "B6", "B6", "B6", "B6", "B6", "B6", "B6", "B6", "B6", "B6", "B6", "B6", "B6", "B6", "B6", "B6", "B6", "B6", "B6", "B6", "B6", "B6", "B6", "B6", "B6", "…
# $ Probe                    <chr> "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE"…
# $ path.length              <dbl> 2493.00576, 223.01346, 2615.63819, 2419.53196, 345.76194, 381.97711, 2370.54192, 468.20140, 1424.53875, 761.75993, 258.32492, 1024.93959, 233.91994, 15…
# $ total.time               <dbl> 119.90, 8.56, 119.90, 119.90, 12.32, 18.16, 119.90, 17.68, 62.16, 29.52, 10.72, 47.12, 7.52, 70.56, 32.00, 53.76, 98.88, 3.84, 7.04, 70.64, 78.40, 20.2…
# $ velocity                 <dbl> 19.92560, 26.71875, 22.23005, 19.22876, 22.71829, 18.85107, 19.91498, 25.96609, 22.32532, 27.07760, 22.27043, 22.30130, 28.16404, 22.39626, 27.07760, 2…
# $ distance.from.goal       <dbl> 67.36565, 44.72998, 82.28740, 80.73506, 38.28343, 72.71806, 68.81168, 47.92076, 92.36460, 56.41788, 30.26083, 61.40971, 50.68096, 83.57000, 64.85180, 4…
# $ distance.from.old.goal   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
# $ initial.heading.error    <dbl> 46.067833, 17.448427, 31.265426, 104.722809, 28.965158, 57.016047, 46.031182, 9.757917, 98.808344, 32.638901, 29.416066, 28.421060, 15.202202, 20.72720…
# $ initial.trajectory.error <dbl> 82.577704, 57.710694, 39.392183, 87.428118, 59.959755, 98.515484, 60.617890, 45.473567, 127.419751, 82.146228, 73.819803, 67.061569, 60.390354, 57.1783…
# $ initial.reversal.error   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
# $ efficiency               <dbl> 13.043478, 38.095238, 11.764706, 0.000000, 0.000000, 7.142857, 0.000000, 71.875000, 2.564103, 30.303030, 20.000000, 3.508772, 48.000000, 34.615385, 25.…
# $ roaming.entropy          <dbl> 0.9033373, 0.6723984, 0.9121016, 0.8931440, 0.7453272, 0.6982651, 0.9100081, 0.7606001, 0.8879064, 0.8407452, 0.6562170, 0.8555062, 0.6801152, 0.892896…
# $ latency.to.goal          <dbl> 39.12, 7.52, NA, NA, 11.36, 17.28, 52.48, 16.32, NA, 28.24, 8.96, 23.68, 6.32, 68.80, 30.72, 45.36, 96.80, 2.88, 6.00, 66.24, 42.08, 19.44, 2.40, NA, N…
# $ latency.to.old.goal      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
# $ time.in.wall.zone        <dbl> 33.1723333, 0.7133333, 36.1298667, 76.7360000, 2.9409032, 4.2214035, 56.3810702, 4.0800000, 41.3596899, 9.0953514, 2.3028148, 15.8131525, 2.2164211, 33…
# $ time.in.far.wall.zone    <dbl> 21.4221333, 0.4755556, 17.2656000, 21.2622667, 1.5101935, 3.0266667, 10.6666890, 5.2800000, 7.1475969, 4.5476757, 2.4616296, 3.9932203, 1.6623158, 13.4…
# $ time.in.annulus.zone     <dbl> 26.298067, 3.328889, 29.255600, 12.149867, 4.451097, 4.460351, 21.253177, 5.040000, 8.432558, 7.100757, 5.161481, 19.247322, 3.641263, 20.000000, 11.49…
# $ time.in.goal.zone        <dbl> 0.6394667, 1.1096296, 0.0000000, 0.0000000, 0.9538065, 0.3185965, 0.2406020, 1.4400000, 0.0000000, 1.3563243, 1.0322963, 1.5972881, 1.2665263, 1.840000…
# $ time.in.old.goal.zone    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
# $ time.in.n.quadrant       <dbl> 22.221467, 2.140000, 7.833467, 14.547867, 5.961290, 2.150526, 34.406087, 7.040000, 15.017984, 5.505081, 5.399704, 10.222644, 2.849684, 16.720000, 6.064…
# $ time.in.e.quadrant       <dbl> 62.9874667, 5.9444444, 40.4462667, 60.3496667, 2.1460645, 12.1863158, 39.5389298, 10.6400000, 13.8133333, 14.4408649, 5.3202963, 16.8513898, 4.6703158,…
# $ time.in.s.quadrant       <dbl> 22.62113333, 0.00000000, 52.67606667, 22.94086667, 0.00000000, 1.35403509, 24.62160535, 0.00000000, 18.79255814, 4.70724324, 0.00000000, 10.78169492, 0…
# $ time.in.w.quadrant       <dbl> 11.990000, 0.000000, 18.944200, 20.782667, 3.894710, 2.469123, 18.526355, 0.000000, 13.492093, 4.787027, 0.000000, 8.625356, 0.000000, 1.520000, 1.5960…
# $ goal.crossings           <dbl> 1, 1, 0, 0, 2, 1, 1, 1, 0, 1, 2, 2, 1, 1, 1, 2, 1, 2, 1, 2, 2, 1, 1, 0, 0, 2, 1, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 0, 1, 1, 1…
# $ old.goal.crossings       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…



# Graficar variables ----

library(tidyverse)
library(ggpubr) # Gráficos listos para publicar
library(rstatix) # Estadística, Se lleva bien con ggpubr

colnames(results)


# It seems that the error "unexpected symbol _Day" is occurring because the x variable "_Day" is being treated as an unexpected symbol. 

# To fix the error, you can wrap the variable name "_Day" in backticks (`) to correctly define it as a character string. 


results <- results |> rename(dia = '_Day')

ggbarplot(results, 
          x = "dia",  # fix the error by wrapping `_Day` in backticks
          y = "velocity",
          # facet.by = "experimento", 
          color = "Strain",
          fill = "Strain", 
          position = position_dodge(),
          add = "mean_se",
          
          # title = "Distancia Media",
          # xlab = "",
          # ylab = "(m)",
          
          # palette = "npg",
          palette = "lancet",
          ggtheme = theme_pubr())


## density map
library(viridisLite)

## original goal all
Rtrack::plot_density(experiment$metrics)
#> Warning in Rtrack::plot_density(experiment$metrics): Multiple arena definitions
#> have been used. A merged plot may not make sense.


## reversal goal divided by strain

b6.reversal.metrics = experiment$metrics[experiment$factors$Strain == "B6" &
    (experiment$factors$`_Day` == 4 | experiment$factors$`_Day` == 5)]

d2.reversal.metrics = experiment$metrics[experiment$factors$Strain == "D2" &
    (experiment$factors$`_Day` == 4 | experiment$factors$`_Day` == 5)]

par(mfrow = c(1, 2))

Rtrack::plot_density(
  b6.reversal.metrics,
  title = "B6 reversal",
  col = magma(100),
  legend = TRUE,
  feature.col = "white",
  resolution = 900
  )


Rtrack::plot_density(d2.reversal.metrics, title = "D2 reversal")
