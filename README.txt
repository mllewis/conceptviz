README - Conceptviz pipeline

(1) using brysbaert concretneess norms, identify cues (total = 345) that are in the top and bottom 6-tile of concreteness (~100) [googledraw.cues.Rmd]
(2) download simplified datasets for these norms from: https://console.cloud.google.com/storage/browser/quickdraw_dataset/full/simplified). These have been scaled so they are all 255 x 255. The simplified versions also do not have time information.
(3) Munge jsons to be in long form and save as csv [munge_jsons.R]
(4) Get summary statistics of drawings [summarize_drawings.R]. Save heatmaps/*_lang_heatmap.pdf and csvs/*_summary.csv for each item. Also writes data to *.Rdata file for faster loading in the future.
(5) [drawing_summary_analysis.Rmd] Summarizes

For movers distance:
(1) [summarize_drawings_dist_not_downsampled.R] Gets movers distances
(2) [mover_distance.Rmd] Summarizes

(1) [summarize_drawings_dist_ds.R] Gets movers distances with down sample n = 1500
(2) [mover_distance_downsampled.Rmd] Summarizes
