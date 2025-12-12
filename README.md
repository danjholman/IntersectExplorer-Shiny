# IntersectExplorer

**Author:** Daniel Holman (daniel.holman@sheffield.ac.uk)  
**Created:** June 2025  
**Published version** December 2025
**License:** MIT

---

## Acknowledgements
The starting point for this app was the code in the following paper for the interactive plot (Bell et al. 2024): https://doi.org/10.1016/j.socscimed.2024.116955. The code provided with this paper was used to do the modelling and create your ownn pred2 file which the app uses to generate the plots.

ChatGPT was used to code the app.

## Live Demo

You can try the app online here: https://danielholman.shinyapps.io/IntersectExplorer/

## Overview

IntersectionExplorer is an R/Shiny application that displays predicted scores for an outcome variable over age, stratified by intersectional categories.

For now, as an exemplar, the app uses data from the UKHLS with SF-12 Physical Component Summary (PCS) as the outcome and generation, ethnicity, sex, NS-SEC as the variables defining the intersectional strata.  In time, the app will be modified to allow users to upload their own data.  Users can toggle which strata to include via four panels (“Generation,” “Ethnicity,” “Sex,” “NS-SEC”). Faint grey lines appear for every intersection whether selected or not.

When filters are active, the selected intersection lines are overlaid in colour.

## Features

- **Fixed axes.** The x-axis (age) and y-axis (PCS score) remain constant for easy comparison.
- **Faint background.** If no filters are selected, you see all intersections as light grey lines.
- **Dynamic filtering.** Four panels allow “All/None” toggles for generation, ethnicity, sex, and NS-SEC.
- **Tooltip.** Hovering over a coloured line shows “Generation,” “Intersection,” and “Age.”
- **Minimal dependencies.** Only uses Shiny, ggplot2, plotly, dplyr, and haven.
- **Outcome selector (PCS / MCS).** Toggle between SF-12 PCS and MCS outcomes — title, axis labels and tooltips update automatically.


## Potential Future Features

- Allow users to upload their own data files
- Let users “favorite” particular intersections so they’re always visible
- Add a statistics panel to summarize selected strata
- Show raw intersection counts on hover
- Extent to cross-sectional and other types of models

## Instructions
Use the code here to do the modelling and generate your own pred2 file: https://doi.org/10.1016/j.socscimed.2024.116955

Using the App
Select Model: Choose between PCS (Physical Component Score) or MCS (Mental Component Score) to view different health outcomes
Filter Intersections: Use the checkboxes at the bottom to filter by:
Generation: Z, Y, X, boomer, silent
Ethnicity: White British, African, Bangladeshi, Caribbean, Indian, Mixed, Other, Other White, Pakistani, White Irish
Sex: Female, Male
NS-SEC: Managerial & professional, Intermediate, Routine & manual, No work history
Color by: Change how lines are colored (by Intersection, Ethnicity, Sex, Generation, or NS-SEC)
Randomise Colors: Click to generate a new color palette if intersections are hard to distinguish
Show legend overlay: Toggle the legend on/off for clearer viewing
Interactive Plot:
Hover over lines to see detailed information
Click legend items to toggle visibility
Double-click legend items to isolate a single trace
Export plot as PNG using the camera icon
Understanding the Visualization
Grey lines: All intersectional strata in the dataset (background)
Colored lines: Selected intersections based on your filter choices
Y-axis: Predicted SF-12 score (higher = better health)
X-axis: Age in years
Tips
Start with a single ethnicity, sex, and NS-SEC category to explore specific patterns
Use "All/None" links to quickly select/deselect all options in each category
The randomize colors button is useful when viewing many intersections simultaneously

Using the App
Select Model: Choose between PCS (Physical Component Score) or MCS (Mental Component Score) to view different health outcomes
Filter Intersections: Use the checkboxes at the bottom to filter by:
Generation: Z, Y, X, boomer, silent
Ethnicity: White British, African, Bangladeshi, Caribbean, Indian, Mixed, Other, Other White, Pakistani, White Irish
Sex: Female, Male
NS-SEC: Managerial & professional, Intermediate, Routine & manual, No work history
Color by: Change how lines are colored (by Intersection, Ethnicity, Sex, Generation, or NS-SEC)
Randomise Colors: Click to generate a new color palette if intersections are hard to distinguish
Show legend overlay: Toggle the legend on/off for clearer viewing
Interactive Plot:
Hover over lines to see detailed information
Click legend items to toggle visibility
Double-click legend items to isolate a single trace
Export plot as PNG using the camera icon
Understanding the Visualization
Grey lines: All intersectional strata in the dataset (background)
Colored lines: Selected intersections based on your filter choices
Y-axis: Predicted SF-12 score (higher = better health)
X-axis: Age in years
Tips
Start with a single ethnicity, sex, and NS-SEC category to explore specific patterns
Use "All/None" links to quickly select/deselect all options in each category
The randomize colors button is useful when viewing many intersections simultaneously

## Contributing
Feel free to open issues or pull requests if you find bugs or want to add features (e.g. improved colour palettes, performance optimisations, etc.).

If you adapt this code, please keep the header block at the top of app.R to maintain attribution.

---
