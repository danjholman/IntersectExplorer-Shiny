# IntersectionExplorer

**Author:** Daniel Holman (daniel.holman@sheffield.ac.uk)  
**Created:** June 2025  
**License:** MIT

---

## Acknowledgements
The starting point for this app was the code in the following paper for the interactive plot (Bell et al. 2024): https://doi.org/10.1016/j.socscimed.2024.116955. The code provided with this paper was used to generate the pred2 file which the app uses to generate the plots.

ChatGPT was used to code the app.

## Live Demo

You can try the app online here:  
[IntersectionExplorer on shinyapps.io](https://danielholman.shinyapps.io/app_v8/)

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
To be added - see above - use the code here to generate your own pred2 file: https://doi.org/10.1016/j.socscimed.2024.116955

## Contributing
Feel free to open issues or pull requests if you find bugs or want to add features (e.g. improved colour palettes, performance optimisations, etc.).

If you adapt this code, please keep the header block at the top of app.R to maintain attribution.

---
