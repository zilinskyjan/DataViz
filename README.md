# Data Visualization with R and ggplot

Course materials for a data visualization class taught at TUM (Technical University of Munich).

**Live site:** [https://zilinskyjan.github.io/DataViz/](https://zilinskyjan.github.io/DataViz/)

## Overview

This course teaches students how to create effective visualizations of social and political data using R and ggplot2. The materials cover:

- **Principles of data visualization** — tradeoffs between truthfulness and simplicity, readability, and design choices
- **Working with survey data** — toplines, crosstabs, and weighted calculations
- **Time series visualization** — economic data, stock returns, and rolling correlations
- **Standard charts** — bar charts, scatterplots, boxplots, density plots, and ridgeplots
- **Mixed data types** — combining text analysis with numerical ratings
- **Advanced ggplot techniques** — faceting, themes, custom palettes, and complex layouts
- **Visualizing statistical models** — coefficients, confidence intervals, predicted probabilities, and marginal effects

## Repository Structure

```
├── _quarto.yml           # Quarto book configuration
├── index.qmd             # Course overview and main lessons
├── 1_principles.qmd      # Principles of data visualization
├── 2_toplines-and-cross-tabs.qmd  # Survey data visualization
├── 3_B_timeseries.qmd    # Economic data over time
├── 3_standard-charts.qmd # Standard chart types
├── 4b_tweet_ratings.qmd  # Mixed data (strings and numbers)
├── 4_advanced-ggplot.qmd # Advanced ggplot techniques
├── 5_visualizing-models.qmd  # Visualizing statistical models
├── typology.qmd          # Typology of chart types
├── datasets.qmd          # Available datasets
├── references.qmd        # Useful resources
├── tidyverse-refresher.qmd   # Tidyverse tips
├── class-exercises.qmd   # In-class exercises
├── data/                 # Sample datasets
├── data_macro/           # Macroeconomic data (IMF, WDI)
├── data_AJPS2021/        # Survey data from Uscinski et al. (2021)
└── _book/                # Built HTML output
```

## Datasets Included

- **Nationscape surveys** (first 10 waves) — public opinion data
- **IMF Fiscal Monitor** — government spending, debt, and deficits
- **World Development Indicators** — inflation data
- **Health-adjusted life expectancy** (HALE, 2019)
- **Uscinski et al. (AJPS, 2021)** — conspiracy thinking and populism surveys
- **YouGov Trump tweet ratings** — ratings (by potential voters) of specific Trump tweets; texts of those tweets
- **Stock price data** — NVIDIA, Broadcom, S&P 500

## Prerequisites

- R (version 4.0+)
- RStudio (recommended)
- Key packages: `tidyverse`, `ggplot2`, `haven`, `labelled`, `pollster`, `broom`, `ggrepel`, `ggridges`

## Building the Book

This project uses [Quarto](https://quarto.org/) to build the book. To render locally:

```bash
quarto render
```

The output will be generated in the `_book/` directory.

## Deployment to GitHub Pages

See `how-to-render.txt` for instructions on publishing updates to the live site.

## Author

**Jan Zilinsky**  
Technical University of Munich

## License

Course materials are provided for educational purposes. Please cite appropriately if using in your own teaching.