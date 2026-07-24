# Tourism Slogans Analysis Tool

An R Shiny application for exploring and analyzing destination tourism slogans through sentiment/affective dimensions (valence, arousal, dominance), thematic patterns (nature, culture, adventure, etc.),  linguistic feature detection (rhyme, alliteration, simile, personification, etc.), and interactive geographic mapping. It was developed to support the analysis in [paper]; it is not intended as a general-purpose package.

This repository accompanies the manuscript:

> [Author names]. [Year]. [Paper title]. *[Journal name]* (under review). DOI: [add once available]

## Contents

```
.
├── app.R                  # Shiny application (UI + server)
├── data/
│   └── slogans_data.csv   # Tourism slogan dataset used in the paper
├── README.md
└── LICENSE
```

## Requirements

- R (>= 4.1 recommended)
- RStudio (optional, but convenient for running Shiny apps)

### R packages

```r
install.packages(c(
  "shiny", "shinydashboard", "DT", "plotly", "ggplot2", "tidyverse",
  "quanteda", "quanteda.textplots", "quanteda.textstats",
  "wordcloud", "RColorBrewer", "gridExtra", "shinycssloaders",
  "readxl", "rnaturalearth", "sf", "stringr", "cluster"
))
```

## How to run

1. Clone this repository:
   ```bash
   git clone https://github.com/<your-username>/<repo-name>.git
   cd <repo-name>
   ```
2. Open `app.R` in RStudio and click **Run App**, or run from the R console:
   ```r
   shiny::runApp("app.R")
   ```
3. Alternatively, run directly from GitHub without cloning:
   ```r
   shiny::runGitHub("<repo-name>", "<your-username>")
   ```
4. In the app, use the file upload widget to load `data/slogans_data.csv` (or your own data formatted per the schema below).

## Data

`data/slogans_data.csv` contains one row per country/slogan. The app expects (at minimum) the following columns:

| Column        | Type        | Description |
|---------------|-------------|--------------|
| `country`     | character   | Country name |
| `slogan`      | character   | Tourism slogan text |
| `cluster`     | integer/factor | Cluster assignment from the clustering analysis |
| `wc`          | numeric     | Total word count of the slogan |
| `cw`          | numeric     | Content word count (function words removed) |
| `cwr`         | numeric     | Content word ratio (`cw / wc`) |
| `valence`     | numeric     | Affective valence score |
| `arousal`     | numeric     | Affective arousal score |
| `dominance`   | numeric     | Affective dominance score |
| `ranking`     | numeric     | Country ranking by total VAD score |
| `region`      | character   | Geographic region (used for map filtering) |
| `sub_region`  | character   | Geographic sub-region (used for map filtering) |
| `iso_alpha3`  | character   | ISO 3166-1 alpha-3 country code (used to join to map geometry via `rnaturalearth`) |

> **Note:** Variable `cluster` may be assigned by user after conducting cluster analysis using total valence/arousal/dominance score. See the citation below for methodology in finding VAD score. Variable 'country' represents countries, dependent territories, and special areas of geographical interest.

**Data provenance:** The `slogan` and `country` fields were compiled from publicly available national/destination tourism marketing campaigns for research purposes. All derived variables (`cluster`, `wc`, `cw`, `cwr`, `valence`, `arousal`, `dominance`, `ranking`) are original analysis by the author(s).

## What the app does

- **Linguistic feature detection**: Flags rhyme, alliteration, simile, personification, imperative/interrogative structure, sensory language, metaphor, hyperbole, inclusive/direct/submissive pronoun use, and more.
- **Thematic tagging**: Classifies slogans along dimensions such as nature, culture, urban, adventure, relaxation, luxury, friendliness, romance, authenticity, and uniqueness using a keyword dictionary approach.
- **Interactive mapping**: Visualizes clusters and rankings geographically by region/sub-region using `sf` and `rnaturalearth`.
- **Downloadable outputs**: Exports thematic tagging, linguistic feature summaries, and within-cluster dissimilarity matrices as CSV files.

## Citation

If you use this code or data, please cite:

```
[Author names] ([Year]). [Paper title]. [Journal name]. [DOI]
```

## License

- **Code**: MIT — see `LICENSE`
- **Data**: CC-BY 4.0. The compiled dataset (slogans, country identifiers, and all derived analytical variables) is released under CC-BY 4.0. Note that this license applies to the authors' compilation and analysis; the tourism slogans themselves are short promotional phrases used here for research/commentary purposes and remain the property of their respective national/destination tourism boards.

## Contact

Issues and pull requests welcome.
