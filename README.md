# Being Sick First
### On Bruce Campbell, cancer, and the things we feel entitled to know

*Notes from the Abstract* — Andrew R. Crocker

---

## About

This repository contains the R code and chart outputs accompanying the *Notes from the Abstract* piece "Being Sick First," which examines celebrity health disclosure through a public health infrastructure lens using three case studies: Angelina Jolie (BRCA), Kylie Minogue (breast cancer), and Bruce Willis (frontotemporal dementia). https://andrewrcrocker.substack.com/p/being-sick-first

---

## Contents

| File | Description |
|------|-------------|
| `disclosure_effect_chart.R` | R script generating all three charts |
| `chart_jolie.png` | BRCA testing rates, United States, 2007–2016 |
| `chart_kylie.png` | Breast screening index, Australia, 2003–2007 |
| `chart_willis.png` | FTD search interest, United States, 2022–2023 |

---

## Data Sources

- **Jolie panel:** Reconstructed from interrupted time series parameters in Liede et al. (2018), *Breast Cancer Research and Treatment*. DOI: [10.1007/s10549-018-4824-9](https://doi.org/10.1007/s10549-018-4824-9). Based on US insurance claims data from approximately 46 million commercially-insured women.
- **Kylie panel:** Modeled from effect sizes in Kelaher et al. (2008), *International Journal of Epidemiology*. DOI: [10.1093/ije/dyn090](https://doi.org/10.1093/ije/dyn090).
- **Willis panel:** Reproduced from pre- and post-disclosure Google Trends averages in Hurley et al. (2023), *Innovation in Aging*. DOI: [10.1093/geroni/igad125](https://doi.org/10.1093/geroni/igad125).

Reconstructed values are illustrative of published findings and are labeled as such in each chart caption.

---

## Requirements

```r
install.packages(c("ggplot2", "dplyr", "lubridate"))
```

Charts are output to the working directory at 1456×816px, 150dpi, sized for Substack's maximum post width.

---

## Usage

Open `disclosure_effect_chart.R` in RStudio and source it. Charts will save to `/Users/crocker/Downloads/Evil Dead`. To change the output location, edit the `setwd()` call at the top of the script.

---

## License

Copyright (c) 2026 Andrew R. Crocker. All rights reserved.
