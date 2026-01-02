# Immigration and Economic Factors between Australia and the United States

## Overview
This project presents an interactive narrative visualization dashboard exploring the relationship between economic factors and immigration movements between Australia and the United States, with particular attention to the impact of the Australia-United States Free Trade Agreement (AUSFTA).

The dashboard is designed to help users understand how population movements relate to economic indicators such as GDP growth, unemployment, inflation, trade volumes, and stock market performance over time.

This work was developed as part of an applied data visualization project and combines statistical analysis, interactive visual design, and storytelling principles to make complex economic relationships accessible to a broad audience.

---

## Key Research Questions
The visualization focuses on answering the following questions:

- How did AUSFTA affect immigration patterns between Australia and the United States?
- What is the relationship between economic indicators and immigrant populations?
- How do trade volumes correlate with population movements?
- Do economic factors influence Australians born in the USA differently from Americans born in Australia?

---

## Target Audience
This project is designed for:

- Policymakers and government officials
  Understanding the interaction between economic policy and migration trends.
- Researchers and analysts
  Exploring international migration patterns and economic relationships.
- Potential migrants and business professionals
  Gaining insight into how economic conditions influence migration opportunities.

---

## Dashboard Structure
The application follows a narrative visualization flow, guiding users from simple trends to more complex analyses:

1. Welcome Page
   Introduction, objectives, and guided tutorial.
2. Population Changes Analysis
   Immigration trends before and after AUSFTA using animated bar races and boxplots.
3. Australians Born in the United States
   - Interactive scatterplots
   - Radar charts (explanatory power of economic factors)
   - Bubble charts (correlation strength and direction)
4. Americans Born in Australia
   A mirrored structure for direct comparison.
5. Economic Indicators Correlation Heatmap
   Overview of relationships among all economic variables.
6. Summary and Insights
   Key findings tailored to different audience groups.
7. Appendix
   Data sources and additional references.

The structure follows Shneiderman's "Overview first, zoom and filter, then details-on-demand" principle and a Martini Glass narrative design.

---

## Design and Visualization Principles
- Multi-view coordination: Scatterplots, radar charts, bubble charts, and heatmaps provide complementary perspectives.
- Colour encoding:
  - Blue -> positive relationships
  - Red -> negative relationships
- Progressive disclosure: Users are introduced to complexity gradually.
- Accessibility:
  - Clear explanations
  - Interactive tooltips
  - Consistent visual encoding

The design is grounded in established visualization theory, including Munzner's Nested Model and narrative visualization principles.

---

## Technical Implementation
The dashboard is implemented using R and Shiny, with the following key technologies:

- R Shiny - interactive web application framework
- ggplot2 - static visualizations
- plotly - interactive charts
- gganimate - animated bar charts
- dplyr / tidyr / reshape2 - data manipulation
- rintrojs / shinyjs - guided tutorials and user assistance

While the original design considered D3.js, R Shiny was chosen for faster implementation and stronger integration with statistical analysis.

---

## System Requirements
- R >= 4.2.0
- RStudio (recommended)
- Minimum 8 GB RAM

### Required R Packages
```r
shiny
ggplot2
plotly
dplyr
scales
gganimate
readr
jsonlite
reshape2
rintrojs
shinyjs
```
