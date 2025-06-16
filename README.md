# SigmaSEQ: RNA-seq Analysis Application

## Creator: Lubega Jason
## Lab: Jason Lubega's Lab

## Motivation
This application was born from a personal experience of spending 20 days analyzing RNA-Seq data for an assignment. While the learning experience was invaluable, the time investment inspired the creation of a tool that would streamline this process for future analyses. As they say, great tools often come from a touch of "productive laziness"!

## Timeline
- Start: 25.Marzo.2025
- v1 release: Mayo 2025
- Status: Work in Progress

## Overview
SigmaSEQ is a Shiny application for RNA-seq data analysis, developed by Jason Lubega's Lab. The application provides an intuitive interface for exploratory data analysis, differential expression analysis, and pathway enrichment analysis. The application integrates with OpenAI's GPT for intelligent interpretation of results.

## Training Dataset
- GSE183325 dataset (included with RMarkdown documentation)
- Future plans include direct integration with GEO Database
- Testing with multiple datasets to ensure broad compatibility

## Features

### 1. Data Upload and Validation
- Support for count matrix and metadata file uploads
- Automatic data validation and quality checks
- Interactive data preview with DataTables
- Summary statistics display

### 2. Exploratory Data Analysis (EDA)
- Quality control metrics visualization
- Sample clustering analysis
- Principal Component Analysis (PCA)
- Expression heatmap visualization
- Interactive plots with download capabilities
- Data normalization and transformation options

### 3. Differential Expression Analysis (DEA)
- Multiple analysis methods (DESeq2, EdgeR, Limma)
- Volcano plots for visualization
- MA plots for expression analysis
- Expression heatmaps for significant genes
- Interactive tables with filtering and sorting
- Downloadable plots and results

### 4. Pathway Analysis
- Gene Set Enrichment Analysis (GSEA)
- Over-representation Analysis (ORA)
- Pathway network visualization
- Enrichment plots
- Multiple pathway databases (KEGG, GO, Reactome)
- Interactive pathway-gene associations

### 5. AI Interpretation
- Automated analysis interpretation
- Customizable analysis focus
- Multiple analysis types
- Exportable insights

## Installation

1. Clone the repository:
```bash
git clone https://github.com/lujason01/sigmaSEQ.git
cd sigmaSEQ
```

2. Install required R packages:
```R
install.packages(c(
  "shiny",
  "shinyjs",
  "shinyWidgets",
  "bs4Dash",
  "plotly",
  "DT",
  "ggplot2",
  "dplyr",
  "tidyr",
  "stringr",
  "readr",
  "openxlsx"
))

# Install Bioconductor packages
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install(c(
  "DESeq2",
  "edgeR",
  "limma",
  "clusterProfiler",
  "enrichplot",
  "pathview",
  "ReactomePA",
  "DOSE",
  "org.Hs.eg.db"
))
```

3. Run the application:
```R
Rscript run_app.R
```

## Usage

### Data Upload
1. Prepare your data:
   - Count matrix (CSV/TSV/TXT format)
   - Metadata file (CSV/TSV/TXT format)
   - Ensure sample names match between files

2. Upload files:
   - Click "Browse" to select files
   - Wait for validation
   - Review data preview and summary statistics

### GEO Data Import
1. Enter GEO Accession Number:
   - Format: GSE followed by numbers (e.g., GSE183325)
   - The app will automatically fetch available platforms
   - Example datasets:
     - GSE183325: Example training dataset
     - GSE12345: Example dataset
     - GSE67890: Example dataset

2. Select Platform:
   - The platform dropdown will be automatically populated
   - Choose the appropriate platform for your analysis
   - Common platforms include:
     - GPL570: [HG-U133_Plus_2] Affymetrix Human Genome U133 Plus 2.0 Array
     - GPL96: [HG-U133A] Affymetrix Human Genome U133A Array
     - GPL6244: [HuGene-1_0-st] Affymetrix Human Gene 1.0 ST Array

3. Select Samples:
   - The samples dropdown will be automatically populated
   - Select one or more samples for analysis
   - You can use Ctrl/Cmd + Click to select multiple samples
   - The app will validate your selections before proceeding

4. Import Data:
   - Click "Import Data" to fetch and process the selected data
   - The app will:
     - Download expression data
     - Extract phenotype information
     - Validate data quality
     - Prepare for analysis

5. Review Data:
   - Check the data preview tables
   - Verify sample information
   - Review quality metrics
   - Proceed to analysis when ready

### Exploratory Data Analysis
1. Configure analysis settings:
   - Select normalization method
   - Choose transformation method
   - Set filtering parameters

2. Run analysis:
   - Click "Run Analysis"
   - Review quality control plots
   - Explore sample clustering
   - Analyze PCA results
   - Download plots as needed

### Differential Expression Analysis
1. Set analysis parameters:
   - Choose analysis method
   - Select contrast groups
   - Set thresholds (fold change, p-value)

2. Run analysis:
   - Click "Run Analysis"
   - Explore volcano plots
   - Review MA plots
   - Analyze expression heatmaps
   - Download results and plots

### Pathway Analysis
1. Configure pathway settings:
   - Select analysis method
   - Choose pathway database
   - Set significance thresholds

2. Run analysis:
   - Click "Run Analysis"
   - Explore enrichment plots
   - Analyze pathway networks
   - Review gene-pathway associations
   - Download results and visualizations

## File Structure

```
sigmaSEQ/
├── R/
│   ├── modules/
│   │   ├── upload_server.R
│   │   ├── upload_ui.R
│   │   ├── eda_server.R
│   │   ├── eda_ui.R
│   │   ├── dea_server.R
│   │   ├── dea_ui.R
│   │   ├── pathway_server.R
│   │   ├── pathway_ui.R
│   │   ├── ai_interpretation_server.R
│   │   └── ai_interpretation_ui.R
│   ├── utils/
│   │   ├── data_processing.R
│   │   ├── visualization.R
│   │   └── helpers.R
│   ├── config.R
│   └── global.R
├── www/
│   ├── css/
│   │   ├── style.css        # Main application styles
│   │   ├── eda.css          # EDA module specific styles
│   │   ├── dea.css          # DEA module specific styles
│   │   ├── pathway.css      # Pathway module specific styles
│   │   └── ai.css           # AI interpretation module styles
│   └── js/
│       ├── handlers.js      # Main application JavaScript handlers
│       ├── eda_handlers.js  # EDA module specific handlers
│       ├── dea_handlers.js  # DEA module specific handlers
│       ├── pathway_handlers.js # Pathway module specific handlers
│       └── ai_handlers.js   # AI interpretation module handlers
├── data/
│   └── example_data/
├── tests/
├── run_app.R
└── README.md
```

### Web Interface Components (www/)

The `www/` directory contains all static assets used by the web interface:

#### CSS Files
- `style.css`: Main application styles including:
  - Layout and grid system
  - Color schemes and themes
  - Typography using Google Fonts (Exo 2, Saira, Winky sans)
  - Responsive design elements
  - Common component styles

- Module-specific CSS files:
  - `eda.css`: Styles for exploratory data analysis visualizations
  - `dea.css`: Styles for differential expression analysis plots
  - `pathway.css`: Styles for pathway analysis visualizations
  - `ai.css`: Styles for AI interpretation interface

#### JavaScript Files
- `handlers.js`: Core application JavaScript including:
  - Event handlers for common interactions
  - Utility functions for data manipulation
  - Plot initialization and updates
  - Error handling and notifications

- Module-specific JavaScript files:
  - `eda_handlers.js`: Handlers for EDA module interactions
  - `dea_handlers.js`: Handlers for DEA module interactions
  - `pathway_handlers.js`: Handlers for pathway analysis
  - `ai_handlers.js`: Handlers for AI interpretation features

## Dependencies

### R Packages
- shiny
- shinyjs
- shinyWidgets
- bs4Dash
- plotly
- DT
- ggplot2
- dplyr
- tidyr
- stringr
- readr
- openxlsx

### Bioconductor Packages
- DESeq2
- edgeR
- limma
- clusterProfiler
- enrichplot
- pathview
- ReactomePA
- DOSE
- org.Hs.eg.db

## Styling
Using Google Fonts:
- Exo 2
- Saira
- Winky sans

## Contributing

1. Fork the repository
2. Create a feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Citation

If you use SigmaSEQ in your research, please cite:

```
@software{sigmaSEQ,
  title = {SigmaSEQ: RNA-seq Analysis Application},
  author = {Lubega Jason},
  year = {2025},
  url = {https://github.com/lujason01/sigmaSEQ}
}
```

## Support

For support, please:
1. Check the documentation
2. Search existing issues
3. Create a new issue if needed

## Acknowledgments

- Bioconductor team for RNA-seq analysis packages
- Shiny team for the web framework
- All contributors and users

## Note
This is a passion project intended to be a learning curve, developed outside work hours. We are open to suggestions to make it better and more robust. 