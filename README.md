# Interactive Microarray Data Viewer

This tool provides quick and flexible analysis of microarray data from **NCBI GEO**. It’s inspired by **GEO2R**, but with additional features for deeper exploration:

- **Probe-to-gene collapsing** is supported before differential analysis, giving gene-level insights.
- **PCA and hierarchical clustering** plots allow coloring by metadata to detect batch effects or sample grouping.
- **Volcano plots** are interactive: you can highlight specific genes of interest directly on the plot.
- **Boxplots** for gene expression levels help visualize differences across two conditions for selected genes.

👉 **For full walkthrough visit here**: [🔗 Microarray Data Analysis Tool – Tutorial](https://ridwan257.github.io/card-viewer.html?id=microarray_analysis&title=Microarray%20Data%20Analysis%20Tool)

## ⚙️ Implementation Overview

The analysis code is written in R and accessed through a simple web-based user interface. Data is fetched from NCBI GEO, processed with standard bioinformatics pipelines, and visualized interactively.

- **Frontend**: HTML5, CSS3, JavaScript
- **Backend**: R plumber api
- **Libraries**: Key R packages:
    - GEOquery
    - limma
    - dplyr, matrixStats
    - plumber


## 📦 Prerequisites

To run the backend API and perform analyses, you need the following:

- **R version** ≥ 4.4.0  
- **Bioconductor** installed and working

If you don't have **Bioconductor** installed yet, run the following in your R console:

```r
> install.packages("BiocManager")
```

## 💡 How to Use

First clone the repository,
```
git clone https://github.com/ridwan257/geo3R.git
cd geo3R
```

Then launch the R api. It may takes some time while running first time, beacuse it will install all missing dependency to run R api.
```
bash run.sh
```

After successfuly launching api, just open the `index.html` file in any browser. You can create multiple independent tabs, each one maintains its own analysis state, making no data corruption.