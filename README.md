> **WORK IN PROGRESS**
> This repository and the accompanying thesis are currently under active development. Content may be updated frequently.

# gipsDA - Engineering Thesis Repository

This repository contains all the supplementary materials for the engineering thesis titled "Adapting the gips library for classification problem utilizing discriminant analysis – gipsDA".

This repository is a companion to the main `gipsDA` R package. It includes the LaTeX source for the thesis, data generation scripts, and manual tests used to validate the models and replicate the study's findings.

## Related Repository

The source code for the R package itself can be found in the main repository:
- **gipsDA R Package:** [https://github.com/AntoniKingston/gipsDA](https://github.com/AntoniKingston/gipsDA)

## Repository Structure

This repository houses all materials related to the engineering thesis, including data, experiments, and the written report. It is structured as follows:

-   #### `thesis/`
    This directory contains the complete LaTeX source code for the written thesis document, including all figures, tables, and the bibliography file.

-   #### `manual_tests/`
    This directory contains R scripts used for higher-level validation, exploratory analysis, and the replication of the simulation studies described in the thesis. These are distinct from the automated unit tests found in the package repository.

-   #### `data/`
    This directory does not store the raw dataset files directly, especially the large ones, to keep the repository lightweight. Instead, it is organized as follows:
    -   A `README.md` file providing detailed instructions and direct links for downloading the real-world datasets from their original sources.
    -   A `generate/` subdirectory containing the R scripts used to generate the five synthetic data scenarios for the simulation study.