# holobiont_stress_newPhyt

Repository for submission in New Phytol. of our paper on the plant holobiont response to stress in the bromeliad *Achmea mertensii*.

# Data origin

The data used in this paper are a part of a greater experimental dataset with samples sequenced over different Illumina runs. To ensure consistence in the OTUs identification across samples, they were processed within a unique bioinformatic pipeline hosted [here](https://github.com/LafontRapnouilTristan/OBIholo_pipeline). Thus, to produce the inputs for `holobiont_under_stress.R`, we need to run several script taking the pipeline outputs and applying sample filtering, cleaning and formating to get to an analysis-ready dataset. Codes and data to go from the raw [pipeline](https://github.com/LafontRapnouilTristan/OBIholo_pipeline) outputs to the analysis ready files hosted correspond to the `clean_metabar_data.zip` archive downloadable from this zenodo [repository](https://zenodo.org/records/24318812).

# Reproduce the figures

# Files

- `holobiont_under_stress.R`: The main script, run this to reproduce all the paper's analyses, figures and tables. Note that some figures were further edited/assembled with [inkscape](https://inkscape.org/).


# Folders
- `/data`: Where to put raw and intermediate data used in for analysis. (can be downloaded from zenodo: [DOI 10.5281/zenodo.24318812](https://zenodo.org/records/24318812), `data.zip`)
    
    The used datasets are:
    - `Chimie_sols.csv`: Soil chemistry data (yes, we're frenchies!)
    - `cleaned_microtable_ff.RData` & `cleaned_microtable_nraref_ff.RData`: not used but outputs of `03_clean_micro_datasets.Rmd` that were kept for diagnoses of filtering strategies.
    - `data_art1.csv`: Phenotypic traits 
    - `micro_data_xx`: microtables for 4 data subsets. Leaves its and 16s (*e.g.*, `micro_data_16s.rds`) communities and soils (*e.g.*, `micro_data_16s_soil.rds`) 

- `/results`: Will be created as you run `holobiont_under_stress.R`. The submitted results folder can be downloaded from [zenodo](https://zenodo.org/records/24318812), `results.zip`.

    The archive contains the following folders:
    - `/figures`: folder with 3 directories, `/main` and `/supp` for work in progress outputs and **`/draft`** for the outputs showed in the paper.
    - `/stats`: some statistical outputs
    - **`/for_submission`: Submission ready `/tables` and `/figures`**

- `/custom_fct`: Two custom R functions for positioning of compact letter display on boxplot and in tables (these are old and suboptimal but it works)

An image of the R environment at the end of the analyses (*e.g.*, after running `holobiont_under_stress.R`) can be downloaded as `.RData` from the zenodo archive: [`env_post_analysis.RData`](https://zenodo.org/records/24318812).
