# holobiont_stress_newPhyt

Repository for submission in New Phytol. of our paper on the plant holobiont response to stress in the bromeliad *Achmea mertensii*.

# Structure of the repository

# Files

- `holobiont_under_stress.R`: The main script, run this to reproduce all the paper's analyses, figures and tables. Note that some figures were further edited/assembled with [inkscape](https://inkscape.org/).
- `env_post_analysis.RData`: Contains the image of the R environment of `holobiont_under_stress.R`. 

# Folders
- `/clean_metabar_data`: Contains bio-informatic pipeline outputs and scripts to clean them for further analyses.
- `/data`: Contains raw and intermediate data used in for analysis.
- `/results`: Contains the outputs produced by the `holobiont_under_stress.R` script as well as submission-ready and [inkscape](https://inkscape.org/)-edited figures. 
- `/taxdb`: Databases used for OTUs taxonomic assignation as they were updated during the process. 
- `/custom_fct`: Two custom R functions for positioning of compact letter display on boxplot and in tables (these are old and suboptimal but it works)

## clean_metabar_data

The data used in this paper are a part of a greater experimental dataset with samples sequenced over different Illumina runs. To ensure consistence in the OTUs identification across samples, they were processed within a unique bioinformatic pipeline hoste [here](https://github.com/LafontRapnouilTristan/OBIholo_pipeline). Thus, to produce the inputs for `holobiont_under_stress.R`, we need to run several script taking the pipeline outputs and applying sample filtering, cleaning and formating to get to an analysis-ready dataset. This is the aim of the `/clean_metabar_data` folder.

### Files

- `01_create_mlist_16s_ff.Rmd` and `01_create_mlist_its_ff.Rmd` create metabarlist (package [metabaR](https://github.com/metabaRfactory/metabaR)) for both bacteria (16s) and fungi (its).
- `02_clean_mlists_ff.Rmd` diagnoses and cleans the metabarlists. Outputs cleaned metabarlist and [phyloseq](https://joey711.github.io/phyloseq/) objects.
- `03_clean_micro_datasets.Rmd` applies final sample selection and filtering (*e.g.* rarefaction) to create the microtables ([microeco](https://chiliubio.github.io/microeco_tutorial/) object) that will be used as inputs for `holobiont_under_stress.R`

### Folders

- `/inputs`: Outputs of the combined [pipeline](https://github.com/LafontRapnouilTristan/OBIholo_pipeline)
- `/tmp`: Outputs of `01_create_mlist_16s_ff.Rmd` and `01_create_mlist_its_ff.Rmd` (inputs for `02_clean_mlists_ff.Rmd`)
- `/outputs`: Outputs of `02_clean_mlists_ff.Rmd` (`raw_mlist_and_physeq_ff.RData` for pre-filtering objects, `clean_mlist_and_physeq_ff.RData` for filtered objects) 
- `/metabaR_figz`: Figures of `02_clean_mlists_ff.Rmd`
- `/cleaning_figz` Figures of `03_clean_micro_datasets.Rmd`

## data

Data used in the main analysis script `holobiont_under_stress.R`.

### Files

- `Chimie_sols.csv`: Soil chemistry data (yes, we're frenchies!)
- `cleaned_microtable_ff.RData` & `cleaned_microtable_nraref_ff.RData`: not used but outputs of `03_clean_micro_datasets.Rmd` that were kept for diagnoses of filtering strategies.
- `data_art1.csv`: Phenotypic traits 
- `micro_data_xx`: microtables for 4 data subsets. Leaves its and 16s (*e.g.*, `micro_data_16s.rds`) communities and soils (*e.g.*, `micro_data_16s_soil.rds`) 

## results

### Folders

- `/figures`: folder with 3 directories, `/main` and `/supp` for work in progress outputs and **`/draft`** for the outputs showed in the paper.
- `/stats`: some statistical outputs
- **`/for_submission`: Submission ready `/tables` and `/figures`**