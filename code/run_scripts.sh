#!/bin/bash

#SBATCH --job-name=run_r_scripts


#SBATCH -o run_scripts.out
#SBATCH --partition=hlwill
#SBATCH --time=08:00:00
#SBATCH --mem=100GB
#SBATCH --ntasks=8

#SBATCH --mail-type=ALL
#SBATCH --mail-user=flack@stanford.edu

ml load system
ml load R/4.1.2

# Rscript id_doctor_sample.R

# Rscript create_matched_samples.R

# Rscript match_doctor_sample.R

cd 01_sample/

Rscript candidate_occs.R 

Rscript select_occs.R

Rscript id_occ_samples.R

cd ../02_features
Rscript make_occ_outcomes.R

cd ../03_model
Rscript occ_balance_tests.R
Rscript model_occ_outcomes.R
