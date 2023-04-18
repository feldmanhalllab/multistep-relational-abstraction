# Multistep relational abstraction

This repository contains all the data and code needed to fully reproduce the analyses. References to the "butterfly network" refer to the artificial social network used in some of the studies, so named because it resembles a butterfly with its wings extended.

Written by [Jae-Young Son](https://jaeyoungson.com/).


## Computing environment

This code was written/run under R 4.2.1, and relies on the following libraries:
- `tidyverse 1.3.2`
- `here 1.0.1`


## Workflow 1: Clean data

This workflow can generally be skipped (cleaned data is already provided), unless you're checking the reproducibility of the code.

```bash
cd ~/Documents/GitHub/multistep-relational-abstraction/code/01-clean-butterfly-data
source ./clean-butterfly-data.sh
```


## Workflow 2: Simulate from butterfly network

In an artificial social network designed to disentangle different schema-like representations of network structure, what do different strategies predict?

```bash
cd ~/Documents/GitHub/multistep-relational-abstraction/code/02-simulate-butterfly
source ./simulate-butterfly.sh
```


## Workflow 3: Statistical models of butterfly network

If our goal is to make group-level inferences about repesentation, we can use mixed-effects regression to capitalize on whatever representational structure might be common to all participants.

```bash
cd ~/Documents/GitHub/multistep-relational-abstraction/code/03-regress-butterfly
source ./regress-butterfly.sh
```


## Workflow 4: Fit computational models of butterfly network

Fitting a computational model can help us understand two additional questions:
1. How does learning format impact representational format?
2. How do individual differences in representation affect choice?

This particular analysis was run on Oscar (Brown's SLURM-managed computing cluster), but could in principle be run on other SLURM-managed computing clusters (or locally) with fairly minimal modification.

To run this workflow, upload the following to Oscar:
1. `/multistep-relational-abstraction.Rproj`
2. `/data/butterfly-behavior/*`
3. `/data/butterfly-fixed-predictions/*`
4. `/code/04-fit-butterfly/*`
5. `/code/utils/*`

```bash
cd /gpfs/home/${USER}/data/${USER}/multistep-relational-abstraction/code/04-fit-butterfly/

# Only need to do this once
module load R/4.2.0
module load gcc/10.2 pcre2/10.35 intel/2020.2 texlive/2018 pandoc
Rscript install-dependencies.R

# Run models in parallel
sbatch memory-walk-memorization.sh
sbatch memory-walk-sr.sh
sbatch memory-walk-memorization-sr.sh

sbatch memory-pair-memorization.sh
sbatch memory-pair-sr.sh
sbatch memory-pair-memorization-sr.sh

sbatch trust-pair-memorization.sh
sbatch trust-pair-sr.sh
sbatch trust-pair-memorization-sr.sh

# Wait for everything to finish...
mkdir -m 775 slurm_out
mv ./*.out ./slurm_out
```

