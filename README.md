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

