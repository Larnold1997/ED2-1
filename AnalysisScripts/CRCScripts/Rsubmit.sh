#!/bin/bash
#$ -M #YOUR EMAIL
#$ -m abe

module purge
module load R
R CMD BATCH  DailySoilWater.R

