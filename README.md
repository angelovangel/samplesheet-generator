# Illumina sample sheet generator

## A Shiny() app for generating sample sheets for use in Illumina sequencing.
This app is a replacement/supplement to the Illumina Experiment Manager. Its main purpose is to generate sample sheets for use with `bcl2fastq`. Several indexing kits are already included (see "supported kits" button), for new kits please [open an issue](https://github.com/angelovangel/samplesheet-generator/issues/new?labels=new_kit&title=New+index+kit+request).

## Usage
The app is deployed on Digital Ocean [here](http://165.22.73.243/samplesheet/). For local usage just clone the repository and start `app.R` in RStudio.