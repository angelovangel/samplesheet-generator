# Illumina sample sheet generator

## A [Shiny](https://shiny.rstudio.com/) app for generating sample sheets for use in Illumina sequencing.

This app is a replacement/supplement to the Illumina Experiment Manager. Its main purpose is to generate sample sheets for use with `bcl2fastq`. Several indexing kits are already included (see "supported kits" button), for new kits please [open an issue](https://github.com/angelovangel/samplesheet-generator/issues/new?labels=new_kit&title=New+index+kit+request). Note that only indexing kits with pre-defined index well positions are supported.

## Usage

The app is deployed on Digital Ocean [HERE](http://165.22.73.243/samplesheet/), and is always up-to-date thanks to [GitHub Actions!](https://github.com/features/actions) For local usage just clone the repository and start `app.R` in RStudio.

## More technical info

### App design and implementation

The app first reads the user input (data pasted in the input text field) and then simply joins (inner join) the user data with the index data by well position and index set. The index data for all supported kits is present in `indexdata/indexcsv.csv` and can easily be extended with other kits. Fast reading of the index data is achieved with `fread()` from `data.table`. After that, various other inputs are collected to construct the final sample sheet according to the Illumina [specifications](https://emea.illumina.com/content/dam/illumina-marketing/documents/products/technotes/sequencing-sheet-format-specifications-technical-note-970-2017-004.pdf).

### Workflows and machines

- The **forward strand** workflow is performed on the NovaSeq 6000, MiSeq, HiSeq 2000/2500, NextSeq 2000.
- The **reverse complement** workflow is performed on the iSeq, MiniSeq, NextSeq 500, HiSeq 3000/4000/X.

[Link to source](https://emea.support.illumina.com/content/dam/illumina-support/documents/documentation/system_documentation/miseq/indexed-sequencing-overview-guide-15057455-06.pdf)

*In this repository, and in the indexcsv.csv file, **miseq** means forward strand workflow and **nextseq** means reverse strand workflow.*

### Adapter trimming sequences
[Link to source](https://emea.support.illumina.com/bulletins/2016/12/what-sequences-do-i-use-for-adapter-trimming.html)

- TruSeq single index- and TruSeq CD index-based kits:

```bash
Adapter, AGATCGGAAGAGCACACGTCTGAACTCCAGTCA
AdapterRead2, AGATCGGAAGAGCGTCGTGTAGGGAAAGAGTGT
```

***


- AmpliSeq for Illumina; 
- Illumina DNA Prep (M) Tagmentation (previously Nextera DNA Flex); 
- Illumina DNA Prep with Enrichment (S) Tagmentation (previously Nextera Flex for Enrichment);
- Nextera DNA; 
- Nextera XT; 
- Nextera Enrichment; 
- Nextera Rapid Capture Enrichment; 
- TruSight Enrichment; 
- TruSight Rapid Capture Enrichment; 
- TruSight HLA; 
- Illumina Stranded mRNA Prep, Ligation; 
- Illumina Stranded Total RNA Prep, Ligation with Ribo-Zero Plus; 
- Illumina RNA Prep with Enrichment, Ligation

```bash
Adapter, CTGTCTCTTATACACATCT
```

***

- Illumina DNA PCR-Free Prep, Tagmentation

```bash
Adapter, CTGTCTCTTATACACATCT+ATGTGTATAAGAGACA
```



### Trimming T-overhang options
Trimming T-overhang options for the 

- Illumina Stranded mRNA and 
- Illumina Stranded Total RNA workflows

Add the following settings to the [Settings] section of the SampleSheet.csv file. 
These settings configure the FASTQ generation to start from the second cycle, skipping the T overhang.

```bash
[Settings]
Read1StartFromCycle,2
Read2StartFromCycle,2
```

[Link to source](https://emea.support.illumina.com/bulletins/2020/06/trimming-t-overhang-options-for-the-illumina-rna-library-prep-wo.html?langsel=/de/)

