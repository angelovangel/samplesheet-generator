# Workflows and machines

>The forward strand workflow is performed on the NovaSeq 6000, MiSeq, HiSeq 2000/2500, NextSeq 2000.
>The reverse complement workflow is performed on the iSeq, MiniSeq, NextSeq 500, HiSeq 3000/4000/X.

[link to source](https://emea.support.illumina.com/content/dam/illumina-support/documents/documentation/system_documentation/miseq/indexed-sequencing-overview-guide-15057455-06.pdf)

*In this repository, and in the indexcsv.csv file, "miseq" means forward strand workflow and "nextseq" means reverse strand workflow.*

# Trimming T-overhang options
Trimming T-overhang options for the Illumina Stranded mRNA and Illumina Stranded Total RNA workflows
>Users can add the following settings to the [Settings] section of the SampleSheet.csv file. These settings configure the FASTQ generation to start from the second cycle, skipping the T overhang.

[Settings]
Read1StartFromCycle,2
Read2StartFromCycle,2

https://emea.support.illumina.com/bulletins/2020/06/trimming-t-overhang-options-for-the-illumina-rna-library-prep-wo.html?langsel=/de/