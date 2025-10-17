# bluehead wrasse

#transcriptome was assembled de novo
#counts generated with RSEM
#
#RSEM ---- HTSEQ (STAR) ----- featureCounts
#
#----> tidy the count output data, until it looks like this:
#
# All of these output looks different, but, they will give otu some sort of file that has raw counts, for each gene per sample. 
#aim of this section? 
# how to take metadata and associate it to your count data for plotting in ggplot?
# how to select specific genes to investigate gene expression?
# how to take top 10,000 most variably expressed genes?
#
#
#This study did de novo trinity, for which you can get raw counts (equivalent to align to genom e counts uding feautre count or htseq/star). which shoudl be then used for differential expression
# here though, as we are going to be doing some plotting, we want to use some sort of normalised count.
# for further explanation, see our RNAseq workshop.
# suffice to say for now, raw counts are far too variable for plotting (eg the smallest count may be 1, teh largest single count thousands)
# a common transformation is to log values
# here we are going to use TMM normlaised counts 


#Count matrix that looks like this:
  
  
  # Load in data files for DGE analysis
  # >> Counts file
  ## If you've got a table like the below...
  ## +-------------+---------+---------+-----+
  ## |   gene_id   | sample1 | sample2 | ... |
  ## +-------------+---------+---------+-----+
  ## |    gene1    |   166   |    128  | ... |
  ## |    gene2    |   260   |    118  | ... |
  ## +-------------+---------+---------+-----+
  ## ... then you should set row.names to be "gene_id"
# counts.table = read.table(file=COUNTS_FILE, header = TRUE, sep = "\t", stringsAsFactors = FALSE, row.names = "gene_id")


# metadata set up

# you probably have all your metadata in excel, make it look like this:
#
# >> Coldata file
## If you've got a table like the below...
## +------------+--------+------+-----------+
## | SampleName | tissue | time | replicate |
## +------------+--------+------+-----------+
## | sample1    | L      |    1 |         1 |
## | sample2    | L      |    1 |         2 |
## | sample3    | L      |    2 |         1 |
## | sample4    | L      |    2 |         2 |
## | sample5    | B      |    1 |         1 |
## | sample6    | B      |    1 |         2 |
## | sample7    | B      |    3 |         1 |
## | sample8    | B      |    3 |         2 |
## +------------+--------+------+-----------+


# these two files are the raw form of data you will most likely be pulling data into R in. 
# except for doing some possible cleaning of rnaseq count data in R before making the counts.df.



# 1️⃣ Associate metadata to your count data for plotting in ggplot

library(tidyverse)


counts <- read_tsv("counts.matrix")
coldata <- read_tsv("coldata.txt")


# Convert counts to long format
counts_long <- counts %>%
  pivot_longer(
    cols = -gene_id,
    names_to = "sample",
    values_to = "count"
  )


# Join metadata
counts_long_metadata <- counts_long %>%
  left_join(coldata, by = "sample")

# factorise to set order
# this allows you to chose the order you want variables to be in 
unique(coldata$sex_stage)
sex_stage_order <- c("F", "S3", "S4", "TPmale")

counts_long_metadata <- counts_long_metadata |>
  mutate(sex_stage = factor(sex_stage, levels = sex_stage_order))
head(counts_long_metadata)

# Now you can plot, e.g.:
ggplot(counts_long_metadata , aes(x = sex_stage, y = log(count), fill = condition)) +
  geom_violin()
 # is this telling us anything interesting? not really, althoguh it appears on average genes are expressed higher in transitional and male samples. 



# 2️⃣ Select specific genes to investigate

#add contig names from ZF annotations into dataframe here using leftjoin probably

genes_of_interest <- c("GeneA", "GeneB")

counts_subset <- counts_long_meta %>%
  filter(gene %in% genes_of_interest)

# Can plot or summarize
ggplot(counts_subset, aes(x = condition, y = count, fill = condition)) +
  geom_boxplot() +
  facet_wrap(~gene)


# 3️⃣ Take top 10,000 most variably expressed genes


top_genes <- counts_df %>%
  rownames_to_column("gene") %>%
  mutate(variance = apply(select(., -gene), 1, var)) %>%
  arrange(desc(variance)) %>%
  slice_head(n = 10000) %>%
  pull(gene)

# Subset long table for plotting
counts_top10k <- counts_long_meta %>%
  filter(gene %in% top_genes)



