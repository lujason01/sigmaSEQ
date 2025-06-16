
#* title: "PEC GSE183325 RNASeq Analysis SCRIPT"
#* author: "Jason Lubega"
#* date: "06.Mar.2025"


##* Background
##* Myelodysplastic syndromes (MDS) are clonal hematopoietic stem cell (HSC) malignancies characterized by ineffective hematopoiesis with increased incidence in elderly individuals. Genetic alterations do not fully explain the molecular pathogenesis of the disease, indicating that other types of lesions may play a role in its development. In this work, we will analyze the transcriptional lesions of human HSCs to explore how aging and MDS are characterized. 



## Plan  
# Load Data   
# EDA - Data Exploration   
# DEA - Differential Expression (Used DeSeq2)  
# GO - Pathway Analysis  



#### Set working directory 

setwd("C:/Users/Lenovo/MIB Assignments/NOTES/Bioinformatica/Bioinformatics Practice R/PEC GSE183325 NereaB")


#### Load libraries 

#Getting GEOData 

library(Biobase)
library(GEOquery)

#EDA, DEA packages
library(edgeR)
library(limma)
library(DESeq2)

#Data visualisation
library(ggplot2)
library(ggrepel)
suppressPackageStartupMessages(library(ComplexHeatmap))

#Data manipulation
library(tidyverse) 
library(dplyr)
library(reshape2)

##****** Human gene annotation package
#if (!require("BiocManager", quietly = TRUE))
#install.packages("BiocManager")
#BiocManager::install("org.Hs.eg.db")

library(org.Hs.eg.db)

#BiocManager::install("biomaRt")
library(biomaRt)

#BiocManager::install("AnnotationDbi")
library(AnnotationDbi)

#Pathway Analysis
library(clusterProfiler)
library(enrichplot)


##**** LOAD DATA 

# Loading metadata files from GEO and Raw Counts (locally)


# counts data
counts_rawdata <- read.csv('GSE183325_RawCounts_matrix_Young_Elderly_MDS.csv' , sep = "", header = TRUE)

#View(counts_rawdata)

# ATTEMPT 2 on sample Metadata
library(GEOquery)

options(download.file.method = "libcurl")


GSE183325_allmetadata <- getGEO("GSE183325", GSEMatrix = TRUE)
#this is a list that contains all sections
#I am interested in the "phenoData" and the "featureData"

#Expression DataSet extraction 

url("http://www.google.com")


# Store the GEO dataset as "GSE60450_gse"
GSE183325_gse <- GSE183325_allmetadata[[1]]

# Get the names of samples
sample_id <- sampleNames(GSE183325_gse)
#View(sample_id)

# Get the expression matrix
#View(exprs(GSE183325_gse)) #empty

#samples metadata
sample_metadata <- pData(GSE183325_gse)
#View(sample_metadata)

#Changing column headings to sample Names 
#colnames(counts_rawdata) == rownames(sample_metadata) #ALL TRUE
colnames(counts_rawdata) <- rownames(sample_metadata) #success



##**Obtaining gene metadata - GENE MAPPING**

#We do nOt have a gene metadata file. Therefore we created one using the ENSEMBL Gene IDs in the counts data. This requires us to first convert ENSEMBL Gene IDs to ENTREZ Gene IDs   

#**org.Hs.eg.db vs biomaRt**  

#In order to obtain the gene metadata, I performed gene mapping using both *biomaRt* and *org.Hs.eg.db*.


#**org.Hs.eg.db**

#GETTING gene_metadata #***********

#* Using org.Hs.eg.db
#columns(org.Hs.eg.db)

ensembl_geneIDs <- rownames(counts_rawdata)
#View(ensembl_geneIDs)

entrez_geneIDS <- mapIds(org.Hs.eg.db, keys = ensembl_geneIDs, column = "ENTREZID", keytype = "ENSEMBL", multiVals = "first")

entrez_geneIDS_df <- data.frame(entrez_geneIDS)
colnames(entrez_geneIDS_df) <- c("ENTREZID")

#View(entrez_geneIDs_df)

gene_metadata_annotated <- AnnotationDbi::select(org.Hs.eg.db,keys=entrez_geneIDS_df$ENTREZID,columns=c("ENTREZID","SYMBOL","GENENAME"))


#entrez_geneIDS_df$ENTREZID == gene_metadata_annotated$ENTREZID #TRUE

rownames(gene_metadata_annotated) <- rownames(entrez_geneIDS_df) #success

#renamed rownames of the gene_metadata_annotated file to contain ENSEMBL gene IDs as some genes lack the Entrez ID.

head(gene_metadata_annotated)


#Alot of N/A s Unannotated genes therefore we will try again with biomaRt.



## **biomaRt**

#********BIOMART gene mapping

#Therefore we will turn to using biomaRt instead
#As it contains the updated ensembl data.

ensembl <- useEnsembl(biomart = "genes", dataset = "hsapiens_gene_ensembl")

# Query Ensembl for gene symbols
# We will use the "ensembl_geneIDs" object created in the previous chunk.

gene_mapping <- getBM(
  attributes = c("ensembl_gene_id","entrezgene_id", "hgnc_symbol","description"),
  filters = "ensembl_gene_id",
  values = ensembl_geneIDs,
  mart = ensembl
)

#Are the rownames in the same order as our gene metadata file???
#gene_mapping$ensembl_gene_id==rownames(gene_metadata_annotated) #FALSE

# View results
head(gene_mapping) 



gene_mapping_backup <- gene_mapping #unmodified gene mapping data
#from now non gene_mapping will be modified to suit my data

gene_mapping_annotated <- gene_mapping[match(rownames(gene_metadata_annotated), gene_mapping$ensembl_gene_id), ]


library(dplyr)

gene_mapping <- gene_mapping %>%
  group_by(ensembl_gene_id) %>%
  summarise(hgnc_symbol = paste(unique(hgnc_symbol), collapse = "; "))  # Merge duplicate symbols

rownames(gene_mapping) <- gene_mapping$ensembl_gene_id

#Since gene_mapping lacks the other important rows we will add them from gene_mapping_annotated

gene_mapping_combined <- full_join(gene_mapping, gene_mapping_annotated, 
                                   by = "ensembl_gene_id")
head(gene_mapping_combined)

#**** gene_mapping_annotated has all IDs but some are duplicated 
#* gene_mapping has unique ensembl Ids (no duplications)
#* 
#* 

gene_mapping_metadata_COMPLETE <- merge.data.frame(gene_metadata_annotated, gene_mapping_annotated, by.x=0, by.y="ensembl_gene_id" )

#** gene_mapping_metadata_COMPLETE has is the BEST gene metadata set as it contains all fields from both "org.Hs.eg.db" and "BiomaRt"




##*****Exploratory Data Analysis (EDA)**  

### Raw Counts vs Normalised Counts   


#**Data filtering**  

# I will use filterByExpr() function with a default parameters.
# The default parameters exclude a lot of genes as seen below.

#myge_filter  FALSE=42022 AND   TRUE =16713  


colSums(counts_rawdata)

group_age <- sample_metadata$`condition:ch1`

table(group_age)

#creating filtering criteria
library(edgeR)

myge_filter <- filterByExpr(counts_rawdata, group = group_age, min.count = 10, min.total.count = 15)

#table(myge_filter)

## Using filter to keep only TRUE values
counts_raw_filtered <- counts_rawdata[myge_filter,]
#dim(counts_raw_filtered) #success [1] 16713  by  37



#**Normalized Counts**  

#Using TMM normalisation, normLibSizes()  What method does DeSeq2 analysis need? 

#tmm normalization - requires edgeR library

dge <- DGEList(counts = counts_raw_filtered, group= group_age)


norm_counts <-  normLibSizes(dge, method = "TMM") 
counts_normalized <- cpm(norm_counts, normalized.lib.sizes = TRUE)

counts_norm_log2 <- log2(counts_normalized+1)





#**Depth Chart (Raw vs Normalised)**  

#Non normalised filtered counts
counts_rawfiltrd_plot <- data.frame(sample_id=colnames(counts_raw_filtered),depth=colSums(counts_raw_filtered))


#Normalised filtered counts
counts_norm_to_plot <- data.frame(sample_id=colnames(counts_norm_log2),norm_depth=colSums(counts_norm_log2))

library(ggplot2)

#PLOTS

#Non-normalised Data  PLOT  
p_raw <-    ggplot(counts_rawfiltrd_plot,aes(x=sample_id, y=depth, fill=sample_id)) +
  ggtitle("Sequence depth of Raw Filtered Data")+ 
  geom_col() + 
  scale_y_log10() + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle=90))


#Normalised Data PLOT

p_norm <-      ggplot(counts_norm_to_plot,aes(x=sample_id, y=norm_depth, fill=sample_id)) +
  ggtitle("Sequence depth of Filtered and Normalised Data")+  
  geom_col() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle=90))+
  ylab("norm_depth (log2)")




par(mfrow = c(1, 2))

p_raw
p_norm

par(mfrow = c(1, 1))


#################--------------

#**Distribution Chart (Raw vs Normalised)** 


## Counts (RAW) distribution 

counts_boxplot_data <- stack(counts_raw_filtered)

#View(counts_boxplot_data)

bp_raw <- ggplot(counts_boxplot_data,aes(x=ind, y=values)) +
  ggtitle("Counts Distribution of Filtered Raw Counts")+
  geom_boxplot(color="blue") +
  scale_y_log10() +
  theme_classic() +
  theme(axis.text.x = element_text(angle=90), axis.line = element_line(color = "#008140")) 



##******************
##* Counts (NORMALIZED) distribution 

counts_norm_boxplot_data <- data.frame(counts_normalized)
counts_norm_boxplot_data <- stack(counts_norm_boxplot_data)

#View(counts_norm_boxplot_data)



#change colnames 
colnames(counts_norm_boxplot_data) <- c("values", "ind")

bp_norm <- ggplot(counts_norm_boxplot_data,aes(x=ind, y=values)) +
  ggtitle("Counts Distribution of Normalized Counts")+
  geom_boxplot(color="blue") +
  scale_y_log10()+
  theme_classic() +
  theme(axis.text.x = element_text(angle=90), axis.line = element_line(color = "#008140")) 


par(mfrow = c(2, 1))

bp_raw
bp_norm

par(mfrow = c(1, 1))


#################--------------

#*****Principal Component Analysis - PCA Plot**  

# Check variance in the dataset. Anomalies or Outliers. 

# I used the normalized Counts data

## Principal component Analysis

pca_output <- prcomp(t(counts_norm_log2))
# to compute for samples we need to provide it by rows
# so we transpose our matrix.

pca_df <- data.frame(PC1=pca_output$x[,1], 
                     PC2=pca_output$x[,2], 
                     Group=group_age)

ggplot(pca_df,aes(x=PC1, y=PC2, color=Group)) +
  geom_point(size=4) +
  theme_classic() +
  xlab("PC1 (13.93%)") +
  ylab("PC2 (11.23%)")+
  geom_text_repel(aes(label=rownames(pca_df), size=1.5))



#################--------------


##***Differential Gene Expression (DEA)**  


# What parameter are we contrasting by?     

#* Young vs Elderly, Healthy vs Untreated MDS*  

# Method: DESeq2 

######### DESeq2 #########
library(dplyr)

# Include the group variable as a factor

sample_metadata$group <- factor(dplyr::recode(sample_metadata$`condition:ch1`,
                                              "Young healthy donor"="Young_healthy",  
                                              "Elderly healthy donor"="Elderly_healthy", "Untreated MDS patient" = "MDS_Untreated"),
                                levels=c("Young_healthy","Elderly_healthy","MDS_Untreated"))

#Also include group2 of Healthy vs MDS sick.
sample_metadata$group2 <- factor(dplyr::recode(sample_metadata$`condition:ch1`,
                                               "Young healthy donor"="Healthy",  
                                               "Elderly healthy donor"="Healthy", "Untreated MDS patient" = "MDS_Untreated"),
                                 levels=c("Healthy","MDS_Untreated"))

# Create DESeqDataSet object with count data, sample metadata, and design matrix

library("DESeq2")  
dds <- DESeqDataSetFromMatrix(countData = counts_rawdata,
                              colData = sample_metadata,
                              design = ~ 0 + group)
#View(dds)

# Filter out low expressed genes

dds.filt <- dds[myge_filter,]

dim(dds.filt) #[1] 16713    37

# Differential expression analysis
dds.filt.dea <- DESeq(dds.filt)


##########################

#### Results:DESEq2  

##**Significantly Differentially Expressed Genes**  

# log2FoldChange >0.58 & padj<=0.05

# Biological significance vs Statistical Significance.



res <- results(dds.filt.dea) # shows the contrast by default based on group factor order.

saveRDS(res, "DEA_DESeq2_GSE183325.rds")

#dim(res) #[1] 16713     6


#***SIGNIFICANTLY DEA Genes*** 

sig_Genes <- res[res$padj<=0.05 & res$log2FoldChange>0.58, ]
sig_Genes <- data.frame(sig_Genes)
#dim(sig_Genes) #[1] 1346    6
n_sig_Genes <- nrow(sig_Genes)  # number of significantly Diff expressed genes 

head(sig_Genes)

# > Therefore *1346* genes out of the filtered *16713* are statisticaly and biologically significant.



##############-------------------------------

### DEA visualization plots 


#**Volcano Plot** 

# Biological significance vs Statistical Significance

# I used the res object (consult if it was the appropriate choice)

ggplot(res, aes(x = log2FoldChange, y = -log10(padj))) + 
  geom_point(color = "black", alpha = 0.3) +
  
  #Overexpressed genes = RED
  
  geom_point(data=res[res$log2FoldChange > 0.58 & res$padj <= 0.05,], 
             aes(x = log2FoldChange, y = -log10(padj)), color="red", alpha = 0.7) +
  
  #Under expressed genes = BLUE
  geom_point(data=res[res$log2FoldChange < -0.58 & res$padj <= 0.05,], 
             aes(x = log2FoldChange, y = -log10(padj)), color="blue", alpha = 0.7) +
  
  #ref lines
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = c(0.58, -0.58), linetype = "dashed", color = "gray") +
  
  theme_minimal() +
  
  labs(title = "Volcano Plot - Differential Expression",
       x = "Log2 Fold Change", 
       y = "-Log10 adj-P-value")



# RED dots  = Differentially Overexpressed genes.  

# BLUE dots = Differentially Underexpressed genes.  



#### **Heatmap Top 50 genes (Labeled with Gene SYMBOLS)**  

# I tried to replace the ENSEMBL IDs with Gene Symbols, however, the matching was erroneous when **%in%** was used. I resolved this by  using **match()** instead of **%in%**.

# And I have randomly checked for rownames in my annotated object vs "SYMBOL" in the gene_metadata file to proof read.


# The DESeq output is not normalised.
# Apply vst() to stabilise variances 

vsd <- vst(dds.filt.dea, blind = TRUE)
dds.filt.dea_norm <- assay(vsd)

# Apply Z-transformation
Z_score <- t(scale(t(dds.filt.dea_norm), center = TRUE, scale = TRUE))
#View(Z_score)

#Order by Fc from Big to Small
sig_Genes_ordFch <- sig_Genes[order(sig_Genes$log2FoldChange, decreasing = TRUE),]

#Annotate SigGenes
sig_Genes_ordFch_metadata <- gene_metadata_annotated[match(rownames(sig_Genes_ordFch), rownames(gene_metadata_annotated)), c("SYMBOL", "GENENAME")]

sig_Genes_ordFch_annotated  <- cbind(sig_Genes_ordFch, sig_Genes_ordFch_metadata) #success
#head(sig_Genes_ordFch_annotated)

group_colors <- c(
  "Elderly healthy donor" = "#EE4D09",
  "Young healthy donor" = "#22BA1A",            
  "Untreated MDS patient" = "#AB2087")   

# Column annotation
column_ha <- HeatmapAnnotation(
  age_status = group_age,  #dev stage grouping
  col = list(age_status = group_colors)  #dev stage coloring
)

#Plot Heatmap (ComplexHeatmap)
DEGenes_heatmap <- Heatmap(Z_score[rownames(sig_Genes_ordFch)[1:50],], 
        row_names_gp = gpar(fontsize = 6),        
        top_annotation = column_ha, 
        row_names_side = "left", 
        row_labels = sig_Genes_ordFch_annotated$SYMBOL[1:50]
)

DEGenes_heatmap


#### **COMPARISONS and CONTRASTS**  

#**DDIT3 across age and health status**    
# symbol:  DDIT3  
# ensembl: ENSG00000175197  
# Description: DNA damage inducible transcript 3


grouped_samples <- sample_metadata[,c("geo_accession", "condition:ch1")]
View(grouped_samples)

young_ids <- grouped_samples$geo_accession[grouped_samples$`condition:ch1` == "Young healthy donor"]

elderly_ids <- grouped_samples$geo_accession[grouped_samples$`condition:ch1` == "Elderly healthy donor"]

mds_ids <- grouped_samples$geo_accession[grouped_samples$`condition:ch1` == "Untreated MDS patient"]

healthy_ids <- c(young_ids,elderly_ids) #healthy ids both Young and Elderly

##** grouped_samples add a column for Healthy, MDS Sick

grouped_samples$status <- c("Healthy", "Healthy", "Healthy","Healthy","Healthy","Healthy","Healthy","Healthy","Healthy","Healthy","Healthy","Healthy","Healthy","Healthy","Healthy","Healthy","Healthy","Healthy","Healthy","Healthy","Healthy","Healthy","Healthy","Healthy","Healthy", "MDS", "MDS","MDS","MDS","MDS","MDS","MDS","MDS","MDS","MDS","MDS","MDS")

###****DDIT3 plot

ddit3_boxplot_data <- as.data.frame(t(dds.filt.dea_norm["ENSG00000175197", c(young_ids, elderly_ids, mds_ids)]))

#View(ddit3_boxplot_data)

ddit3_boxplot_data <- cbind(t(ddit3_boxplot_data),  grouped_samples[,c("condition:ch1", "status")])


#change colnames 
colnames(ddit3_boxplot_data) <- c("norm_counts", "ind", "status")

#str(ddit3_boxplot_data) #oops even the values are characters!!

ddit3_boxplot_data <- as.data.frame(ddit3_boxplot_data)  # Convert to data frame
ddit3_boxplot_data$norm_counts <- as.numeric(ddit3_boxplot_data$norm_counts)  # Convert to numeric

#plot
ggplot(ddit3_boxplot_data, aes(x=ind, y=norm_counts, fill=group_age)) +
  ggtitle("DDIT3 expression: Young vs Elderly vs MDS")+
  geom_boxplot(size = 0.5) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=270), axis.line = element_line(color = "#008140"))+
  ylab("Gene Expression")





#**DDIT3 in Healthy vs MDS**  
# Here, the young and elderly donors were combined to form a healthy population to compare with the sick (Untreated MDS patients).


ggplot(ddit3_boxplot_data, aes(x=status, y=norm_counts, color=status)) +
  ggtitle("DDIT3 expression: Healthy donors vs MDS patients")+
  geom_boxplot(size = 1) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=360), axis.line = element_line(color = "#008140")) 




# > Untreated MDS patients express higher DDIT3 levels than both healthy young and elderly populations. Hence MDS could be characterised by a higher occcurence of DNA damage pathways. 


#### **SOMATIC MUTATIONS**   

# Broadly, the most common genes with recurrent somatic mutations in MDS include **ASXL1** (10–20%), EZH2 (5–10%), NRAS (5–10%), **RUNX1** (10–15%), **SF3B1** (20–30%), **SRSF2** (10–15%), STAG2 (5–10%), **TET2** (20–30%), **TP53** (10–12%), and U2AF1 (5–12%).   

# For this, I will only check those that have an occurrence greater than 10% (in Bold).  

library(gridExtra)


###****** COMMON SOMATIC MUTATIONS GENES plot 
###*
###* COMMMON GENES AS PER LLITERATURE

#list of genes
som_mut_ids <- gene_mapping_metadata_COMPLETE$Row.names[gene_mapping_metadata_COMPLETE$hgnc_symbol %in% c("ASXL1", "RUNX1", "SF3B1", "SRSF2", "TET2", "TP53")]

som_mut_data_to_plot <- as.data.frame(t(dds.filt.dea_norm[som_mut_ids, ]))

# Created named vector, id_to_symbol,  Ensembl IDs as names, Gene Symbols as values
id_to_symbol <- c(
  "ENSG00000171456" = "ASXL1",
  "ENSG00000159216" = "RUNX1",
  "ENSG00000115524" = "SF3B1",
  "ENSG00000161547" = "SRSF2",
  "ENSG00000168769" = "TET2",
  "ENSG00000141510" = "TP53"
)

# Replace column names using the named vector
colnames(som_mut_data_to_plot) <- id_to_symbol[colnames(som_mut_data_to_plot)]

# add sample groups column
som_mut_data_to_plot$group<- sample_metadata[,c("group")]

# PLOTS 
# Convert from wide to long format
som_mut_data_to_plot_long <- som_mut_data_to_plot %>%
  pivot_longer(cols = -group, names_to = "gene", values_to = "expression")

#View(som_mut_data_to_plot_long)

# Generate individual plots for each gene
plots <- lapply(unique(som_mut_data_to_plot_long$gene), function(g) {
  ggplot(som_mut_data_to_plot_long %>% filter(gene == g), aes(x = group, y = expression, fill = group)) +
    geom_boxplot() +
    ggtitle(g) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust =  1))
})

# Arrange plots in a grid
grid.arrange(grobs = plots, ncol = 3)





#### **MORE Comparisons and Contrasts**

#COMPARISONS and CONTRASTS
# To get the changes for each comparison we need to define a contrast

Young_vs_Elderly_DESeq2 <- results(dds.filt.dea, contrast=c("group", "Young_healthy", "Elderly_healthy"))

Healthy_Yg_vs_MDS_DESeq2 <- results(dds.filt.dea, contrast=c("group", "Young_healthy", "MDS_Untreated"))

Healthy_El_vs_MDS_DESeq2 <- results(dds.filt.dea, contrast=c("group", "Elderly_healthy", "MDS_Untreated"))

#Healthy_yg_El_vs_MDS <- results(dds.filt.dea, contrast=c("group2", "Healthy", "MDS_Untreated")) #This  comparison needs its own DEA model matrix. Its group is  **dds.filt.dea$group2**

# Save in a list including the gene info 
DEA_output <- list(Young_vs_Elderly=merge(data.frame(Young_vs_Elderly_DESeq2),gene_mapping_metadata_COMPLETE,by.x=0, by.y="Row.names"),
                   Healthy_Yg_vs_MDS=merge(data.frame(Healthy_Yg_vs_MDS_DESeq2),gene_mapping_metadata_COMPLETE,by.x=0, by.y="Row.names"),
                   Healthy_El_vs_MDS=merge(data.frame(Healthy_El_vs_MDS_DESeq2),gene_mapping_metadata_COMPLETE,by.x=0, by.y="Row.names"))


#colnames(gene_mapping_metadata_COMPLETE)
#View(Healthy_Yg_vs_MDS_DESeq2) 

#****** It MERGED SUCCESSFULLY...


# TO BE REVISED

#****** PLAN B

#1-yOUNG healthy vs ELDERLY healthy

result_yg_vs_El_DF <- data.frame(Young_vs_Elderly_DESeq2)

Young_vs_Elderly_DESeq2_metadata <- gene_metadata_annotated[rownames(gene_metadata_annotated) %in% rownames(result_yg_vs_El_DF), c("SYMBOL", "GENENAME")]

Young_vs_Elderly_DESeq2_annotated  <- cbind(result_yg_vs_El_DF, Young_vs_Elderly_DESeq2_metadata) #success

#head(Young_vs_Elderly_DESeq2_annotated[order(Young_vs_Elderly_DESeq2_annotated$log2FoldChange, decreasing = TRUE),]) #check


#2-yOUNG healthy vs MDS Untreated Patient 

result_yg_vs_MDS_DF <- data.frame(Healthy_Yg_vs_MDS_DESeq2)

Healthy_Yg_vs_MDS_DESeq2_metadata <- gene_metadata_annotated[rownames(gene_metadata_annotated) %in% rownames(result_yg_vs_MDS_DF), c("SYMBOL", "GENENAME")]

Healthy_Yg_vs_MDS_DESeq2_annotated  <- cbind(result_yg_vs_MDS_DF, Healthy_Yg_vs_MDS_DESeq2_metadata) #success


#3-ELDERLY healthy vs MDS Untreated Patient 

result_El_vs_MDS_DF <- data.frame(Healthy_El_vs_MDS_DESeq2)

Healthy_El_vs_MDS_DESeq2_metadata <- gene_metadata_annotated[rownames(gene_metadata_annotated) %in% rownames(result_El_vs_MDS_DF), c("SYMBOL", "GENENAME")]

Healthy_El_vs_MDS_DESeq2_annotated  <- cbind(result_El_vs_MDS_DF, Healthy_El_vs_MDS_DESeq2_metadata) #success





##**Visualizing contrasts**  

#  **Healthy: Young vs Elderly**  

#  *Top 5 Over and Underexpressed*


# Prepare a joint data frame with the data to plot

contrasts_to_plot <- data.frame(gene=rownames(Young_vs_Elderly_DESeq2_annotated),
                                log2FC_yg_vs_El=Young_vs_Elderly_DESeq2_annotated$log2FoldChange,
                                padj_yg_vs_El = Young_vs_Elderly_DESeq2_annotated$padj,
                                log2FC_yg_vs_MDS=Healthy_Yg_vs_MDS_DESeq2_annotated$log2FoldChange,
                                padj_yg_vs_MDS = Healthy_Yg_vs_MDS_DESeq2_annotated$padj,
                                log2FC_El_vs_MDS=Healthy_El_vs_MDS_DESeq2_annotated$log2FoldChange, 
                                padj_El_vs_MDS = Healthy_El_vs_MDS_DESeq2_annotated$padj)

#*****young vs elderly PLOT

#Order this dataframe according to fold change 

yg_vs_el_plot_ordFch <- contrasts_to_plot[,c("gene", "log2FC_yg_vs_El", "padj_yg_vs_El")]

#Order by Fc from Big to Small
yg_vs_el_plot_ordFch <- yg_vs_el_plot_ordFch[order(yg_vs_el_plot_ordFch$log2FC_yg_vs_El, decreasing = TRUE),]

#Putting these in the same plot (5 of each)
yg_el_five_ordFch <- rbind(yg_vs_el_plot_ordFch[1:5, ], tail(yg_vs_el_plot_ordFch, n=5) )

yg_el_five_ordFch <- yg_el_five_ordFch %>%
  mutate(expression = ifelse(log2FC_yg_vs_El > 0, "UP", "DOWN"))

yg_el_five_ordFch$hgnc_symbol <- gene_mapping_metadata_COMPLETE[match(yg_el_five_ordFch$gene, gene_mapping_metadata_COMPLETE$Row.names), c("hgnc_symbol") ]

ggplot(data = yg_el_five_ordFch , mapping=aes(x=hgnc_symbol, y=log2FC_yg_vs_El, fill = expression)) + 
  ggtitle("Young vs Elderly: Top 5 Overexpressed & Underexpressed genes")+
  geom_col()+
  scale_fill_manual(values = c("UP" = "red", "DOWN" = "blue")) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90,hjust=1)) 






#**Young Healthy vs MDS**  
#  Top 5 over expressed and top 5 Underexpressed.  

# Here, we would like to retrieve 10 overexpressed genes and 10 Underexpressed genes between the Healthy donors and the Sick Untreated MDS patients.   

# Therefore we would have to perform a contrast between the two populations.   


#++++ Young HEALTHY VS MDS (PLOT) ++++#

#Order this dataframe according to fold change 
#Order by Fc from Big to Small

yg_vs_MDS_plot_ordFch <- contrasts_to_plot[,c("gene", "log2FC_yg_vs_MDS")]

yg_vs_MDS_plot_ordFch <- yg_vs_MDS_plot_ordFch[order(yg_vs_MDS_plot_ordFch$log2FC_yg_vs_MDS, decreasing = TRUE),]

#Putting these in the same plot (5 of each)
yg_MDS_five_ordFch <- rbind(yg_vs_MDS_plot_ordFch[1:5, ], tail(yg_vs_MDS_plot_ordFch, n=5) )

yg_MDS_five_ordFch <- yg_MDS_five_ordFch %>%
  mutate(expression = ifelse(log2FC_yg_vs_MDS > 0, "UP", "DOWN"))

yg_MDS_five_ordFch$hgnc_symbol <- gene_mapping_metadata_COMPLETE[match(yg_MDS_five_ordFch$gene, gene_mapping_metadata_COMPLETE$Row.names), c("hgnc_symbol") ]

ggplot(data = yg_MDS_five_ordFch , mapping=aes(x=hgnc_symbol, y=log2FC_yg_vs_MDS, fill = expression)) +
  geom_col()+ 
  scale_fill_manual(values = c("UP" = "red", "DOWN" = "blue")) +
  coord_flip() +
  ggtitle("Young vs MDS: Top 5 Overexpressed vs Underexpressed genes")+
  theme(axis.text.x = element_text(angle = 90,hjust=1)) 



##**PATHWAY ANALYSIS** (GO: Gene Ontology)  

# Now that differentially expressed genes have been identified, we will analyse enriched pathways in our data groups. This will help us make biological meaning of the actual changes happening in our data.

# There are three known methods for performing the pathway analysis namely; ORA , GSEA , GSVA. We will start with the **Over Representation Analysis (ORA)** method.  



#### **ORA GO**  

# **Young Vs Elderly**  1. Up REGULATED  


#  Over Representation Analysis (ORA)
# --------------------------------------------------------------------------

# Define the set of genes to test: Young vs Elderly --- up-regulated genes:

sig_UP_regulated_genes <- DEA_output$Young_vs_Elderly[DEA_output$Young_vs_Elderly$padj<0.05 & DEA_output$Young_vs_Elderly$log2FoldChange>0.58,]

#dim(sig_UP_regulated_genes)


# Define the ranked genes list based on logFCh.

sig_UP_regulated_genes <-  sig_UP_regulated_genes[order(sig_UP_regulated_genes$log2FoldChange, decreasing = TRUE),]

head(sig_UP_regulated_genes)

# Keep the UP-regulated genes
up_regulated_genes <- sig_UP_regulated_genes$entrezgene_id


# Perform the ORA based on GO terms

# GO over-representation analysis
ora_go_up <- enrichGO(gene          = up_regulated_genes,
                      universe      = DEA_output$Young_vs_Elderly$entrezgene_id,
                      OrgDb         = org.Hs.eg.db,
                      ont           = "BP",
                      pAdjustMethod = "BH",
                      pvalueCutoff  = 0.01,
                      qvalueCutoff  = 0.05,
                      readable      = TRUE)
#ora_go_up
#View(ora_go_up@result)

# Results visualization

layout(matrix(c(1, 2, 3), nrow = 1, ncol = 3, byrow = TRUE))

# Barplot
barplot(ora_go_up, showCategory=10, title ="GO_UP: Young vs Elderly") 

# Network
cnetplot(ora_go_up, node_label="category", 
         cex.params = list(cex_label_category = 1.2))

cnetplot(ora_go_up, node_label="gene", 
         cex.params = list(cex_label_category = 1.2))





# 2. *Down REGULATED: Young vs Elderly* 


# We are going to assess the up-regulated genes:
sig_DOWN_regulated_genes <- DEA_output$Young_vs_Elderly[DEA_output$Young_vs_Elderly$padj<0.05 & DEA_output$Young_vs_Elderly$log2FoldChange< -0.58,]

#dim(sig_DOWN_regulated_genes)
head(sig_DOWN_regulated_genes)

# Define the ranked genes list based on logFCh.
sig_DOWN_regulated_genes <- sig_DOWN_regulated_genes[order(sig_DOWN_regulated_genes$log2FoldChange, decreasing = TRUE),]

head(sig_DOWN_regulated_genes)

# Keep the UP-regulated genes
dw_regulated_genes <- sig_DOWN_regulated_genes$entrezgene_id



# Perform the ORA based on GO terms

# GO over-representation analysis
ora_go_dw <- enrichGO(gene          = dw_regulated_genes,
                      universe      = DEA_output$Young_vs_Elderly$entrezgene_id,
                      OrgDb         = org.Hs.eg.db,
                      ont           = "BP",
                      pAdjustMethod = "BH",
                      pvalueCutoff  = 0.01,
                      qvalueCutoff  = 0.05,
                      readable      = TRUE)

#head(ora_go_dw@result)


# Results visualization

layout(matrix(c(1, 2, 3), nrow = 1, ncol = 3, byrow = TRUE))

# Barplot
barplot(ora_go_dw, showCategory=10, title ="GO_DOWN: Young vs Elderly") 

# Network
cnetplot(ora_go_dw, node_label="category", 
         cex.params = list(cex_label_category = 1.2))

cnetplot(ora_go_dw, node_label="gene", 
         cex.params = list(cex_label_category = 1.2))





#**Young Healthy Vs Untreated MDS **   

# 1. *ORA > Up REGULATED*  

#  Over Representation Analysis (ORA)
# --------------------------------------------------------------------------

# Define the set of genes to test: Young vs MDS _up-regulated genes:

sig_UP_regulated_ym_genes <- DEA_output$Healthy_Yg_vs_MDS[DEA_output$Healthy_Yg_vs_MDS$padj<0.05 & DEA_output$Healthy_Yg_vs_MDS$log2FoldChange>0.58,]

#dim(sig_UP_regulated_genes)

# Define the ranked genes list based on logFCh.
sig_UP_regulated_ym_genes <- sig_UP_regulated_ym_genes[order(sig_UP_regulated_ym_genes$log2FoldChange, decreasing = TRUE),]

head(sig_UP_regulated_ym_genes)

# Keep the UP-regulated genes
up_regulated_ym_genes <- sig_UP_regulated_ym_genes$entrezgene_id


# GO over-representation analysis
ora_go_up_ym <- enrichGO(gene          = up_regulated_ym_genes,
                         universe      = DEA_output$Healthy_Yg_vs_MDS$entrezgene_id,
                         OrgDb         = org.Hs.eg.db,
                         ont           = "BP",
                         pAdjustMethod = "BH",
                         pvalueCutoff  = 0.01,
                         qvalueCutoff  = 0.05,
                         readable      = TRUE)
#ora_go_up_ym
#View(ora_go_up_ym@result)

# Results visualization

layout(matrix(c(1, 2, 3), nrow = 1, ncol = 3, byrow = TRUE))

# Barplot
barplot(ora_go_up_ym, showCategory=10, title ="GO_UP: Young vs MDS") 

# Network
cnetplot(ora_go_up_ym, node_label="category", 
         cex.params = list(cex_label_category = 1.2))

cnetplot(ora_go_up_ym, node_label="gene", 
         cex.params = list(cex_label_category = 1.2))





##***** Elderly Healthy Vs Untreated MDS **   

# *ORA > Up REGULATED*  

#  Over Representation Analysis (ORA)
# --------------------------------------------------------------------------

# Define the set of genes to test: Elderly vs MDS up-regulated genes

sig_UP_regulated_elm_genes <- DEA_output$Healthy_El_vs_MDS[DEA_output$Healthy_El_vs_MDS$padj<0.05 & DEA_output$Healthy_El_vs_MDS$log2FoldChange>0.58,]

#dim(sig_UP_regulated_genes)

# Define the ranked genes list based on logFCh.
sig_UP_regulated_elm_genes <- sig_UP_regulated_elm_genes[order(sig_UP_regulated_elm_genes$log2FoldChange, decreasing = TRUE),]

head(sig_UP_regulated_elm_genes, n=5)

# Keep the UP-regulated genes
up_regulated_elm_genes <- sig_UP_regulated_elm_genes$entrezgene_id


# Perform the ORA based on GO terms

# GO over-representation analysis
ora_go_up_elm <- enrichGO(gene          = up_regulated_elm_genes,
                          universe      = DEA_output$Healthy_El_vs_MDS$entrezgene_id,
                          OrgDb         = org.Hs.eg.db,
                          ont           = "BP",
                          pAdjustMethod = "BH",
                          pvalueCutoff  = 0.01,
                          qvalueCutoff  = 0.05,
                          readable      = TRUE)
#ora_go_up_elm
#View(ora_go_up_elm@result)

# Results visualization

layout(matrix(c(1, 2, 3), nrow = 1, ncol = 3, byrow = TRUE))

# Barplot
barplot(ora_go_up_elm, showCategory=10, title ="GO_UP: Elderly vs MDS") 

# Network
cnetplot(ora_go_up_elm, node_label="category", 
         cex.params = list(cex_label_category = 1.2))

cnetplot(ora_go_up_elm, node_label="gene", 
         cex.params = list(cex_label_category = 1.2))




--------------------------------------------------------------------------
  
  ####  **GSEA - Gene Set Enrichment Analysis**  
  
--------------------------------------------------------------------------
  
  
  ## **GSEA: Young Vs Elderly**   
  
  
  # Young vs Elderly
  
  # Define the ranked genes list based on logFCh.
  
YgElgeneList <- DEA_output$Young_vs_Elderly$log2FoldChange

names(YgElgeneList) <- DEA_output$Young_vs_Elderly$entrezgene_id
YgElgeneList_ranked <- YgElgeneList[order(YgElgeneList, decreasing = TRUE)]


# Perform the GSEA based on GO terms

gsea_go <- gseGO(geneList     = YgElgeneList_ranked,
                 OrgDb        = org.Hs.eg.db,
                 ont          = "BP",
                 minGSSize    = 100,
                 maxGSSize    = 500,
                 pvalueCutoff = 0.05,
                 verbose      = FALSE)

#View(gsea_go@result)

# Results visualization

layout(matrix(c(1, 2, 3), nrow = 1, ncol = 3, byrow = TRUE))

# Dotplot
dotplot(gsea_go, 
        x = "GeneRatio",
        color = "NES",
        size = "p.adjust",
        showCategory=10) + ggtitle("GSEA: Young vs Elderly dotplot")

#Network Plot
cnetplot(gsea_go, node_label="category", 
         cex.params = list(cex_label_category = 1.2),
         categorySize="p.adjust", color.params = list(foldChange = YgElgeneList_ranked))

# Treeplot
gsea_go2 <- pairwise_termsim(gsea_go)

treeplot(gsea_go2, showCategory = 10)





#** GSEA: Young Vs Untreated MDS**  

# Young vs Untreated MDS

# Define the ranked genes list based on logFCh.

YgMDSgeneList <- DEA_output$Healthy_Yg_vs_MDS$log2FoldChange

names(YgMDSgeneList) <- DEA_output$Healthy_Yg_vs_MDS$entrezgene_id
YgMDSgeneList_ranked <- YgMDSgeneList[order(YgMDSgeneList, decreasing = TRUE)]


# Perform the GSEA based on GO terms

gsea_go_ymds <- gseGO(geneList     = YgMDSgeneList_ranked,
                      OrgDb        = org.Hs.eg.db,
                      ont          = "BP",
                      minGSSize    = 100,
                      maxGSSize    = 500,
                      pvalueCutoff = 0.05,
                      verbose      = FALSE)

#View(gsea_go_ymds@result)

# Results visualization

layout(matrix(c(1, 2, 3), nrow = 1, ncol = 3, byrow = TRUE))

# Dotplot
dotplot(gsea_go_ymds, 
        x = "GeneRatio",
        color = "NES",
        size = "p.adjust",
        showCategory=15) + ggtitle("GSEA: Young vs MDS dotplot")

# Network plot
cnetplot(gsea_go_ymds, node_label="category", 
         cex.params = list(cex_label_category = 1.2),
         categorySize="p.adjust", color.params = list(foldChange = YgMDSgeneList_ranked))

# Treeplot
gsea_go_ymds_pairwise <- pairwise_termsim(gsea_go_ymds)

treeplot(gsea_go_ymds_pairwise, showCategory = 10)





#***GSEA: Elderly vs MDS***  

# Elderly vs Untreated MDS

# Define the ranked genes list based on logFCh.

El_MDSgeneList <- DEA_output$Healthy_El_vs_MDS$log2FoldChange

names(El_MDSgeneList) <- DEA_output$Healthy_El_vs_MDS$entrezgene_id
El_MDSgeneList_ranked <- El_MDSgeneList[order(El_MDSgeneList, decreasing = TRUE)]

# Perform the GSEA based on GO terms
gsea_go_elmds <- gseGO(geneList     = El_MDSgeneList_ranked,
                       OrgDb        = org.Hs.eg.db,
                       ont          = "BP",
                       minGSSize    = 100,
                       maxGSSize    = 500,
                       pvalueCutoff = 0.05,
                       verbose      = FALSE)

#View(gsea_go_elmds@result)

# Results visualization
layout(matrix(c(1, 2, 3), nrow = 1, ncol = 3, byrow = TRUE))

# Dotplot
dotplot(gsea_go_elmds, 
        x = "GeneRatio",
        color = "NES",
        size = "p.adjust",
        showCategory=10) + ggtitle("GSEA: Elderly vs MDS dotplot")

# Network
cnetplot(gsea_go_elmds, node_label="category", 
         cex.params = list(cex_label_category = 1.2),
         categorySize="p.adjust", color.params = list(foldChange = El_MDSgeneList_ranked))

# Treeplot
gsea_go_elmds_pairwise <- pairwise_termsim(gsea_go_elmds)

treeplot(gsea_go_elmds_pairwise, showCategory = 10)




