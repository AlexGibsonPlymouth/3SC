# 01_CleanCCGDataAndExtractAnalyticalDataset.R

# Preamble ----

# This script reads in the collated file of CCG invoices, cleans it up,
# then reads in what we know about the suppliers. It then combines the files 
# and extracts a series of files used by the 02_AnalysisForReport.R script,
# including the final 'analytical dataset' of valid invoices >= £25k

#----

### Preliminary ----
  library(tidyverse)

# Define working directory
# Assumes script in /Rscripts directory - all paths relative  
  DataInputDir="../04_Data/processed/"
  DataOutputDir="../04_Data/processed/"

#----

### Read-in 01_CollatedInvoices and hack to deal with hospices with same name ----
# (01_CollatedInvoices created by 00_CollateAndCombineCCGData.R)

# Import CCG Invoices 
  Invoices=readRDS(paste0(DataInputDir,"01_CollatedInvoices.RDS"))
  
#----

# Read in manually-created SuppliersInfo file----
# Based 01_UniqueSupplierNames.csv created by the 00_CollateAndCombineCCGData.R script,
# this file has been produced manually.  See github README and the accompanying 
# MS word report 'CCG_VCSE_Spend_Report.docx'.
  Suppliers = read.csv(paste0(DataInputDir,"01_SuppliersInfo_FINAL.csv"))
  nrow(Suppliers) # 13,459

#----

# Combine the Invoices and Suppliers Data and extract 'valid' invoices----
# Save interim datsets for use by the 02_AnalysisForReport.R script
  
  Combined = left_join(Invoices, Suppliers, by="Supplier")
  nrow(Combined) # 689,536
  
  saveRDS(Combined,paste0(DataOutputDir,"02_FullCollatedDataset.RDS"))

#----

# Extract 'analytical dataset' and tidy----
# Thus remove invalid (i.e. with Supplier==NA or tagged with Redacted==1 or
# PHB/CHC==1 or Uninformative==1) and extract all invoices greater or equal than £25k,
# (This is to put all CCGs on the same footing as not all CCGs apply this threshold)
  
  InvoicesExtractDF = Combined %>% filter(InvoiceOver25k==1 & !is.na(Supplier) & Redacted==0 & PHB_CHC==0 & Uninformative==0)  

# Summarise InvoicesExtractDF > this is our analytical dataset
  InvoicesExtractDF %>% group_by(Supplier) %>% summarise(Suppliers=n()) %>% nrow(.)               #          10,929 total suppliers
  InvoicesExtractDF %>% group_by(SUPPLIER_STANDARD_NAME) %>% summarise(Suppliers=n()) %>% nrow(.) #          10,562 total suppliers
  nrow(InvoicesExtractDF)                                                                         #         225,889 total invoices
  InvoicesExtractDF %>% summarise(Spending=sum(Spending))                                         # £70,496,280,797 total spending

# Tidy up the file for analysis
  names(InvoicesExtractDF)
  InvoicesExtractDF=InvoicesExtractDF[,c(1:10,12:19,30:48)]
  names(InvoicesExtractDF)[18:19]=c("CCCH_Status","VCSE_Status")

# Save analytical file InvoicesExtractDF
  saveRDS(InvoicesExtractDF,paste0(DataOutputDir,"02_InvoicesExtractDF.RDS"))
  write.csv(InvoicesExtractDF,paste0(DataOutputDir,"02_InvoicesExtractDF.csv"),row.names=FALSE)

#----

# End