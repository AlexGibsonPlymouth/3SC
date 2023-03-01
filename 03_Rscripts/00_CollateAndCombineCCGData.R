# CollateAndCombineCCGData.R

# Preamble ----

# The original 'raw' CCG monthly accounts are available as CCG-specific 
# zipfiles in the 3CS/data/raw/ directory. These are for information only.

# Extensive manual data cleaning (correcting titles, repairing problems 
# arising when PDF files had to be converted using 'Tabula', etc) produced
# a set of CCG-specific Excel files in ../data/processed/Collated_CCG_Invoices

# Only from this point has it been possible to provide something close to a
# Reproducible Analytical Pipeline (https://tinyurl.com/ycy5b7d4) using this
# and subsequent R scripts.  Any manual intervention (as in collating 
# information on suppliers) is identified in the R scripts. 

# This R script collates the 189 useable CCG files (see github README.md and
# CCG_VCSE_Spend_Report.docx) into a single file (CollatedInvoices.RDS - also
# saved as CollatedInvoices.csv)

#----

### Preliminary ----
  library(tidyverse)
  library(readxl)     # To read Excel files

# Define working directory
# Assumes script in /Rscripts directory - all paths relative  
  DataInputDir="../02_CollatedCCGAccounts/"
  ExternalDataDir="../04_Data/external/"
  CollatedDataOutputDir="../04_Data/processed/"

#----
  
### Combine CCG files and save----
  
# Retrieve list of corrected CCG account files (these are xlsx files)
  FileList=read.csv(paste0(DataInputDir,"99_CCG_CollatedDataList.csv"))
  CCGCount=nrow(FileList)

# Loop to read .xlsx CCG files > reporting progress to console to 
#                                identify problems if they occur
#                                > states number of suppliers, invoices 
#                                  and sum of invoices
  
  #for (i in 1:CCGCount){
  options(warn=0)
  for (i in 1:CCGCount){
      Test1 = read_excel(paste0(DataInputDir,FileList[i,1])) # read .xlsx file
    
    cat(i,"\t",FileList[i,1],"\t")                     
    
    Test1 = Test1 %>% select(Entity,`Expense Type`,`Expense area`,Supplier,`AP Amount (£)`)
    #Out=Test1 %>% arrange(Entity) %>% summarise(Entity1 = first(Entity), Entity2 = last(Entity))
    Out=Test1 %>% arrange(Entity) %>% summarise(Entity = first(Entity))
    Suppliers=Test1 %>% group_by(Supplier) %>% summarise(Count=n()) %>% nrow(.)
    SumAmount=sum(Test1$`AP Amount (£)`)
    Count=nrow(Test1)
    
    cat(Out$Entity,"\t",Suppliers,"\t",Count,"\t",SumAmount,"\n")
    
    # Collate the CCG files into CollatedInvoices
      if(i==1){
        Results=data.frame(Count=i,File=FileList[i,1],CCG=Out$Entity,Suppliers=Suppliers,Invoices=Count,Amount=SumAmount)
        CollatedInvoices=Test1
      } else {
        Results=rbind(Results,data.frame(Count=i,File=FileList[i,1],CCG=Out$Entity,Suppliers=Suppliers,Invoices=Count,Amount=SumAmount))
        CollatedInvoices=rbind(CollatedInvoices,Test1)
      }
  }

# Overview of CollatedInvoices for visual check
  length(unique(CollatedInvoices$Entity)) # 189 CCGs
  nrow(CollatedInvoices)                  # 689,536 records
  sum(CollatedInvoices$`AP Amount (£)`)   # £69,511,531,698 (NB - incl both +ve & -ve amounts)
  
# Make the column names more friendly
  names(CollatedInvoices)[c(1:3,5)]=c("CCGName","ExpenseType","ExpenseArea","Amount")
  names(CollatedInvoices)

# Add CCG Codes (originally from from https://tinyurl.com/4mkam6r8)
  CCG_Lookup=read.csv(paste0(ExternalDataDir,"Clinical_Commissioning_Groups_(April_2018)_Names_and_Codes_in_England.csv"))
  CCG_Lookup=CCG_Lookup[,c(2,5)]
  names(CCG_Lookup)[1:2]=c("CCGCode","CCGName")
  
  CollatedInvoices=left_join(CollatedInvoices,CCG_Lookup,by="CCGName")
  CollatedInvoices=CollatedInvoices[,c(1,6,2:5)]

# Add [0,1] flag whether 'invoice' is positive, and new cols for 
# positive invoices (=Income) and negative invoices (=Spending)
  CollatedInvoices = CollatedInvoices %>% mutate(PositiveFlag = ifelse(Amount>0,1,0))
  CollatedInvoices = CollatedInvoices %>% mutate(Income = ifelse(PositiveFlag==0,Amount,0))
  CollatedInvoices = CollatedInvoices %>% mutate(Spending = ifelse(PositiveFlag==1,Amount,0))

# Add [0,1] flag whether 'invoice' is £25k or above 
  CollatedInvoices = CollatedInvoices %>% mutate(InvoiceOver25k = ifelse(Amount>=25000,1,0))

# Convert all text to uppercase to facilitate matching etc
  CollatedInvoices$CCGName=toupper(CollatedInvoices$CCGName)
  CollatedInvoices$CCGCode=toupper(CollatedInvoices$CCGCode)
  CollatedInvoices$ExpenseType=toupper(CollatedInvoices$ExpenseType)
  CollatedInvoices$ExpenseArea=toupper(CollatedInvoices$ExpenseArea)
  CollatedInvoices$Supplier=toupper(CollatedInvoices$Supplier)
  
# Hack hospices with the same name but in different locations
  
  CollatedInvoices = CollatedInvoices %>%
    mutate(Supplier = ifelse((CCGCode=="E38000072" | CCGCode=="E38000088" | CCGCode=="E38000186") &
                               Supplier=="ST JOSEPHS HOSPICE","ST JOSEPHS HOSPICE (HACKNEY)",Supplier))
  
  CollatedInvoices = CollatedInvoices %>%
    mutate(Supplier = ifelse((CCGCode=="E38000101" | CCGCode=="E38000161") &
                               Supplier=="ST JOSEPHS HOSPICE","ST JOSEPHS HOSPICE (LIVERPOOL)",Supplier))
  
  CollatedInvoices = CollatedInvoices %>%
    mutate(Supplier = ifelse((CCGCode=="E38000101" | CCGCode=="E38000161") &
                               Supplier=="ST JOSEPHS HOSPICE","ST JOSEPHS HOSPICE (LIVERPOOL)",Supplier))
  
  CollatedInvoices = CollatedInvoices %>%
    mutate(Supplier = ifelse((CCGCode=="E38000073" | CCGCode=="E38000073") &
                               (Supplier=="ST MICHAELS HOSPICE" | Supplier=="ST MICHAEL'S HOSPICE"),
                             "ST MICHAELS HOSPICE (HARROGATE)",Supplier))
  
  CollatedInvoices = CollatedInvoices %>%
    mutate(Supplier = ifelse((CCGCode=="E38000076" | CCGCode=="E38000076") &
                               (Supplier=="ST MICHAELS HOSPICE" | Supplier=="ST MICHAEL'S HOSPICE"),
                             "ST MICHAEL'S HOSPICE (HASTINGS)",Supplier))
  
  CollatedInvoices = CollatedInvoices %>%
    mutate(Supplier = ifelse((CCGCode=="E38000078" | CCGCode=="E38000078") &
                               (Supplier=="ST MICHAELS HOSPICE" | Supplier=="ST MICHAEL'S HOSPICE"),
                             "ST MICHAEL'S HOSPICE (HEREFORDSHIRE)",Supplier))
  
 
# Save the collated data as an RDS and csv file
  saveRDS(CollatedInvoices,paste0(CollatedDataOutputDir,"01_CollatedInvoices.RDS"))
  write.csv(CollatedInvoices,paste0(CollatedDataOutputDir,"01_CollatedInvoices.csv"), row.names=FALSE)

#----
  
### Extract and save list of suppliers as input for manual processing----
  
# Getting details about suppliers required cross-referencing with Companies 
# House and Charity Commission registers *AND* extensive manual searching
# of supplier websites - it has not been possible to capture this process.
# The results of this are saved as SuppliersInfo_FINAL.csv/SuppliersInfo_FINAL.RDS
# and is used by Rscript 01_CleanCCGDataAndExtractAnalyticalDataset.R
  
  UniqueSupplierNames=CollatedInvoices %>% group_by(Supplier) %>% summarise(Supplier=first(Supplier))
  nrow(UniqueSupplierNames) # 13453 unique 'named' suppliers
  write.csv(UniqueSupplierNames,paste0(CollatedDataOutputDir,"01_UniqueSupplierNames.csv"), row.names=FALSE)

#### end
