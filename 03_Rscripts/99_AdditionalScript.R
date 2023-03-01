# 


### Preliminary ----
library(tidyverse)
#library(janitor)  # easy way to add column total to dataframe using adorn_totals("row")
#library(scales)   # for easy labelling of ggplots
#library(patchwork)# for easy placement of plots side-by-side

#library(ggrepel)

# Define working directory
# Assumes script in /Rscripts directory - all paths relative  
setwd("H:/00_Sheaff New Project/00_FinalReport/3SC/03_Rscripts")
DataInputDir="../04_Data/processed/"
ExternalDataDir="../04_Data/external/"
DataOutputDir="../04_Data/processed/"
CSVOutputDir="../05_Outputs/CSVfiles/"
ForReportOutputDir="../05_Outputs/ForReport/"
PrivateDir="../06_Private/"
#----

# Extract summary stats reported in Report Section 2.1----

# Read in the full set of CCG invoices to summarise composition of the data
FullDataDF=readRDS(paste0(DataInputDir,"02_FullCollatedDataset.RDS"))
nrow(FullDataDF) # 689,536
names(FullDataDF)

# Read in the 'analytical dataset'
InvoicesExtractDF=readRDS(paste0(DataInputDir,"02_InvoicesExtractDF.RDS"))
nrow(InvoicesExtractDF) # 225889

# Get 2018 pop data & join to VCSESpend Data
PopData=read.csv(paste0(ExternalDataDir,"CCG2018Pops.csv"))

sort(unique(FullDataDF$CCGName)) # 189 CCGs in 2018-19
189/4
length(FullDataDF$CCGName[1:47])    # 47
length(FullDataDF$CCGName[48:95])   # 48
length(FullDataDF$CCGName[96:142])  # 47
length(FullDataDF$CCGName[143:189]) # 47

names(FullDataDF)[30]="VCSE"  # Rename VCSE status for convenience

head(FullDataDF)
names(FullDataDF)

# Total VCSE spending & count of Invoices (all transactions)
FullDataDF %>% filter(VCSE==1 & PositiveFlag==1) %>% group_by(CCGName) %>% summarise(TotVCSESpend=sum(Spending),Invoices=n()) %>% 
  arrange(desc(TotVCSESpend)) %>% mutate(VCSESpendRank=seq(1:nrow(.))) %>% 
  mutate(PerTransaction=TotVCSESpend/Invoices) %>% arrange(desc(PerTransaction)) %>% 
  mutate(VCSEPerTransactionRank=seq(1:nrow(.))) %>% filter(CCGName=="NHS TOWER HAMLETS CCG")

# Percent Transactions that are VCSE (all transactions)
FullDataDF  %>% filter(PositiveFlag==1) %>% group_by(CCGName) %>% summarise(TotInvoices=n(),VCSEInvoices=sum(VCSE)) %>%
  mutate(PercentInvoicesVCSE=VCSEInvoices/TotInvoices) %>% arrange(desc(PercentInvoicesVCSE)) %>%
  mutate(Rank=seq(1:nrow(.))) %>% filter(CCGName=="NHS TOWER HAMLETS CCG")

# Percent value of transactions that are VCSE (all transactions)
FullDataDF %>% filter(PositiveFlag==1) %>% mutate(VCSESpend=VCSE*Spending) %>% 
  group_by(CCGName) %>% summarise(TotValue=sum(Spending),VCSESpend=sum(VCSESpend)) %>%
  mutate(PercentVCSESpend=VCSESpend/TotValue) %>% arrange(desc(PercentVCSESpend)) %>%
  mutate(Rank=seq(1:nrow(.))) %>% filter(CCGName=="NHS TOWER HAMLETS CCG")

# Actual number of VCSE Suppliers
Part1 = FullDataDF %>% filter(PositiveFlag==1) %>% group_by(CCGName, SUPPLIER_STANDARD_NAME) %>%
  summarise(Spending=sum(Spending)) %>% group_by(CCGName) %>% 
  summarise(CountSuppliers=n(),ValueSuppliers=sum(Spending)) 
  
Part2 = FullDataDF %>% filter(VCSE==1 & PositiveFlag==1) %>% group_by(CCGName, SUPPLIER_STANDARD_NAME) %>%
  summarise(Spending=sum(Spending)) %>% group_by(CCGName) %>% 
  summarise(CountVCSESuppliers=n(),ValueVCSESuppliers=sum(Spending)) 

Output=left_join(Part1,Part2,by="CCGName")

Output %>% mutate(PercentCountVCSE=CountVCSESuppliers/CountSuppliers) %>% arrange(desc(PercentCountVCSE)) %>%
  mutate(CountRank=seq(1:nrow(.))) %>% mutate(PercentSpendVCSE=ValueVCSESuppliers/ValueSuppliers) %>%
  arrange(desc(PercentCountVCSE)) %>% mutate(ValueRank=seq(1:nrow(.))) %>%
  filter(CCGName=="NHS TOWER HAMLETS CCG")

Output %>% mutate(PercentCountVCSE=CountVCSESuppliers/CountSuppliers) %>% arrange(desc(CountVCSESuppliers)) %>%
  mutate(CountRank=seq(1:nrow(.))) %>% arrange(desc(PercentCountVCSE)) %>% mutate(PercentRank=seq(1:nrow(.))) %>%
  filter(CCGName=="NHS TOWER HAMLETS CCG")



# Percent value of transactions >£25k that are VCSE
names(InvoicesExtractDF)

InvoicesExtractDF %>% filter(PositiveFlag==1) %>% mutate(VCSESpend=VCSE_Status*Spending) %>% 
  group_by(CCGName) %>% summarise(TotValue=sum(Spending),VCSESpend=sum(VCSESpend)) %>%
  mutate(PercentVCSESpend=VCSESpend/TotValue) %>% arrange(desc(PercentVCSESpend)) %>%
  mutate(Rank=seq(1:nrow(.))) %>% filter(CCGName=="NHS TOWER HAMLETS CCG")

###############################################################################
###############################################################################

# Lincolnshire - need to do this for the 135 suppliers
135/4

# Now we need to aggregate data to the new CCG geography, and re-calculate PerCapita VCSE

# Get the lookup to link 2018/19 CCGs to 2020/21 CCGs
CCGLookup = read.csv(paste0(ExternalDataDir,"CCGLookupForR.csv"))
# Lookup constructed on basis of https://tinyurl.com/2p9zc39c
# and https://tinyurl.com/ywsduztv

names(CCGLookup)[1:2]=c("CCGCode","CCGName")


# Total VCSE spending & count of Invoices (all transactions)
Table1_1819 = FullDataDF %>% filter(VCSE==1 & PositiveFlag==1) %>%
  group_by(CCGCode) %>% summarise(CCGName=first(CCGName),TotVCSESpend=sum(Spending),Invoices=n())

names(Table1_1819)
head(Table1_1819)

Table1_1819 %>% 
  left_join(CCGLookup, by="CCGCode") %>% group_by(CCGName2020) %>% 
  summarise(VCSESpend=sum(TotVCSESpend),TotInvoices=sum(Invoices)) %>%
  filter(CCGName2020!="NHS West Sussex CCG" & CCGName2020!="NHS Bradford District and Craven CCG" &
           CCGName2020!="NHS Cheshire CCG") %>%
  mutate(PertransactionSpend=VCSESpend/TotInvoices) %>% arrange(desc(VCSESpend)) %>%
  mutate(VCSESpendRank=seq(1:nrow(.))) %>% arrange(desc(PertransactionSpend)) %>%
  mutate(PertransactionSpendRank=seq(1:nrow(.))) %>% filter(CCGName2020=="NHS Lincolnshire CCG")


# Percent Transactions that are VCSE (all transactions)
Table2_1819 = FullDataDF  %>% filter(PositiveFlag==1) %>% group_by(CCGCode) %>% summarise(CCGName=first(CCGName),TotInvoices=n(),VCSEInvoices=sum(VCSE))

Table2_1819 %>% 
  left_join(CCGLookup, by="CCGCode") %>% group_by(CCGName2020) %>% 
  summarise(VCSEInvoices=sum(VCSEInvoices),TotInvoices=sum(TotInvoices)) %>%
  filter(CCGName2020!="NHS West Sussex CCG" & CCGName2020!="NHS Bradford District and Craven CCG" &
           CCGName2020!="NHS Cheshire CCG") %>%
  mutate(PertransactionSpend=VCSEInvoices/TotInvoices) %>% arrange(desc(PertransactionSpend)) %>%
  mutate(Rank=seq(1:nrow(.))) %>% filter(CCGName2020=="NHS Lincolnshire CCG")

# Percent value of transactions that are VCSE (all transactions)
Table3_1819 = FullDataDF  %>% filter(PositiveFlag==1) %>% mutate(VCSESpend=VCSE*Spending) %>%
  group_by(CCGCode) %>% summarise(CCGName=first(CCGName),TotValue=sum(Spending),VCSESpend=sum(VCSESpend))

Table3_1819 %>% 
  left_join(CCGLookup, by="CCGCode") %>% group_by(CCGName2020) %>% 
  summarise(TotValue=sum(TotValue),VCSESpend=sum(VCSESpend)) %>%
  filter(CCGName2020!="NHS West Sussex CCG" & CCGName2020!="NHS Bradford District and Craven CCG" &
           CCGName2020!="NHS Cheshire CCG") %>%
  mutate(PercentVCSESpend=VCSESpend/TotValue) %>% arrange(desc(PercentVCSESpend)) %>%
  mutate(Rank=seq(1:nrow(.))) %>% filter(CCGName2020=="NHS Lincolnshire CCG")

# Actual number (and value) of VCSE Suppliers
Part1 = FullDataDF %>% filter(PositiveFlag==1) %>% group_by(CCGCode, SUPPLIER_STANDARD_NAME) %>%
  summarise(CCGName=first(CCGName),Spending=sum(Spending)) %>% group_by(CCGName) %>% 
  summarise(CCGCode=first(CCGCode),CountSuppliers=n(),ValueSuppliers=sum(Spending)) 

Part2 = FullDataDF %>% filter(VCSE==1 & PositiveFlag==1) %>% group_by(CCGCode, SUPPLIER_STANDARD_NAME) %>%
  summarise(CCGName=first(CCGName),Spending=sum(Spending)) %>% group_by(CCGName) %>% 
  summarise(CCGCode=first(CCGCode),CountVCSESuppliers=n(),ValueVCSESuppliers=sum(Spending)) 

Table4_1819=left_join(Part1,Part2,by="CCGName")
Table4_1819=Table4_1819[,c(1:4,6:7)]
names(Table4_1819)[2]="CCGCode"
head(Table4_1819)

Table4_1819 %>% 
  left_join(CCGLookup, by="CCGCode") %>% group_by(CCGName2020) %>% 
  summarise(CountSuppliers=sum(CountSuppliers),ValueSuppliers=sum(ValueSuppliers),
            CountVCSESuppliers=sum(CountVCSESuppliers),ValueVCSESuppliers=sum(ValueVCSESuppliers)) %>%
  filter(CCGName2020!="NHS West Sussex CCG" & CCGName2020!="NHS Bradford District and Craven CCG" &
           CCGName2020!="NHS Cheshire CCG") %>% arrange(desc(CountVCSESuppliers)) %>%
  mutate(CountRank=seq(1:nrow(.))) %>% mutate(PercentVCSESuppliers=CountVCSESuppliers/CountSuppliers) %>%
  arrange(desc(PercentVCSESuppliers)) %>% mutate(PercentRank=seq(1:nrow(.))) %>%
  filter(CCGName2020=="NHS Lincolnshire CCG")


#Output %>% mutate(PercentCountVCSE=CountVCSESuppliers/CountSuppliers) %>% arrange(desc(CountVCSESuppliers)) %>%
#  mutate(CountRank=seq(1:nrow(.))) %>% arrange(desc(PercentCountVCSE)) %>% mutate(PercentRank=seq(1:nrow(.))) %>%
#  filter(CCGName==InputCCGName)




# Percent value of transactions >£25k that are VCSE
InvoicesExtractDF %>% filter(PositiveFlag==1) %>% 
  mutate(VCSESpend=VCSE_Status*Spending) %>% 
  group_by(CCGCode) %>% summarise(CCGName=first(CCGName), TotValue=sum(Spending),VCSESpend=sum(VCSESpend)) %>%
  left_join(CCGLookup, by="CCGCode") %>% group_by(CCGName2020) %>% 
  summarise(TotValue=sum(TotValue),VCSESpend=sum(VCSESpend))  %>%
  filter(CCGName2020!="NHS West Sussex CCG" & CCGName2020!="NHS Bradford District and Craven CCG" &
           CCGName2020!="NHS Cheshire CCG") %>% mutate(PercentVCSESpend=VCSESpend/TotValue) %>%
  arrange(desc(PercentVCSESpend)) %>% mutate(PercentRank=seq(1:nrow(.))) %>% filter(CCGName2020=="NHS Lincolnshire CCG")

###############################################################################
###############################################################################

# Original Lincolnshire components

InputCCGName="NHS LINCOLNSHIRE EAST CCG"
InputCCGName="NHS LINCOLNSHIRE WEST CCG"
InputCCGName="NHS SOUTH WEST LINCOLNSHIRE CCG"
InputCCGName="NHS SOUTH LINCOLNSHIRE CCG"


# Total VCSE spending & count of Invoices (all transactions)
FullDataDF %>% filter(VCSE==1 & PositiveFlag==1) %>% group_by(CCGName) %>% summarise(TotVCSESpend=sum(Spending),Invoices=n()) %>% 
  arrange(desc(TotVCSESpend)) %>% mutate(VCSESpendRank=seq(1:nrow(.))) %>% 
  mutate(PerTransaction=TotVCSESpend/Invoices) %>% arrange(desc(PerTransaction)) %>% 
  mutate(VCSEPerTransactionRank=seq(1:nrow(.))) %>% filter(CCGName==InputCCGName)

# Percent Transactions that are VCSE (all transactions)
FullDataDF  %>% filter(PositiveFlag==1) %>% group_by(CCGName) %>% summarise(TotInvoices=n(),VCSEInvoices=sum(VCSE)) %>%
  mutate(PercentInvoicesVCSE=VCSEInvoices/TotInvoices) %>% arrange(desc(PercentInvoicesVCSE)) %>%
  mutate(Rank=seq(1:nrow(.))) %>% filter(CCGName==InputCCGName)

# Percent value of transactions that are VCSE (all transactions)
FullDataDF %>% filter(PositiveFlag==1) %>% mutate(VCSESpend=VCSE*Spending) %>% 
  group_by(CCGName) %>% summarise(TotValue=sum(Spending),VCSESpend=sum(VCSESpend)) %>%
  mutate(PercentVCSESpend=VCSESpend/TotValue) %>% arrange(desc(PercentVCSESpend)) %>%
  mutate(Rank=seq(1:nrow(.))) %>% filter(CCGName==InputCCGName)

# Actual number of VCSE Suppliers
Part1 = FullDataDF %>% filter(PositiveFlag==1) %>% group_by(CCGName, SUPPLIER_STANDARD_NAME) %>%
  summarise(Spending=sum(Spending)) %>% group_by(CCGName) %>% 
  summarise(CountSuppliers=n(),ValueSuppliers=sum(Spending)) 

Part2 = FullDataDF %>% filter(VCSE==1 & PositiveFlag==1) %>% group_by(CCGName, SUPPLIER_STANDARD_NAME) %>%
  summarise(Spending=sum(Spending)) %>% group_by(CCGName) %>% 
  summarise(CountVCSESuppliers=n(),ValueVCSESuppliers=sum(Spending)) 

Output=left_join(Part1,Part2,by="CCGName")

Output %>% mutate(PercentCountVCSE=CountVCSESuppliers/CountSuppliers) %>% arrange(desc(CountVCSESuppliers)) %>%
  mutate(CountRank=seq(1:nrow(.))) %>% arrange(desc(PercentCountVCSE)) %>% mutate(PercentRank=seq(1:nrow(.))) %>%
  filter(CCGName==InputCCGName)

# Percent value of transactions >£25k that are VCSE
names(InvoicesExtractDF)

InvoicesExtractDF %>% filter(PositiveFlag==1) %>% mutate(VCSESpend=VCSE_Status*Spending) %>% 
  group_by(CCGName) %>% summarise(TotValue=sum(Spending),VCSESpend=sum(VCSESpend)) %>%
  mutate(PercentVCSESpend=VCSESpend/TotValue) %>% arrange(desc(PercentVCSESpend)) %>%
  mutate(Rank=seq(1:nrow(.))) %>% filter(CCGName==InputCCGName)









