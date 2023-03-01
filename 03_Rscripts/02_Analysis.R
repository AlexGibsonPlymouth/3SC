# 02_AnalysisForReport.R

# Preamble ----

# This script implements all the analysis, including plot generation, used
# in the report 'CCG_VCSE_Spend_Report.docx' and github README.md
# It also produces the various summary .csv files in the github /outputs 
# directory.

#----


### Preliminary ----
  library(tidyverse)
  library(janitor)  # easy way to add column total to dataframe using adorn_totals("row")
  library(scales)   # for easy labelling of ggplots
  library(patchwork)# for easy placement of plots side-by-side

  #library(ggrepel)

# Define working directory
# Assumes script in /Rscripts directory - all paths relative  
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

# Read in the 'analytical dataset'
  InvoicesExtractDF=readRDS(paste0(DataInputDir,"02_InvoicesExtractDF.RDS"))

# How many CCGs are covered?
  length(unique(FullDataDF$CCGCode))         # 189 CCG codes in full dataset
  length(unique(InvoicesExtractDF$CCGCode))  # 189 CCG name 'analytical dataset'

# Using the full dataset:
# How many 'bad' records?
  BadRecords = FullDataDF %>% filter(is.na(Supplier) | Redacted==1 | PHB_CHC==1 | Uninformative==1)  
  BadRecords %>% summarise(n())  
  BadRecords %>% summarise(n()/nrow(FullDataDF))
  # 1287 records (0.00187 of all) missing supplier, redacted, with PHB or CHC 
  # entries or uninformative (i.e. no way on knowing what type of supplier, let 
  # alone supplier name)

# How many records with no ExpenseArea information?
  BadExpenseArea = FullDataDF %>% filter(is.na(ExpenseArea))
  BadExpenseArea %>% summarise(n())                   
  BadExpenseArea %>% summarise(n())/nrow(FullDataDF)
  # 27,846 records (0.04038368 of all) missing ExpenseArea

# How many records with no ExpenseType information?
  BadExpenseType= FullDataDF %>% filter(is.na(ExpenseType)) 
  BadExpenseType %>% summarise(n())                   
  BadExpenseType %>% summarise(n())/nrow(FullDataDF)
  # 32,412 missing (0.04700552 of all) missing ExpenseType
  
# How are missing ExpenseArea and ExpenseType entries distributed between CCGs?
  CCGList=FullDataDF %>% group_by(CCGName) %>% summarise(TotalRecords=n())
  EA_Counts=FullDataDF %>% group_by(CCGName) %>% filter(is.na(ExpenseArea)) %>% summarise(MissingEA=n())
  EA_Join=left_join(CCGList,EA_Counts, by="CCGName")
  EA_Join = EA_Join %>% replace(is.na(.), 0) %>%
    mutate(Percent_EA = MissingEA/TotalRecords) %>% arrange(desc(Percent_EA))
  head(EA_Join,20)

  ET_Counts=FullDataDF %>% group_by(CCGName) %>% filter(is.na(ExpenseType)) %>% summarise(MissingET=n())
  ET_Join=left_join(CCGList,ET_Counts, by="CCGName")
  ET_Join = ET_Join %>% replace(is.na(.), 0) %>%
    mutate(Percent_ET = MissingET/TotalRecords) %>% arrange(desc(Percent_ET))
  head(ET_Join,20)
  
  MissingExpenseInfo=Combined=left_join(EA_Join,ET_Join,by="CCGName")
  MissingExpenseInfo = MissingExpenseInfo %>% mutate(Indicator = Percent_EA+Percent_ET) %>% arrange(desc(Indicator))
  print(MissingExpenseInfo,n=25)

  sum(MissingExpenseInfo$MissingEA[1:14]+MissingExpenseInfo$MissingET[1:14])/
    sum(MissingExpenseInfo$MissingEA+MissingExpenseInfo$MissingET)
  # 0.9509609 of all invoices with missing Expense information in just 14 CCGs
  

# For Table 2: How many records (transactions) in the full dataset?
  nrow(FullDataDF)                        # 689,536

# For Table 2: How many CCG payments (positive transactions) in the full dataset?
  sum(FullDataDF$PositiveFlag)            # 622,514 records with positive amounts (expenditure invoices of any size)

# For Table 2: What is the total value of CCG payments (positive transactions) in the full dataset?
  sum(FullDataDF$Spending)                # £72,179,437,919 total expenditure (of any size invoice)

# For Table 2: What is the total value of CCG income (negative transactions) in the full dataset?
  nrow(FullDataDF)-sum(FullDataDF$PositiveFlag) # 67,022 records with negative amounts (income invoices)
  sum(FullDataDF$Income)                        # -£2,667,906,221 total income (i.e. negative amounts)

# For Table 2: What is the value of CCG income from invoices >£25k in the full dataset?
  FullDataDF %>% filter(InvoiceOver25k==1) %>% nrow()                   
  FullDataDF %>% filter(InvoiceOver25k==1) %>% nrow()/sum(FullDataDF$PositiveFlag)  
  # 226,138 records with spending >£25k (0.3632657 of total positive invoices)

  FullDataDF %>% filter(InvoiceOver25k==1) %>% summarise(sum(Spending))
  FullDataDF %>% filter(InvoiceOver25k==1) %>% summarise(sum(Spending))/sum(FullDataDF$Spending)
  # £70,524,855,859 expenditure (0.9770768 of all spending) through invoices >=£25k
  # Note that this includes all suppliers - even those removed because invalid and 
  # included in the 'analytical dataset' of invoices >£25k (see below)

#----

# Extract summary stats reported in Report Section 2.2----

# What is the CCG-level %split of invoices between over/under the £25k threshold?
# (Used by Report Figure 1)
  InvoiceSplit = FullDataDF %>% filter(PositiveFlag==1) %>% group_by(CCGName) %>%
    summarise(CCGCode = first(CCGCode),Over=sum(InvoiceOver25k),Total=n())
  InvoiceSplit = InvoiceSplit %>% mutate(PercentOver = Over/Total, PercentUnder=1-PercentOver) %>% arrange(PercentUnder)
  InvoiceSplit = InvoiceSplit %>% mutate(CCG = seq(1,nrow(.)))
  write.csv(InvoiceSplit,paste0(CSVOutputDir,"ThresholdSplitByCCG.csv"),row.names=FALSE)
  
  tail(InvoiceSplit,4)
  head(InvoiceSplit,n=22) %>% print(n=22)

# Plot of CCG-level %expenditure over £25k
  dev.new(width=16,height=6,units="cm",noRStudioGD = TRUE)   

  Over25kSpendPlot<-ggplot(InvoiceSplit, aes(x=CCG, y=PercentOver)) +
  geom_point(size=1, shape=3) +
  scale_y_continuous(name="% invoices >£25k", limits=c(0,1), 
                     breaks = c(0.0,0.2,0.4,0.6,0.8,1.0),
                     labels = scales::percent_format(accuracy = 1.0)) +
  scale_x_discrete(name="CCGs in analytical dataset (n = 189)") +
  theme(plot.background = element_rect(fill="white"),
        legend.position="none",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_blank(),
        axis.line = element_line(color="black", linewidth =0.25),
        axis.text.x = element_blank(),
        axis.text.y.left = element_text(color="black", size = 9, hjust=1, vjust = 0.5),
        axis.title.x=element_text(color="black", size=9, vjust = -2.0, hjust=0.5),
        axis.title.y.left=element_text(color="black", size=9, vjust = 2, hjust=0.5),
        panel.background = element_rect(fill="white"),
        panel.grid.major.y = element_line(colour = "black", linewidth = 0.1, linetype="dotted"),
        panel.grid.major.x = element_blank())
  Over25kSpendPlot
  ggsave(paste0(ForReportOutputDir,"Figure1.png"), width=16,height=6,units="cm", dpi = 300)


# How many 'named' suppliers are in (1) the full set of invoices, 
# (2) the spending invoices set, (3) the set of invoices for >£25k, and
# (4) the 'analytical dataset' of invoices >£25k once 'invalid' payments are removed?

  FullDataDF %>% group_by(Supplier) %>% summarise(Total=n()) %>% nrow(.)
  # 13,455 named suppliers in the full dataset (i.e. positive or negative amounts)

  FullDataDF %>% filter(PositiveFlag==1) %>% group_by(Supplier) %>% summarise(Total=n()) %>% nrow(.)
  # 13,418 suppliers getting payments (i.e. positive values = payments)
  
  FullDataDF %>% filter(InvoiceOver25k==1) %>% group_by(Supplier) %>% summarise(Total=n()) %>% nrow(.)
  # 10,980 suppliers getting payments over £25k (incl those we
  # exclude in the 'analytical dataset' InvoicesExtractDF)  
  
  InvoicesExtractDF %>% group_by(Supplier) %>% summarise(Total=n()) %>% nrow(.)
  # 10,930 suppliers in the 'analytical dataset'
  # In other words, 50 'invalid invoices' in the FullDataDF are for >£25k!


# And how unique suppliers (using SUPPLIER_STANDARD_NAME) are in (1) the  
# full set of invoices, (2) the spending invoices set, (3) the set of 
# invoices for >£25k, and (4) the 'analytical dataset' of invoices >£25k 
# once 'invalid' payments are removed?
 
  FullDataDF %>% group_by(SUPPLIER_STANDARD_NAME) %>% summarise(Total=n()) %>% nrow(.)
  # 13,034 named suppliers in the full dataset (i.e. positive or negative amounts)
  
  FullDataDF %>% filter(PositiveFlag==1) %>% group_by(SUPPLIER_STANDARD_NAME) %>% summarise(Total=n()) %>% nrow(.)
  # 13,000 suppliers getting payments (i.e. positive values = payments)
  
  FullDataDF %>% filter(InvoiceOver25k==1) %>% group_by(SUPPLIER_STANDARD_NAME) %>% summarise(Total=n()) %>% nrow(.)
  # 10,612 suppliers getting payments over £25k (incl those we
  # exclude in the 'analytical dataset' InvoicesExtractDF)  
  
  InvoicesExtractDF %>% group_by(SUPPLIER_STANDARD_NAME) %>% summarise(Total=n()) %>% nrow(.)
  # 10,562 suppliers in the 'analytical dataset'
 
#----
  
# Extract summary stats reported in Report Section 2.3----
  
  InvoicesExtractDF %>% nrow(.)
  # 225,889 invoices >£25k in the 'analytical dataset'

  length(unique(InvoicesExtractDF$CCGName))
  # 189 CCGs in the 'analytical dataset'
  
  InvoicesExtractDF %>% group_by(SUPPLIER_STANDARD_NAME) %>% summarise(n()) %>% nrow(.)
  # 10,562 unique suppliers in the 'analytical dataset'
  
  sum(InvoicesExtractDF$Spending)
  sum(InvoicesExtractDF$Spending)/sum(FullDataDF$Spending)
  # £70,496,280,797 spending through invoices of >£25k (which is 0.9766809 of total CCG Spending)
  
#----
  
# Extract summary stats reported in Report Section 2.4----

# What is the overall national share of invoices/spending going to NHS & LA suppliers 

  SupplierSectorSplit = InvoicesExtractDF%>% group_by(SupplierSector) %>%
    summarise(Invoices=n(),Expenditure=sum(Spending)) %>% 
    mutate(PercentInvoices = Invoices/sum(Invoices), PercentExpenditure = Expenditure/sum(Expenditure))
  
  SupplierSectorSplit %>% filter(SupplierSector=="NHS" | SupplierSector=="LA") %>% pull(PercentInvoices) %>% sum(.)
  # NHS & LA invoices comprise 0.3672761 of all invoices
  SupplierSectorSplit %>% filter(SupplierSector=="NHS" | SupplierSector=="LA") %>% pull(PercentExpenditure) %>% sum(.)
  # NHS & LA expenditure comprises 0.8329596 of total expenditure
  

# And how does it vary by CCG? (This used to create Report Figure 2)
  SupplierSplitByCCG = InvoicesExtractDF %>% group_by(CCGName,SupplierSector) %>%
    summarize(Spending=sum(Spending))  %>% spread(SupplierSector,Spending) %>% 
    mutate(Total = sum(c_across(CareHome:University),na.rm=TRUE)) %>% replace(is.na(.), 0)
# Add percentages
  SupplierSplitByCCG = SupplierSplitByCCG %>% mutate(PC_CareHome=CareHome/Total,
                   PC_Executor=Executor/Total, PC_GPServices=GPServices/Total,
                   PC_GOV=GOV/Total, PC_Hospice=Hospice/Total, PC_LA=LA/Total,
                   PC_LMC_LPC=LMC_LPC/Total, PC_NHS=NHS/Total, PC_Other=Other/Total,
                   PC_PharmOp=PharmOp/Total, PC_PoliceFire=PoliceFire/Total,
                   PC_School=School/Total, PC_Solicitor=Solicitor/Total,
                   PC_University=University/Total)
  
# Save split by supplier type by CCG
  write.csv(SupplierSplitByCCG,paste0(CSVOutputDir,"SupplierCategorySplitByCCG.csv"),row.names=FALSE)


# Plot the %of expenditure that was LA/NHS by CCG (Report Figure 2)
# Extract data for plot
  Fig2PlotData = SupplierSplitByCCG %>%
    transmute(CCGName=CCGName, PercentLA_NHS=(LA+NHS)/Total) %>% arrange(PercentLA_NHS) 
  Fig2PlotData$CCG=seq(1,189)

# Range of NHS/LA suppliers by CCG
  head(Fig2PlotData,n=3)
  tail(Fig2PlotData,n=8)
  
# Information on NHS North East Lincolnshire CCG
  NELincs=InvoicesExtractDF %>%
    filter(CCGName=="NHS NORTH EAST LINCOLNSHIRE CCG") %>%
    group_by(SUPPLIER_STANDARD_NAME) %>% summarise(n(), Spending=sum(Spending)) %>%
    arrange(desc(Spending)) %>% mutate(Percent = Spending/sum(Spending))
  NELincs
  (NELincs$Spending[2]+NELincs$Spending[3]+NELincs$Spending[7])/sum(NELincs$Spending) 
  # 0.2472933

# Information on East Staffs
  EastStaffs=InvoicesExtractDF %>%
    filter(CCGName=="NHS EAST STAFFORDSHIRE CCG") %>%
    group_by(SUPPLIER_STANDARD_NAME) %>% summarise(n(), Spending=sum(Spending)) %>%
    arrange(desc(Spending)) %>% mutate(Percent = Spending/sum(Spending))
  EastStaffs
  EastStaffs$Spending[1]/sum(EastStaffs$Spending) 
  # 0.3085463
  
  dev.off()
  dev.new(width=16,height=6,units="cm",noRStudioGD = TRUE)  
  
  LA_NHS_SpendPlot<-ggplot(Fig2PlotData, aes(x=CCG, y=PercentLA_NHS)) +
    geom_point(size=1, shape=3) +
    scale_y_continuous(name="% LA/NHS spend ", limits=c(0,1), 
                       breaks = c(0.0,0.2,0.4,0.6,0.8,1.0),
                       labels = scales::percent_format(accuracy = 1.0)) +
    scale_x_discrete(name="CCGs in analytical dataset (n = 189)") +
    theme(plot.background = element_rect(fill="white"),
          legend.position="none",
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.text = element_blank(),
          axis.line = element_line(color="black", linewidth =0.25),
          axis.text.x = element_blank(),
          axis.text.y.left = element_text(color="black", size = 9, hjust=1, vjust = 0.5),
          axis.title.x=element_text(color="black", size=9, vjust = -2.0, hjust=0.5),
          axis.title.y.left=element_text(color="black", size=9, vjust = 2, hjust=0.5),
          panel.background = element_rect(fill="white"),
          panel.grid.major.y = element_line(colour = "black", linewidth = 0.1, linetype="dotted"),
          panel.grid.major.x = element_blank())
  LA_NHS_SpendPlot
  ggsave(paste0(ForReportOutputDir,"Figure2.png"), width=16,height=6,units="cm", dpi = 300)
  

#----
  
# Extract summary stats reported in Report Section 2.5----
  # This includes Figure 3
  
# What is the national share of invoices/spending going to GPServices?
  SupplierSectorSplit # 0.0890 of CCG >£25k expenditure on GPServices

  # How does this vary by CCG?
  TotalCCGSpend=InvoicesExtractDF %>% group_by(CCGName) %>%
    summarise(CCGName=first(CCGName),TotalSpend=sum(Spending)) 
  
  CCGSpendOnGPs=InvoicesExtractDF %>% filter(GPServices==1) %>% group_by(CCGName) %>%
    summarise(CCGName=first(CCGName),CCGCode=first(CCGCode),GPSpend=sum(Spending)) 
  
  CCGSpendOnGPs =left_join(TotalCCGSpend,CCGSpendOnGPs,by="CCGName")
  CCGSpendOnGPs=CCGSpendOnGPs[,c(1,3,2,4)]
  
  CCGSpendOnGPs = CCGSpendOnGPs %>% mutate(Percent_GPSpend = GPSpend/TotalSpend) %>%
    arrange(Percent_GPSpend)
  
# Range of % GP spend
  head(CCGSpendOnGPs,3)
  tail(CCGSpendOnGPs,4)
  
# How many GP Services suppliers identified?
  InvoicesExtractDF %>% filter(GPServices==1) %>%
    group_by(SUPPLIER_STANDARD_NAME) %>% summarise(n()) %>% nrow(.)
  # 6,288 GP Services suppliers 

# And extract plot data and produce plot (Report Figure 3)
  Fig3PlotData = CCGSpendOnGPs %>% select(CCGName,Percent_GPSpend) %>% 
    mutate(CCG=c(1:189))

  dev.off()
  dev.new(width=16,height=6,units="cm",noRStudioGD = TRUE) 
  
  GPServices_SpendPlot<-ggplot(Fig3PlotData, aes(x=CCG, y=Percent_GPSpend)) +
    geom_point(size=1, shape=3) +
    scale_y_continuous(name="% spend on GP Services", limits=c(0,0.25), 
                       breaks = c(0.0,0.05,0.1,0.15,0.2,0.25),
                       labels = scales::percent_format(accuracy = 1.0)) +
    scale_x_discrete(name="CCGs in analytical dataset (n = 189)") +
    # Manual Annotation
    geom_text(label="Greater Huddersfield",x=7,y=0.12, hjust=0, size=3) +
    geom_text(label="Crawley",x=17,y=0.10, hjust=0, size=3) +
    geom_text(label="West Norfolk",x=170,y=0.2, hjust=1, size=3) +
    geom_text(label="East Riding of Yorkshire",x=175,y=0.22, hjust=1, size=3) +
    geom_text(label="Hull",x=180,y=0.24, hjust=1, size=3) +
    geom_segment(aes(x = 1, y = 0.000813, xend = 7, yend = 0.11), colour = "black", linewidth=0.5) +
    geom_segment(aes(x = 2, y = 0.000879, xend = 17, yend = 0.09), colour = "black", linewidth=0.5) +
    geom_segment(aes(x = 172, y = 0.2, xend = 187, yend = 0.201), colour = "black", linewidth=0.5) +
    geom_segment(aes(x = 177, y = 0.22, xend = 188, yend = 0.227), colour = "black", linewidth=0.5) +
    geom_segment(aes(x = 182, y = 0.24, xend = 189, yend = 0.241), colour = "black", linewidth=0.5) +
    theme(plot.background = element_rect(fill="white"),
          legend.position="none",
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.text = element_blank(),
          axis.line = element_line(color="black", linewidth =0.25),
          axis.text.x = element_blank(),
          axis.text.y.left = element_text(color="black", size = 9, hjust=1, vjust = 0.5),
          axis.title.x=element_text(color="black", size=9, vjust = -2.0, hjust=0.5),
          axis.title.y.left=element_text(color="black", size=9, vjust = 2, hjust=0.5),
          panel.background = element_rect(fill="white"),
          panel.grid.major.y = element_line(colour = "black", linewidth = 0.1, linetype="dotted"),
          panel.grid.major.x = element_blank())
  GPServices_SpendPlot
  ggsave(paste0(ForReportOutputDir,"Figure3.png"), width=16,height=6,units="cm", dpi = 300)

# List of Greater Huddersfield GPs to compare with 'NHS Payments to
# General Practice 2018/19' (https://tinyurl.com/hkkxcmeu)
  InvoicesExtractDF %>% filter(CCGName=="NHS GREATER HUDDERSFIELD CCG" & GPServices==1) %>%
    group_by(SUPPLIER_STANDARD_NAME) %>% summarise(Spending=sum(Spending))
  InvoicesExtractDF %>% filter(CCGName=="NHS GREATER HUDDERSFIELD CCG" & GPServices==1) %>%
    group_by(SUPPLIER_STANDARD_NAME) %>% summarise(Spending=sum(Spending)) %>% pull(Spending) %>%
    sum
  
# List of Crawley GPs to compare with 'NHS Payments to
  InvoicesExtractDF %>% filter(CCGName=="NHS CRAWLEY CCG" & GPServices==1) %>%
    group_by(SUPPLIER_STANDARD_NAME) %>% summarise(Spending=sum(Spending))
  InvoicesExtractDF %>% filter(CCGName=="NHS CRAWLEY CCG" & GPServices==1) %>%
    group_by(SUPPLIER_STANDARD_NAME) %>% summarise(Spending=sum(Spending)) %>% pull(Spending) %>%
    sum

# List of Crawley GPs to compare with 'NHS Payments to
  InvoicesExtractDF %>% filter(CCGName=="NHS WEST NORFOLK CCG" & GPServices==1) %>%
    group_by(SUPPLIER_STANDARD_NAME) %>% summarise(Spending=sum(Spending)) %>% print(n=50)
  InvoicesExtractDF %>% filter(CCGName=="NHS WEST NORFOLK CCG" & GPServices==1) %>%
    group_by(SUPPLIER_STANDARD_NAME) %>% summarise(Spending=sum(Spending)) %>% pull(Spending) %>%
    sum
  
# List of East Riding of Yorkshire GPs to compare with 'NHS Payments to
  InvoicesExtractDF %>% filter(CCGName=="NHS EAST RIDING OF YORKSHIRE CCG" & GPServices==1) %>%
    group_by(SUPPLIER_STANDARD_NAME) %>% summarise(Spending=sum(Spending)) %>% print(n=50)
  InvoicesExtractDF %>% filter(CCGName=="NHS EAST RIDING OF YORKSHIRE CCG" & GPServices==1) %>%
    group_by(SUPPLIER_STANDARD_NAME) %>% summarise(Spending=sum(Spending)) %>% pull(Spending) %>%
    sum
  
# List of Hull GPs to compare with 'NHS Payments to
  InvoicesExtractDF %>% filter(CCGName=="NHS HULL CCG" & GPServices==1) %>%
    group_by(SUPPLIER_STANDARD_NAME) %>% summarise(Spending=sum(Spending)) %>% print(n=50)
  InvoicesExtractDF %>% filter(CCGName=="NHS HULL CCG" & GPServices==1) %>%
    group_by(SUPPLIER_STANDARD_NAME) %>% summarise(Spending=sum(Spending)) %>% pull(Spending) %>%
    sum
 
#----
  
# Extract summary stats reported in Report Section 3 (the Workflow section)----

  nrow(InvoicesExtractDF) 
  # 225,889 invoices over £25k for valid suppliers
  
 sum(InvoicesExtractDF$Spending)
 # £70,496,280,797 spending via valid >£25k invoices
 
 InvoicesExtractDF %>% group_by(Supplier) %>% summarise(n()) %>% nrow(.)
 # 10,929 named suppliers
 
 InvoicesExtractDF %>% group_by(SUPPLIER_STANDARD_NAME) %>% summarise(n()) %>% nrow(.)
 # 10,562 unique suppliers

# NHS Suppliers 
 InvoicesExtractDF %>% filter(NHS==1) %>% group_by(SUPPLIER_STANDARD_NAME) %>%
   summarise(n()) %>% nrow(.)
 InvoicesExtractDF %>% filter(NHS==1) %>% group_by(SUPPLIER_STANDARD_NAME) %>%
   summarise(n()) %>% nrow(.) /
   InvoicesExtractDF %>% group_by(SUPPLIER_STANDARD_NAME) %>% summarise(n()) %>% nrow(.)
 # 538 unique suppliers are NHS (0.05093732 of total)
 
 InvoicesExtractDF %>% filter(NHS==1) %>% nrow(.) 
 InvoicesExtractDF %>% filter(NHS==1) %>% nrow(.) /
   InvoicesExtractDF %>% nrow(.)
 # 71,424 invoices are NHS (0.316190 of total)

 InvoicesExtractDF %>% filter(NHS==1) %>% pull(Spending) %>% sum
 InvoicesExtractDF %>% filter(NHS==1) %>% pull(Spending) %>% sum / 
   sum(InvoicesExtractDF$Spending)
 # £54,262,880,444 expenditure on NHS suppliers (0.7697269 of total)

# LA Suppliers 
 InvoicesExtractDF %>% filter(LA==1) %>% group_by(SUPPLIER_STANDARD_NAME) %>%
   summarise(n()) %>% nrow(.)
 InvoicesExtractDF %>% filter(LA==1) %>% group_by(SUPPLIER_STANDARD_NAME) %>%
   summarise(n()) %>% nrow(.) /
   InvoicesExtractDF %>% group_by(SUPPLIER_STANDARD_NAME) %>% summarise(n()) %>% nrow(.)
 # 212 unique suppliers are LA (0.02007196 of total)
 
 InvoicesExtractDF %>% filter(LA==1) %>% nrow(.) 
 InvoicesExtractDF %>% filter(LA==1) %>% nrow(.) /
   InvoicesExtractDF %>% nrow(.)
 # 11,540 invoices are NHS (0.05108704 of total)
 
 InvoicesExtractDF %>% filter(LA==1) %>% pull(Spending) %>% sum
 InvoicesExtractDF %>% filter(LA==1) %>% pull(Spending) %>% sum / 
   sum(InvoicesExtractDF$Spending)
 # £4,457,706,446 expenditure on NHS suppliers (0.06323321 of total)
 
# Table 3: Count of (unique) Suppliers cross-tabbed Sector by CC/CH    

  SupplierCount_SectorByStatus=InvoicesExtractDF %>%
   group_by(SUPPLIER_STANDARD_NAME) %>%
   summarise(Count = n(), CCCH_Status = first(CCCH_Status),
             SupplierSector = first(SupplierSector)) %>% 
   group_by(CCCH_Status, SupplierSector) %>%
   summarise(Count = n()) %>%
   spread(CCCH_Status, Count) %>% replace(is.na(.), 0) %>%
   rowwise %>% mutate(Total=sum(c_across('CC&CH':'NotCCorCH'))) 
 
  SupplierCount_SectorByStatus = SupplierCount_SectorByStatus %>%
   mutate(PC_CCandCH = `CC&CH`/Total, PC_CCnotCH = CCnotCH/Total,
          PC_CHnotCC = CHnotCC/Total, PC_NotCCorCH = NotCCorCH/Total,)
 
  SupplierCount_SectorByStatus$Category=c("Care Home","Categorised Other",
                  "Categorised Other","GP Services","Hospice","LA",
                  "Categorised Other","NHS","Other","Categorised Other",
                  "Categorised Other","Categorised Other",
                  "Categorised Other","Categorised Other") 
 
  SupplierCount_SectorByStatus
 
  Table3 = SupplierCount_SectorByStatus %>% group_by(Category) %>% 
  summarise(across(`CC&CH`:Total, sum)) %>%
  adorn_totals("row")
 
  Table3 = Table3[c(6,5,3,1,4,2,7,8),]
  Table3
 
# Table 4: Sum of expenditure cross-tabbed Sector by CC/CH 

  Spending_SectorByStatus=InvoicesExtractDF %>%
    group_by(CCCH_Status,SupplierSector) %>%
    summarise(Spending = sum(Spending)) %>% 
    spread(CCCH_Status,Spending) %>% replace(is.na(.), 0) %>%
    rowwise %>% mutate(Total=sum(c_across('CC&CH':'NotCCorCH'))) 
  
  Spending_SectorByStatus = Spending_SectorByStatus %>%
    mutate(PC_CCandCH = `CC&CH`/Total, PC_CCnotCH = CCnotCH/Total,
           PC_CHnotCC = CHnotCC/Total, PC_NotCCorCH = NotCCorCH/Total,)
  
  Spending_SectorByStatus$Category=c("Care Home","Categorised Other",
                          "Categorised Other","GP Services","Hospice","LA",
                          "Categorised Other","NHS","Other","Categorised Other",
                          "Categorised Other","Categorised Other",
                          "Categorised Other","Categorised Other") 

  Spending_SectorByStatus
  
  Table4 = Spending_SectorByStatus %>% group_by(Category) %>% 
    summarise(across(`CC&CH`:Total, sum)) %>%
    adorn_totals("row")
  
  Table4 = Table4[c(6,5,3,1,4,2,7,8),]
  Table4Millions = Table4[,2:6]/1000000
  Table4Millions

# How many (and what %) non-NHS/LA organisations not in CC/CH registers?
  sum(Table3$NotCCorCH[3:7])                      # 6,416
  sum(Table3$NotCCorCH[3:7])/Table3$Total[8]  # 0.6074607 of all unique suppliers

# How much (and what %) spent on non-NHS/LA organisations not in CC/CH registers?
  sum(Table4$NotCCorCH[3:7])                      # £6,028,789,858
  sum(Table4$NotCCorCH[3:7])/Table4$Total[8]  # 0.08551926 of total expenditure
  
#----
  
# Extract summary stats reported in Report Section 4 ----
  
# Summary of 3SC Project (invoices >£25k)
  length(unique(InvoicesExtractDF$CCGName))                # 189 CCGs in both datasets
  nrow(InvoicesExtractDF)                                  # 225,889 invoices
  length(unique(InvoicesExtractDF$Supplier))               # 10,929 named suppliers
  length(unique(InvoicesExtractDF$SUPPLIER_STANDARD_NAME)) # 10,562 unique suppliers
  sum(InvoicesExtractDF$Spending)                          # £70,496,280,797

# Extract subset of InvoiceExtractDF that equates to CCGs covered by
# the NHS Spend Project (https://github.com/crahal/NHSSpend)
  
  SharedCCGlist<-read.csv(paste0(ExternalDataDir,"ListofCCGsSharedWithNHSProject.csv"))
  ComparisonSubset=left_join(SharedCCGlist,InvoicesExtractDF,by="CCGName")

# How many Charities  
  ComparisonSubset %>% filter(CCFound==1) %>% group_by(SUPPLIER_STANDARD_NAME) %>%
    summarise(n()) %>% nrow(.)
  # 623 charities in comparison dataset
  
  # How many Companies  
  ComparisonSubset %>% filter(CHFound==1) %>% group_by(SUPPLIER_STANDARD_NAME) %>%
    summarise(n()) %>% nrow(.)
  # 2550 companies in comparison dataset

# And extract count of charities and companies for comparison with data from the
# NHS Spend project data
  
  CCCHSuppliersbyCCG=ComparisonSubset %>% group_by(CCGName) %>%
    summarise(Charities=sum(CCFound), Companies=sum(CHFound))

#----
 
# Extract summary stats reported in Report Section 5 ----
# For Table 6 (and used in text)
  
# Suppliers in CC and CH register
  CCandCH = InvoicesExtractDF %>% filter(CCFound==1 & CHFound==1) %>%
    group_by(GivenCompanyCategory,SUPPLIER_STANDARD_NAME) %>%
    summarise (Invoices=n(),Spending=sum(Spending)) %>% 
    group_by(GivenCompanyCategory) %>% summarise (Suppliers=n(),Spending=sum(Spending))
 
# Suppliers in CC but not CH register
  CCnotCH = InvoicesExtractDF %>% filter(CCFound==1 & CHFound==0) %>%
    group_by(GivenCompanyCategory,SUPPLIER_STANDARD_NAME) %>%
    summarise (Invoices=n(),Spending=sum(Spending)) %>% 
    group_by(GivenCompanyCategory) %>% summarise (Suppliers=n(),Spending=sum(Spending))
  
# VCSE Suppliers in CH but not CC register (not NHS/GOV/LA)
  CHnotCC_VCSE = InvoicesExtractDF %>% filter(CCFound==0 & CHFound==1 & VCSE_Status==1 &
                                                NHS==0 & LA==0 & GOV==0) %>%
    group_by(GivenCompanyCategory,SUPPLIER_STANDARD_NAME) %>%
    summarise (Invoices=n(),Spending=sum(Spending)) %>% 
    group_by(GivenCompanyCategory) %>% summarise (Suppliers=n(),Spending=sum(Spending))
  
# Non-VCSE Suppliers in CH but not CC register (not NHS/GOV/LA)
  CHnotCC_NotVCSE = InvoicesExtractDF %>% filter(CCFound==0 & CHFound==1 & VCSE_Status==0 &
                                                   NHS==0 & LA==0 & GOV==0) %>%
    group_by(GivenCompanyCategory,SUPPLIER_STANDARD_NAME) %>%
    summarise (Invoices=n(),Spending=sum(Spending)) %>% 
    group_by(GivenCompanyCategory) %>% summarise (Suppliers=n(),Spending=sum(Spending))

# NHS, GOV and LA suppliers
  NHSGOVLA = InvoicesExtractDF %>% filter(NHS==1 | LA==1 | GOV==1) %>%
    group_by(SupplierSector,SUPPLIER_STANDARD_NAME) %>%
    summarise (Invoices=n(),Spending=sum(Spending)) %>% 
    group_by(SupplierSector) %>% summarise (Suppliers=n(),Spending=sum(Spending))
  
# VCSE suppliers not in CC or CH (not NHS, GOV and LA)
  NeitherCCnorCH_VCSE = InvoicesExtractDF %>% filter(CCFound==0 & CHFound==0 &
                                                       NHS==0 & LA==0 & GOV==0 & VCSE_Status==1) %>%
    group_by(GivenCompanyCategory,SUPPLIER_STANDARD_NAME) %>%
    summarise (Invoices=n(),Spending=sum(Spending)) %>% 
    group_by(GivenCompanyCategory) %>% summarise (Suppliers=n(),Spending=sum(Spending))
 
# Non-VCSE suppliers not in CC or CH (not NHS, GOV and LA)
  NeitherCCnorCH_NotVCSE = InvoicesExtractDF %>% filter(CCFound==0 & CHFound==0 &
                                                  NHS==0 & LA==0 & GOV==0 & VCSE_Status==0) %>%
    group_by(GivenCompanyCategory,SUPPLIER_STANDARD_NAME) %>%
    summarise (Invoices=n(),Spending=sum(Spending)) %>% 
    group_by(GivenCompanyCategory) %>% summarise (Suppliers=n(),Spending=sum(Spending))

    
# Pull together (aggregating as required) to create Table 6
  Table6a = CCandCH %>% slice_head()
  Table6b = CCandCH %>% slice(c(3:7)) %>% adorn_totals("row") %>% slice_tail()
  Table6b$GivenCompanyCategory[1]="Private Limited Companies by guarantee"
  Table6c = CCandCH %>% slice(c(2,8)) %>% adorn_totals("row") %>% slice_tail()
  Table6c$GivenCompanyCategory[1]="Other Company Types"

  Table6d = CCnotCH %>% slice_head()
  Table6d$GivenCompanyCategory[1]="Named charities"

  Table6e = CHnotCC_VCSE %>% slice(c(1,2,8,9))
  Table6f = CHnotCC_VCSE %>% slice(c(3:5,7)) %>% adorn_totals("row") %>% slice_tail()
  Table6f$GivenCompanyCategory[1]="Private Limited Companies by guarantee"
  Table6g = CHnotCC_VCSE %>% slice(6)
  
  Table6h = CHnotCC_NotVCSE %>% slice(c(7:9,11)) %>% adorn_totals("row") %>% slice_tail()
  Table6h$GivenCompanyCategory[1]="Private Limited Companies by guarantee"
  Table6i = CHnotCC_NotVCSE %>% slice(c(1:6,12:14)) %>% adorn_totals("row") %>% slice_tail()
  Table6i$GivenCompanyCategory[1]="Other CH Company Types"
  Table6j = CHnotCC_NotVCSE %>% slice(c(10)) 
  
  Table6k = NHSGOVLA
  names(Table6k)[1]="GivenCompanyCategory"
  Table6k$GivenCompanyCategory=c("Government Bodies","Local Authorities","NHS Organisations")
  Table6l = NeitherCCnorCH_VCSE
  Table6l$GivenCompanyCategory[1]="Other Suppliers (VCSE)"
  Table6m = NeitherCCnorCH_NotVCSE
  Table6m$GivenCompanyCategory[1]="Other Suppliers (Non-VCSE)"
  
  Table6=rbind(Table6a,Table6b,Table6c,Table6d,Table6e,Table6f,Table6g,Table6h,
               Table6i,Table6j,Table6k,Table6l,Table6m)
  
  # Check sums
  InvoicesExtractDF %>% group_by(SUPPLIER_STANDARD_NAME) %>%
    summarise(n()) %>% nrow(.)            # 10,562 Unique suppliers in dataset
  sum(CCandCH$Suppliers)+sum(CCnotCH$Suppliers)+sum(CHnotCC_VCSE$Suppliers)+
    sum(CHnotCC_NotVCSE$Suppliers)+sum(NHSGOVLA$Suppliers)+sum(NeitherCCnorCH_VCSE$Suppliers)+
    sum(NeitherCCnorCH_NotVCSE$Suppliers) # 10,562 Unique suppliers in extracted tables
  sum(Table6$Suppliers)                   # 10,562 Unique suppliers in final aggregated table
  
  # Table sums
  sum(Table6$Suppliers[c(1:10,17)]) # 1051 unique VCSE suppliers
  sum(Table6$Spending[c(1:10,17)]) # £1,912,408,680 VCSE spend
  
  sum(Table6$Suppliers[c(11:16,18)]) # 9511 unique non-VCSE supplier
  sum(Table6$Spending[c(11:16,18)])  # £68,583,872,117 non-VCSE spend
  
  sum(Table6$Suppliers) # 10,562 total unique suppliers
  sum(Table6$Spending)  # £70,496,280,797 total spend
  
  
# What are the summary stats regarding suppliers we have identified as VCSEs?
  names(InvoicesExtractDF)
  InvoicesExtractDF %>% filter(VCSE_Status==1) %>% nrow(.)
  InvoicesExtractDF %>% filter(VCSE_Status==1) %>% nrow(.)/InvoicesExtractDF %>% nrow(.)
  # 13,952 invoices (0.06176485 of total)
  
  InvoicesExtractDF %>% filter(VCSE_Status==1) %>% group_by(SUPPLIER_STANDARD_NAME) %>%
    summarise(n()) %>% nrow(.)
  InvoicesExtractDF %>% filter(VCSE_Status==1) %>% group_by(SUPPLIER_STANDARD_NAME) %>%
    summarise(n()) %>% nrow(.) / InvoicesExtractDF %>% group_by(SUPPLIER_STANDARD_NAME) %>%
    summarise(n()) %>% nrow(.)
  # 1,051 unique suppliers (0.09950767 of total)
  
  InvoicesExtractDF %>% filter(VCSE_Status==1) %>% pull(Spending) %>% sum
  InvoicesExtractDF %>% filter(VCSE_Status==1) %>% pull(Spending) %>% sum / 
    InvoicesExtractDF %>% pull(Spending) %>% sum
  # £1,912,408,680 national spending on VCSE (via invoices >£25k) (0.0271278 of total spend)
  
  
#----
  
# Extract summary stats reported in Report Section 6 ----
# Includes Figs 4 and 5
  
# VCSE Suppliers, ordered by total income 
  VCSE_Overview=InvoicesExtractDF %>% filter(VCSE_Status==1) %>% group_by(SUPPLIER_STANDARD_NAME) %>%
  summarise(Invoices=n(),Spending=sum(Spending)) %>% arrange(desc(Spending))
  VCSE_Overview = VCSE_Overview %>% mutate(ID = seq(1:nrow(.)))
  
  VCSE_Overview = VCSE_Overview[,c(4,1:3)]
  write.csv(VCSE_Overview,paste0(CSVOutputDir,"VCSESuppliers.csv"),row.names=FALSE)

# Plot VCSE Spend
  dev.off()
  dev.new(width=16,height=6,units="cm",noRStudioGD = TRUE)  
  
  VCSE_ActualPlot<-ggplot(VCSE_Overview, aes(ID, Spending)) +
    geom_point(size=1, shape=3) +
    scale_y_continuous(name="VCSE spend (£)",  
                       labels = label_number(prefix = "£", suffix = "M", scale = 1e-6, big.mark = ",")) +
    scale_x_continuous(name="VCSE suppliers in analytical dataset (n = 1043)", limits = c(0, 1051), expand=c(0.005,0)) +
    geom_text(data=subset(VCSE_Overview,Spending > 50000000), aes(ID, Spending,label=SUPPLIER_STANDARD_NAME),
              check_overlap = TRUE, hjust = 0, nudge_x = 8, nudge_y = 0.0008, size=2.5, colour="black") +
    theme(plot.background = element_rect(fill="white"),
          legend.position="none",
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.text = element_blank(),
          axis.line = element_line(color="black", linewidth =0.25),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y.left = element_text(color="black", size = 9, hjust=1, vjust = 0.5),
          axis.title.x=element_text(color="black", size=9, vjust = -2.0, hjust=0.5),
          axis.title.y.left=element_text(color="black", size=9, vjust = 2, hjust=0.5),
          panel.background = element_rect(fill="white"),
          panel.grid.major.y = element_line(colour = "black", linewidth = 0.1, linetype="dotted"),
          panel.grid.major.x = element_blank())
  VCSE_ActualPlot
  ggsave(paste0(ForReportOutputDir,"Figure4.png"), width=16,height=6,units="cm", dpi = 300)
  
# Number (%) of VCSEs by overall spending threshold (and % spending on VCSEs by same thresholds)

  VCSE_Overview %>% filter(Spending>50000000) %>% nrow(.) # n=6 over £50 million
  VCSE_Overview %>% filter(Spending>10000000) %>% nrow(.) # n=34 over £10 million
  VCSE_Overview %>% filter(Spending>1000000) %>% nrow(.) # n=242 over £1 million
  VCSE_Overview %>% filter(Spending>500000) %>% nrow(.) # n=344 over £500k million
  
  VCSE_Overview %>% filter(Spending>50000000) %>% nrow(.)/VCSE_Overview %>% nrow(.) # 0.005708849
  VCSE_Overview %>% filter(Spending>10000000) %>% nrow(.)/VCSE_Overview %>% nrow(.) # 0.03235014
  VCSE_Overview %>% filter(Spending>1000000) %>% nrow(.)/VCSE_Overview %>% nrow(.)  # 0.2302569
  VCSE_Overview %>% filter(Spending>500000) %>% nrow(.)/VCSE_Overview %>% nrow(.)   # 0.3273073
  
  VCSE_Overview %>% filter(Spending>50000000) %>% summarise(sum(Spending))/sum(VCSE_Overview$Spending) # 0.2337606
  VCSE_Overview %>% filter(Spending>10000000) %>% summarise(sum(Spending))/sum(VCSE_Overview$Spending) # 0.6061119
  VCSE_Overview %>% filter(Spending>1000000) %>% summarise(sum(Spending))/sum(VCSE_Overview$Spending)  # 0.911271
  VCSE_Overview %>% filter(Spending>500000) %>% summarise(sum(Spending))/sum(VCSE_Overview$Spending)   # 0.9482877
  

# So what about how many CCGs spend money on each VCSE?

  names(VCSE_Overview)
  
  VCSE_ByCCG=InvoicesExtractDF %>% filter(VCSE_Status==1) %>%
    group_by(SUPPLIER_STANDARD_NAME,CCGName) %>%
    summarise(Spending=sum(Spending)) %>%
    group_by(SUPPLIER_STANDARD_NAME) %>%
    summarise(CCGCount=n(), Spending=sum(Spending)) %>% arrange(desc(CCGCount))
  write.csv(VCSE_ByCCG,paste0(CSVOutputDir,"VCSE_CCGCounts.csv"),row.names=FALSE)

# Plot of VCSE Suppliers - Actual Spend against number of CCGs in which they appear
  dev.off()
  dev.new(width=14,height=14,units="cm",noRStudioGD = TRUE)   
  
  VCSE_ActualvCCGCountPlot<-ggplot(VCSE_ByCCG, aes(CCGCount, Spending)) +
    geom_point(size=1, shape=3) +
    scale_y_continuous(name="VCSE spend (£)",  
                       labels = label_number(prefix = "£", suffix = "M", scale = 1e-6, big.mark = ",")) +
    scale_x_continuous(name="Number of CCGs served by VCSEs", limits = c(0, 55), expand=c(0.005,0)) +
    geom_text(data=subset(VCSE_ByCCG,Spending > 50000000 & CCGCount < 40), aes(CCGCount, Spending,label=SUPPLIER_STANDARD_NAME),
              hjust = 0.0, nudge_x = 0.5, nudge_y = 1000000, size=2.5, colour="black") +
    geom_text(data=subset(VCSE_ByCCG,Spending > 24000000 & Spending < 50000000 & CCGCount > 5 & CCGCount < 9),
              aes(CCGCount, Spending,label=SUPPLIER_STANDARD_NAME),
              hjust = 0.0, nudge_x = 0.5, nudge_y = 1000000, size=2.5, colour="black") +
    geom_text(data=subset(VCSE_ByCCG,CCGCount > 45),aes(CCGCount, Spending,label=SUPPLIER_STANDARD_NAME),
              hjust = 0.7, nudge_x = 0, nudge_y = 2000000, size=2.5, colour="black") +
    geom_text(data=subset(VCSE_ByCCG,Spending > 20000000 & Spending < 35000000 & CCGCount > 10 & CCGCount < 20),
              aes(CCGCount, Spending,label=SUPPLIER_STANDARD_NAME),
              hjust = 0.0, nudge_x = 0, nudge_y = 2500000, size=2.5, colour="black") +
    geom_text(data=subset(VCSE_ByCCG,Spending < 25000000 & CCGCount > 27 & CCGCount < 35),
              aes(CCGCount, Spending,label=SUPPLIER_STANDARD_NAME),
              hjust = 0.5, nudge_x = 0, nudge_y = -2000000, size=2.5, colour="black") +
    geom_text(data=subset(VCSE_ByCCG,Spending < 25000000 & CCGCount > 35 & CCGCount < 40),
              aes(CCGCount, Spending,label=SUPPLIER_STANDARD_NAME),
              hjust = 0.2, nudge_x = 0, nudge_y = -4000000, size=2.5, colour="black") +
    theme(plot.background = element_rect(fill="white"),
          legend.position="none",
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.text = element_blank(),
          axis.line = element_line(color="black", linewidth =0.25),
          #axis.text.x = element_blank(),
          #axis.ticks.x = element_blank(),
          axis.text.y.left = element_text(color="black", size = 9, hjust=1, vjust = 0.5),
          axis.title.x=element_text(color="black", size=9, vjust = -2.0, hjust=0.5),
          axis.title.y.left=element_text(color="black", size=9, vjust = 2, hjust=0.5),
          panel.background = element_rect(fill="white"),
          panel.grid.major.y = element_line(colour = "black", linewidth = 0.1, linetype="dotted"),
          panel.grid.major.x = element_blank())
  VCSE_ActualvCCGCountPlot
  ggsave(paste0(ForReportOutputDir,"Figure5.png"), width=14,height=14,units="cm", dpi = 300)
 
  
# Info on CITY HEALTH CARE PARTNERSHIP CIC
  InvoicesExtractDF %>% filter(SUPPLIER_STANDARD_NAME=="CITY HEALTH CARE PARTNERSHIP CIC") %>%
    group_by(CCGName) %>% summarise(Spend=sum(Spending))
   
# Info on MARIE STOPES INTERNATIONAL
  InvoicesExtractDF %>% filter(SUPPLIER_STANDARD_NAME=="MARIE STOPES INTERNATIONAL") %>%
    group_by(CCGName) %>% summarise(Spend=sum(Spending)) %>% arrange(desc(Spend)) %>%
    print(n=50)
  
  
#----
  
# Extract summary stats reported in Report Section 7 ----
# Includes Figs 6, 8 & 9, 11 & 12 and data for Figs 7 & 10 (maps created using QGIS)

# Proportion spend on non-NHA/LA suppliers
 
  SpendSplitByCCG = InvoicesExtractDF %>% group_by(CCGName) %>%
    summarise(CCGCode = first(CCGCode), TotalSpend = sum(Spending), 
              NHSLASpend=sum(Spending[NHS==1 | LA==1]),
              NonNHSLASpend=sum(Spending[NHS==0 & LA==0]),
              NonNHSLAVCSESpend = sum(Spending[NHS==0 & LA==0 & VCSE_Status==1]),
              NonNHSLANonVCSESpend = sum(Spending[NHS==0 & LA==0 & VCSE_Status==0])) %>%
    mutate(PC_NHSLASpend = NHSLASpend/TotalSpend, PC_NonNHSLASpend = NonNHSLASpend/TotalSpend,
           PC_NonNHSLAVCSESpend = NonNHSLAVCSESpend/TotalSpend,
           PC_NonNHSLAVCSENonSpend = NonNHSLANonVCSESpend/TotalSpend,
           VCSEofNonNHSLASpend = NonNHSLAVCSESpend/NonNHSLASpend,
           NonVCSEofNonNHSLASpend = NonNHSLANonVCSESpend/NonNHSLASpend) %>% 
    arrange(desc(PC_NonNHSLASpend)) %>% mutate(ID = seq(1:nrow(.)))
  
  head(SpendSplitByCCG[,c(1,9)],3)
  tail(SpendSplitByCCG[,c(1,9)],3)

# Plot Figure 6 
  dev.off()
  dev.new(width=16,height=6,units="cm",noRStudioGD = TRUE)   #  Happy with size?
  
  LA_NHS_SpendPlot<-ggplot(SpendSplitByCCG, aes(x=ID, y=PC_NHSLASpend)) +
    geom_ribbon(ymin=-Inf, aes(ymax=PC_NHSLASpend), fill='darkolivegreen1') +
    geom_point(size=1, shape=3) + 
    scale_y_continuous(name="% CCG spend ", limits=c(0,1), 
                       breaks = c(0.0,0.2,0.4,0.6,0.8,1.0),
                       labels = scales::percent_format(accuracy = 1.0),expand=c(0,0)) +
    scale_x_discrete(name="CCGs in analytical dataset (n = 189)",expand=c(0,0)) +
    geom_label(label="Non-NHS/LA Spend",x=20,y=0.9, hjust="inward", size=3,label.size = 0.25,fill="white") +
    geom_label(label="NHS/LA Spend",x=20,y=0.6, hjust="inward", size=3,label.size = 0.25,fill="white") +
    theme(plot.background = element_rect(fill="white"),
          legend.position="none",
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.text = element_blank(),
          axis.line = element_line(color="black", linewidth =0.25),
          axis.text.x = element_blank(),
          axis.text.y.left = element_text(color="black", size = 9, hjust=1, vjust = 0.5),
          axis.title.x=element_text(color="black", size=9, vjust = -2.0, hjust=0.5),
          axis.title.y.left=element_text(color="black", size=9, vjust = 2, hjust=0.5),
          panel.background = element_rect(fill="dodgerblue2"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  LA_NHS_SpendPlot
  ggsave(paste0(ForReportOutputDir,"Figure6.png"), width=16,height=6,units="cm", dpi = 300)
  
  # csv file saved (including for QGIS maps = Fig 7 & Fig 10)
  write.csv(SpendSplitByCCG,paste0(CSVOutputDir,"CCGSpendingSplit.csv"),row.names=FALSE)
  
# Plot Figure 8
  Plot8Data = SpendSplitByCCG %>% select(ID,CCGName,PC_NHSLASpend,PC_NonNHSLAVCSESpend,
                                      PC_NonNHSLAVCSENonSpend) %>%
    mutate(Line2 = PC_NHSLASpend+PC_NonNHSLAVCSESpend)
  
  dev.off()
  dev.new(width=16,height=6,units="cm",noRStudioGD = TRUE)  

  LA_NHS_SpendPlot<-ggplot(Plot8Data, aes(x=ID)) +
  geom_ribbon(data=Plot8Data,aes(ymin=-Inf, ymax=PC_NHSLASpend), fill='darkolivegreen1') +
  geom_ribbon(data=Plot8Data,aes(ymin=PC_NHSLASpend, ymax=Line2), fill='red') +
  geom_line(aes(y=PC_NHSLASpend)) +
  geom_line(aes(y=Line2)) +
  scale_y_continuous(name="% CCG spend ", limits=c(0,1), 
                   breaks = c(0.0,0.2,0.4,0.6,0.8,1.0),
                   labels = scales::percent_format(accuracy = 1.0),expand=c(0,0)) +
  scale_x_discrete(name="CCGs in analytical dataset (n = 189)",expand=c(0,0)) +
  geom_segment(aes(x = 22, y = 0.78, xend = 40, yend = 0.72), colour = "black", linewidth=0.5) +
  geom_segment(aes(x = 70, y = 0.72, xend = 107, yend = 0.85), colour = "black", linewidth=0.5) +
  geom_label(label="Non-NHS/LA Non-VCSE Spend",x=22,y=0.95, hjust="inward", size=3,label.size = 0.25,fill="white") +
  geom_label(label="NHS/LA Spend",x=20,y=0.6, hjust="inward", size=3,label.size = 0.25,fill="white") +
  geom_label(label="Non-NHS/LA VCSE Spend",x=40,y=0.73, hjust="inward", size=3,label.size = 0.25,fill="white") +
  theme(plot.background = element_rect(fill="white"),
        legend.position="none",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_blank(),
        axis.line = element_line(color="black", size =0.25),
        axis.text.x = element_blank(),
        axis.text.y.left = element_text(color="black", size = 9, hjust=1, vjust = 0.5),
        axis.title.x=element_text(color="black", size=9, vjust = -2.0, hjust=0.5),
        axis.title.y.left=element_text(color="black", size=9, vjust = 2, hjust=0.5),
        panel.background = element_rect(fill="dodgerblue2"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
  LA_NHS_SpendPlot
  ggsave(paste0(ForReportOutputDir,"Figure8.png"), width=16,height=6,units="cm", dpi = 300)
  

  
# Figure 9
  Plot9Data = SpendSplitByCCG %>% select(ID,CCGName,VCSEofNonNHSLASpend) %>%
    arrange(desc(VCSEofNonNHSLASpend)) %>% mutate(ID = seq(1:nrow(.)))
  
  dev.off()
  dev.new(width=16,height=6,units="cm",noRStudioGD = TRUE)   #  Happy with size?
  
  NonNHSLA_SpendPlot <- ggplot(Plot9Data, aes(x=ID, y=VCSEofNonNHSLASpend)) +
  geom_ribbon(ymin=-Inf, aes(ymax=VCSEofNonNHSLASpend), fill='darkolivegreen1') +
  geom_point(size=1, shape=3) + 
  scale_y_continuous(name="% Non-NHS/LA CCG spend", limits=c(0,1), 
                  breaks = c(0.0,0.2,0.4,0.6,0.8,1.0),
                  labels = scales::percent_format(accuracy = 1.0),expand=c(0,0)) +
  scale_x_discrete(name="CCGs in analytical dataset (n = 189)",expand=c(0,0)) +
  geom_label(label="VCSE Spend",x=20,y=0.1, hjust="inward", size=3,label.size = 0.25,fill="white") +
  geom_label(label="Non-VCSE Spend",x=30,y=0.4, hjust="inward", size=3,label.size = 0.25,fill="white") +
  theme(plot.background = element_rect(fill="white"),
      legend.position="none",
      legend.title = element_blank(),
      legend.background = element_blank(),
      legend.text = element_blank(),
      axis.line = element_line(color="black", linewidth =0.25),
      axis.text.x = element_blank(),
      axis.text.y.left = element_text(color="black", size = 9, hjust=1, vjust = 0.5),
      axis.title.x=element_text(color="black", size=9, vjust = -2.0, hjust=0.5),
      axis.title.y.left=element_text(color="black", size=9, vjust = 2, hjust=0.5),
      panel.background = element_rect(fill="dodgerblue2"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank())
  NonNHSLA_SpendPlot
  ggsave(paste0(ForReportOutputDir,"Figure9.png"), width=16,height=6,units="cm", dpi = 300)
 
# Range of non-NHS/LA spending on VCSEs
   head(Plot9Data,3)
   tail(Plot9Data,4)
   
   
# Percent Overall spend on VCSE suppliers by CCG (Figure 11)
   Plot11Data = SpendSplitByCCG %>% select(ID,CCGName,TotalSpend,NonNHSLAVCSESpend) %>%
     mutate(PC_VCSESpend = NonNHSLAVCSESpend/TotalSpend) %>% 
     arrange(desc(PC_VCSESpend)) %>% mutate(ID = seq(1:nrow(.)))

   dev.off()
   dev.new(width=16,height=6,units="cm",noRStudioGD = TRUE) 
   
   PercentVCSESpendPlot<-ggplot(Plot11Data, aes(x=ID, y=PC_VCSESpend)) +
     geom_point(size=1, shape=3) +
     scale_y_continuous(name="% VCSE spend", limits=c(0,0.3), 
                        breaks = c(0.0,0.05,0.1,0.15,0.2,0.25,0.3),
                        labels = scales::percent_format(accuracy = 1.0), expand=c(0,0)) +
     scale_x_continuous(name="CCGs in analytical dataset (n = 189)", limits = c(0, 190), expand=c(0,0)) +
     theme(plot.background = element_rect(fill="white"),
           legend.position="none",
           legend.title = element_blank(),
           legend.background = element_blank(),
           legend.text = element_blank(),
           axis.line = element_line(color="black", linewidth =0.25),
           axis.text.x = element_blank(),
           axis.text.y.left = element_text(color="black", size = 9, hjust=1, vjust = 0.5),
           axis.title.x=element_text(color="black", size=9, vjust = -2.0, hjust=0.5),
           axis.title.y.left=element_text(color="black", size=9, vjust = 2, hjust=0.5),
           panel.background = element_rect(fill="white"),
           panel.grid.major.y = element_line(colour = "black", linewidth = 0.1, linetype="dotted"),
           panel.grid.major.x = element_blank())
   PercentVCSESpendPlot
   ggsave(paste0(ForReportOutputDir,"Figure11.png"), width=16,height=6,units="cm", dpi = 300)
   

# Per capita spend on VCSE suppliers by CCG (Figure 112
   
# Get 2018 pop data & join to VCSESpend Data
  PopData=read.csv(paste0(ExternalDataDir,"CCG2018Pops.csv"))
  # From SAPE21DT6b-mid-2018-ccg-syoa-estimates-unformatted.xlsx
  # from https://tinyurl.com/32mb7tcn

  PerCapitaVCSESpendData = SpendSplitByCCG %>% select(ID,CCGCode,CCGName,NonNHSLAVCSESpend) %>%
    left_join(PopData, by="CCGCode") %>% 
    mutate(PerCapitaVCSESpend = NonNHSLAVCSESpend/Mid2018Pop) %>%
    arrange(desc(PerCapitaVCSESpend)) %>% mutate(ID = seq(1:nrow(.)))

  dev.off()
  dev.new(width=16,height=6,units="cm",noRStudioGD = TRUE)   #  Happy with size?

  PerCapitaPlot <- ggplot(PerCapitaVCSESpendData, aes(ID, PerCapitaVCSESpend)) +
  geom_point(size=1, shape=3) +
  scale_y_continuous(name="VCSE spend per capita (£)",limits = c(0,450),labels = label_number(prefix = "£"), expand=c(0,0)) +
  scale_x_continuous(name="CCGs in analytical dataset (n = 189)", limits = c(0, 190), expand=c(0,0)) +
  theme(plot.background = element_rect(fill="white"),
        legend.position="none",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_blank(),
        axis.line = element_line(color="black", linewidth =0.25),
        axis.text.y.left = element_text(color="black", size = 9, hjust=1, vjust = 0.5),
        axis.title.x=element_text(color="black", size=9, vjust = -2.0, hjust=0.5),
        axis.title.y.left=element_text(color="black", size=9, vjust = 2, hjust=0.5),
        panel.background = element_rect(fill="white"),
        panel.grid.major.y = element_line(colour = "black", linewidth = 0.1, linetype="dotted"),
        panel.grid.major.x = element_blank())
  PerCapitaPlot
  ggsave(paste0(ForReportOutputDir,"Figure12.png"), width=16,height=6,units="cm", dpi = 300)
  
#----

# Extract summary stats reported in Report Section 8 ----
# Includes Figs 13 & 14

# To maintain case study site anonymity, 'Plot' values are manually tagged 
# following random jitter of actual rank order by +/-3 
  
# Figure 13 (potential case study sites - using 2018/19 CCGs)
  Fig13JitterIDs=c(3,36,41,46,54,73,100,116,135,151,162,182)
  Fig13JitterData=PerCapitaVCSESpendData[Fig13JitterIDs,]
  Figure13Data = PerCapitaVCSESpendData[-Fig13JitterIDs,]
  
  Fig13JitterData$CaseStudyShortList="CaseStudy"
  Figure13Data$CaseStudyShortList="Other"
  
  Figure13Data=rbind(Figure13Data, Fig13JitterData)
  
  dev.off()
  dev.new(width=16,height=6,units="cm",noRStudioGD = TRUE)   #  Happy with size?
  
  Figure13Plot<-ggplot(Figure13Data, aes(ID, PerCapitaVCSESpend,
        colour=CaseStudyShortList, size=CaseStudyShortList, shape=CaseStudyShortList, label=CCGName)) +
    geom_point(stroke=0.75) +
    scale_colour_manual(values=c("red","darkgrey")) +
    scale_size_manual(values = c(4,1)) +
    scale_shape_manual(values = c(3,3)) +
    geom_segment(aes(x = 47.25, y = 0, xend = 47.25, yend = 350), colour = "red", linewidth=0.5, linetype="dashed") +
    geom_segment(aes(x = 94.5, y = 0, xend = 94.5, yend = 350), colour = "red", linewidth=0.5, linetype="dashed") +
    geom_segment(aes(x = 141.75, y = 0, xend = 141.75, yend = 350), colour = "red", linewidth=0.5, linetype="dashed") +
    geom_text(label="Q1",x=23.6,y=360,size=4, colour="red") +
    geom_text(label="Q2",x=70.85,y=360, size=4, colour="red") +
    geom_text(label="Q3",x=118.1,y=360, size=4, colour="red") +
    geom_text(label="Q4",x=165.4,y=360, size=4, colour="red") +
    scale_y_continuous(name="VCSE spend per capita (£)",limits = c(0,450),labels = label_number(prefix = "£"), expand=c(0,0)) +
    scale_x_continuous(name="CCGs in analytical dataset (n = 189)", limits = c(0, 190), expand=c(0,0)) +
    theme(plot.background = element_rect(fill="white"),
          legend.position="none",
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.text = element_blank(),
          axis.line = element_line(color="black", linewidth =0.25),
          axis.text.y.left = element_text(color="black", size = 9, hjust=1, vjust = 0.5),
          axis.text.x = element_text(color="black", size = 9),
          axis.title.x=element_text(color="black", size=9, vjust = -2.0, hjust=0.5),
          axis.title.y.left=element_text(color="black", size=9, vjust = 2, hjust=0.5),
          panel.background = element_rect(fill="white"),
          panel.grid.major.y = element_line(colour = "black", linewidth = 0.1, linetype="dotted"),
          panel.grid.major.x = element_blank())
  Figure13Plot
  ggsave(paste0(ForReportOutputDir,"Figure13.png"), width=16,height=6,units="cm", dpi = 300)
  

# Now we need to aggregate data to the new CCG geography, and re-calculate PerCapita VCSE
  
# Get the lookup to link 2018/19 CCGs to 2020/21 CCGs
  CCGLookup = read.csv(paste0(ExternalDataDir,"CCGLookupForR.csv"))
  # Lookup constructed on basis of https://tinyurl.com/2p9zc39c
  # and https://tinyurl.com/ywsduztv

  names(CCGLookup)[1:2]=c("CCGCode","CCGName")

  Data2020=PerCapitaVCSESpendData %>% select(CCGCode,CCGName,NonNHSLAVCSESpend,Mid2018Pop) %>%
    left_join(CCGLookup, by="CCGCode") %>% group_by(CCGCode2020) %>% 
    summarise(CCGName2020=first(CCGName2020),VCSESpend=sum(NonNHSLAVCSESpend),
              Population=sum(Mid2018Pop)) %>% mutate(PercapitaSpend=VCSESpend/Population) %>%
    arrange(desc(PercapitaSpend))
  
  # Hack to exclude some 2020 CCGs because we have missing 2018/19 data for component CCGs
  # Exclude "E38000248" = West Sussex as we don't have Horsham
  # Exclude "E38000232" = Bradford District and Craven as we don't have Airedale
  # Exclude "E38000233" = Cheshire as we don't have Eastern Cheshire
  # (NB East Leicestershire & Rutland, City & Hackney, and Newham remained CCGs in
  #  2020 and are thus already excluded)

  Data2020 = Data2020 %>% filter(CCGCode2020!="E38000248" & CCGCode2020!="E38000232" & CCGCode2020!="E38000233")
  Data2020 = Data2020 %>% arrange(desc(PercapitaSpend)) %>% mutate(ID=seq(1:nrow(.)))
  nrow(Data2020) # So we are left with 129 CCGs (out of 135 extant in 2020)

  # To maintain case study site anonymity, 'Plot' values are manually tagged 
  # following random jitter of actual rank order by +/-3 
  
  # Figure 14 (potential case study sites - using 2018/19 CCGs)
  Fig14JitterIDs=c(8,29,31,91,95,111)
  Fig14JitterData=Data2020[Fig14JitterIDs,]
  Figure14Data = Data2020[-Fig14JitterIDs,]
  
  Fig14JitterData$CaseStudy="CaseStudy"
  Figure14Data$CaseStudy="Other"
  
  Figure14Data=rbind(Figure14Data, Fig14JitterData)
  
  dev.off()
  dev.new(width=16,height=6,units="cm",noRStudioGD = TRUE)   #  Happy with size?

  Figure14Plot<-ggplot(Figure14Data, aes(ID, PercapitaSpend, colour=CaseStudy, size=CaseStudy, shape=CaseStudy)) +
  geom_point(stroke=0.75) +
  scale_colour_manual(values=c("red","darkgrey")) +
  scale_size_manual(values = c(4,1)) +
  scale_shape_manual(values = c(3,3)) +
  geom_segment(aes(x = 32.5, y = 0, xend = 32.5, yend = 350), colour = "red", linewidth=0.5, linetype="dashed") +
  geom_segment(aes(x = 65.5, y = 0, xend = 65.5, yend = 350), colour = "red", linewidth=0.5, linetype="dashed") +
  geom_segment(aes(x = 97.5, y = 0, xend = 97.5, yend = 350), colour = "red", linewidth=0.5, linetype="dashed") +
  geom_text(label="Q1",x=16,y=360,size=4, colour="red") +
  geom_text(label="Q2",x=49,y=360, size=4, colour="red") +
  geom_text(label="Q3",x=81,y=360, size=4, colour="red") +
  geom_text(label="Q4",x=113,y=360, size=4, colour="red") +
  geom_text(label="Case Study 1",x=7,y=140, hjust=0, size=3, colour="black") +
  geom_text(label="Case Study 2",x=26,y=120, hjust=0, size=3, colour="black") +
  geom_text(label="Case Study 3",x=31,y=80, hjust=0, size=3, colour="black") +
  geom_text(label="Case Study 4",x=81,y=85, hjust=0, size=3, colour="black") +
  geom_text(label="Case Study 5",x=94,y=55, hjust=0, size=3, colour="black") +
  geom_text(label="Case Study 6",x=111,y=40, hjust=0, size=3, colour="black") +
  scale_y_continuous(name="VCSE spend per capita (£)",limits = c(0,450),labels = label_number(prefix = "£"), expand=c(0,0)) +
  scale_x_continuous(name="2020 CCGs for which complete 2018/19 data available (n = 129)", limits = c(0, 130), expand=c(0,0)) +
  theme(plot.background = element_rect(fill="white"),
        legend.position="none",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_blank(),
        axis.line = element_line(color="black", linewidth =0.25),
        axis.text.y.left = element_text(color="black", size = 9, hjust=1, vjust = 0.5),
        axis.title.x=element_text(color="black", size=9, vjust = -2.0, hjust=0.5),
        axis.title.y.left=element_text(color="black", size=9, vjust = 2, hjust=0.5),
        panel.background = element_rect(fill="white"),
        panel.grid.major.y = element_line(colour = "black", linewidth = 0.1, linetype="dotted"),
        panel.grid.major.x = element_blank())
  Figure14Plot
  ggsave(paste0(ForReportOutputDir,"Figure14.png"), width=16,height=6,units="cm", dpi = 300)

#END
