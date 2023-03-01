<!--
Any comments?
-->
## CCG Spend on VCSEs, 2018/19:  Data & Analysis

As part of an NIHR-funded project on *Commissioning, Co-commissioning and Being Commissioned; the NHS and Third Sector Organisations* ([research protocol](https://tinyurl.com/tvpx3ty2)) a nearly complete set of Clinical Commissioning Group (CCG) “over-threshold” expenditure data for 2018/19 was acquired, cleaned and analysed.  

These data were published in response to [HM Treasury’s 2013 guidance](https://tinyurl.com/4m9p23hp) that government bodies should make available, with few exceptions, "all individual invoices, grant payments, expense payments or other such transactions that are over £25,000". These CCG accounts are part of a vast and rapidly increasing array of potentially invaluable expenditure data published in response to the government's transparency agenda.

Our project, more conveniently known as the ***3SC (3rd Sector Commissioning) Project***, was particularly concerned with investigating in how CCGs engage with Voluntary, Community and Social Enterprises (VCSEs) in the commissioning of services for three ‘tracer’ care groups; social prescribing for older people, hospices, and people with learning disability and complex behavioural needs.

The ***3SC Project*** used the expenditure data to (a) guide the selection of a ‘maximum-variety’ sample of CCG study sites in terms of their commissioning of VCSEs suppliers, (b) understand in detail the scale and nature of CCG commissioning of VCSEs in the selected study sites, and (c) gain a broader perspective on patterns of VCSE commissioning across the country as a whole.

![NHS/LA, VCSE & non-VCSE spending by CCG](./05_Outputs/ForReport/Figure8.png)

This ***3SC Project*** *github* repository comprises:

- The ‘raw’ monthly CCG accounts for the 189 (of the then 195) CCGs for which we were able to obtain satisfactory expenditure data (as CCG-specific zipfiles in the *./01_RawCCGAccounts* directory).
- A set of CCG-specific Excel files manually constructed from the above data and saved in the *./02_CollatedCCGCAccounts* directory). 
- A single collated file containing all data from the above 189 CCG expenditure files, with a minor clarification of shared hospice names (saved as *01_CollatedInvoices.RDS* and *01_CollatedInvoices.csv* in the *./04_Data/processed* directory).
- An extract from the above collated expenditure datafile listing all named suppliers (*01_UniqueSuppliers.csv* in the *./04_Data/processed* directory) and a manually-updated version of that file with details on all suppliers (*01_SuppliersInfo_FINAL.csv*).  As detailed in the accompanying report (*CCG_VCSE_Spend_Report.docx*), this includes information on Charity Commission and Company House registration (where available) along with a classification of each supplier's service sector and its VCSE status.
- A single dataset combining the expenditure and supplier datasets (*02_FullCollatedDataset.RDS*). This includes all records (n=689,536), of which 622,514 were for for payments made and 67,022 were for income received.
- A cleaned dataset of all valid invoices for payments of £25k and above from across the 189 CCGs (saved as *02_InvoicesExtractDF.csv* and *02_InvoicesExtractDF.RDS* in the *./04_Data/processed* directory). This constitutes our ***analytical dataset*** and comprises 226,138 records accounting for ***£70.525 billion*** of CCG expenditure. 
- A report detailing (a) how we obtained and cleaned the expenditure data, (b) how suppliers were categorised (including whether or not VCSE), and (c) what the data reveal regarding CCG-level variation in spending on VCSEs in 2018/18 (*./CCG_VCSE_Spend_Report.docx*). ***Users are strongly advised to view this report before using any of the data published here.***
- A series of data-wrangling and analytical R scripts (in the *./03_Rscripts* directory) - see below.
- All .csv output files (in the *./05_Outputs/CSVfiles* directory) created by running the above scripts on the analytical dataset.
- There are also population and lookup files used by the R scripts (in the *./04_Data/external directory*) and a variety of .png files produced by the R scripts for inclusion in the accompanying report and this README document (in the *./05_Outputs/ForReport* directory).


### Data Analysis and Outputs

The R scripts are fully commented and should guide any replication and/or subsequent use of the CCG expenditure data for 2018/19.  As detailed in the accompanying report (*./CCG_VCSE_Spend_Report.docx*) it is not now possible to download most of the original accounts from the internet - hence their inclusion in the *./01_RawCCGAccounts* directory on this repository.

Nor is it possible to provide a 'Reproducible Analytical Pipeline' regarding the identification and classification of suppliers. This was based on automated and manual searches of Company House and Charity Commission registers, as well as a huge variety of public-facing supplier websites.

However, all cleaning/manipulation of the expenditure data, culminating in the construction of the project's ***analytical dataset*** was undertaken using R, as was the analysis used to both inform the accompanying report and generate a variety of summary .csv files. These describe CCG-level variation in commissioning, including of VCSE suppliers, and list all unique suppliers identified across the 189 CCGs.

***The R scripts and outputs are as follows:***

- ***00_CollateAndCombineCCGData.R***: collates the 189 CCG files into a single file (saved as *01_CollatedInvoices.RDS* and *01_CollatedInvoices.csv*) and extracts the listing of all supplier names (*01_UniqueSupplierNames.csv*) that was then used to search Charity Commission and Company House registers as well as supplier websites from across the internet more widely.  It is unlikely that users will find this useful.

- ***01_CleanCCGDataAndExtractAnalyticalDataset.R***: combines the 'invoices' data (*01_CollatedInvoices.RDS*) with what is known about the suppliers (*01_SuppliersInfo_FINAL.csv*), saves the full dataset (*02_FullCollatedDataset.RDS*), and then extracts our ***analytical dataset*** (as *02_InvoicesExtractDF.RDS* and *02_InvoicesExtractDF.csv*). This comprises all 'valid' invoices >= £25k from across the 189 CCGs.  This too is unlikely to be of particular interest.

- ***02_Analysis.R*** (and its outputs) will be of more relevance to most users. Using the full dataset (*02_FullCollatedDataset.RDS*) and, more particularly, the ***analytical dataset*** (*02_InvoicesExtractDF.RDS*), it implements all analysis, including plot generation, underpinning the accompanying report (*CCG_VCSE_Spend_Report.docx*) and this *github* README. It also produces the following summary .csv files (saved in the *./05_Outputs/CSVfiles* directory).

  * **ThresholdSplitByCCG.csv**: For each CCG, the number and proportion of invoices <£25k and >=£25k
  * **SupplierCategorySplitByCCG.csv**: For each CCG, expenditure on suppliers categorised as 'Executor', 'Government', GP Services', 'Hospice', 'Local Authority', 'Local Medical Committee or Local Pharmaceutical Committee', 'NHS', 'Other', 'Pharmacy or Optician', 'Police or Fire Service', 'School', 'Solicitor', 'University'. 'Total' CCG expenditure is also given alongside  expenditure on each of the foregoing categories as a proportion of total CCG spending.
  * **VCSESuppliers.csv**: A list of all unique suppliers for whom invoices >=£25k appear in the 189 CCGs (n=1,051) with a) number of invoices and b) total value of invoices.
   - **VCSE_CCGCounts.csv**: All suppliers with a count of the number of CCGs from which they received payments of >=£25k.
  * **CCGSpendingSplit.csv**: For each CCG, *TotalSpend*, the spend on NHS and LA suppliers (*NHSLASpend*), on non-NHS/LA suppliers (*NonNHSLASpend*), on non-NHS/LA VCSE suppliers (*NonNHSLAVCSESpend*), and on nonNHS/LA non-VCSE suppliers(*NonNHSLANonVCSESpend*). Then each of these as a proportion of total spend, and finally, VCSE spend as a proportion of non-NHS/LA spending (*VCSEofNonNHSLASpend*) and non-VCSE spend as a proportion of non-NHS/LA spending (*NonVCSEofNonNHSLASpend*).

The accompanying report discusses these outputs and illustrates many of the CCG-summary statistics graphically. Users will, of course, be able to create tables to suit their particular needs using the data provided.


## Contacts

For further information on the *Commissioning, Co-commissioning and Being Commissioned; the NHS and Third Sector Organisations* project contact Mark Exworthy (m.exworthy@bham.ac.uk) or Rod Sheaff (rod.sheaff@plymouth.ac.uk).

For further information about this github and the CCG expenditure data, contact Alex.Gibson@plymouth.ac.uk.

***15/02/2013***
