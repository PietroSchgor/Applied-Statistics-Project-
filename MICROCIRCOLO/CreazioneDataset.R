library(readxl)
library(openxlsx)

baseline <- read_excel("dataset_microcircolo.xlsx")
data_micro2 <- read_excel("dataset_microcircolo.xlsx",sheet ="2mis_h&n")
data_micro3 <- read_excel("dataset_microcircolo.xlsx",sheet ="3mis_h&n")
data_micro4 <- read_excel("dataset_microcircolo.xlsx",sheet ="4mis_h&n")
data_micro5 <- read_excel("dataset_microcircolo.xlsx",sheet ="5mis_h&n")
data_micro6 <- read_excel("dataset_microcircolo.xlsx",sheet ="6mis_h&n")
data_micro7 <- read_excel("dataset_microcircolo.xlsx",sheet ="7mis_h&n")

data_micro2 <- merge(data_micro2, baseline, by = "Name")
data_micro3 <- merge(data_micro3, baseline, by = "Name")
data_micro4 <- merge(data_micro4, baseline, by = "Name")
data_micro5 <- merge(data_micro5, baseline, by = "Name")
data_micro6 <- merge(data_micro6, baseline, by = "Name")
data_micro7 <- merge(data_micro7, baseline, by = "Name")

dataset <- rbind(data_micro2, data_micro3, data_micro4, data_micro5, 
                 data_micro6, data_micro7)
pazienti <- read_excel("dataset_paz.xlsx")
dosi_lingua <- read_excel("dataset_paz.xlsx",sheet ="dosi_lingua")
paz_lingua <- merge(pazienti, dosi_lingua, by.x = "Name", by.y = "ID")
frazioni <- read_excel("frazioni_HN.xlsx")
paz_frazioni <- merge(paz_lingua, frazioni, by.x = "Name", by.y = "ID")
dataset <- merge(dataset, paz_frazioni, by = "Name")

# Creazione di un nuovo workbook
wb <- createWorkbook()

# Aggiunta di un foglio di lavoro (worksheet)
addWorksheet(wb, "Foglio1")

# Scrittura del data frame nel foglio di lavoro
writeData(wb, "Foglio1", dataset)

# Salvataggio del file Excel
saveWorkbook(wb, "full.xlsx", overwrite = TRUE)

