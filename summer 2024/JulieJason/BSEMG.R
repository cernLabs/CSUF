data = read.csv("output_cleaned.csv", header=TRUE)
head(data)

# Baseline deltoid activity <E1>
E1_start = as.numeric(rownames(data[data$Event == "baseline deltoids",]))
E1_end = as.numeric(rownames(data[data$Event == "end of baseline deltoids",]))
Set1 = data[E1_start: E1_end,]

# Deltoid with pectoral activity <E2>
E2_start = as.numeric(rownames(data[data$Event == "internal rotation start pecs",]))
E2_end = as.numeric(rownames(data[data$Event == "internal rotation end pecs",]))
Set2 =data[E2_start: E2_end,]

# Deltoid with revisited pectoral activity <E3>
E3_start = as.numeric(rownames(data[data$Event == "int rot w resistance pecs",]))
E3_end = as.numeric(rownames(data[data$Event == "end of int rotation pecs w resi",]))
Set3 = data[E3_start: E3_end,]

# Shoulder external rotation <E4>
E4_start = as.numeric(rownames(data[data$Event == "ext rot ",]))
E4_end = as.numeric(rownames(data[data$Event == "ext rot end",]))
Set4 =data[E4_start: E4_end,]

# Testing on/off detection
library(biosignalEMG)
newdata = rbind(Set2,Set3)
ts.plot(newdata[,2])
