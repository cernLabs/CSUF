
### Some practice with bootstrapping from Mike

# # read data
# Kine.dat = read.csv("Kinematic_Data_EMG.csv",header = T)
# # split into books 
# Kine.df = split(Kine.dat, Kine.dat$PID)
# 
# # initialize the dataframes (we will store our bootstraps in)
# duration.bs <- onset.bs <- step.init.bs <- full.step.bs <- step.height.bs <- data.frame(NULL)
# 
# # initialize the xbar vectors
# xbar.duration <- xbar.onset <- xbar.step.init <- xbar.full.step <-  xbar.step.height <- c()
# 
# # begin loop
# for(i in 1:length(Kine.df)){  # loop through each subject
#   for(j in 1:5000){  # 5000 bootstrap samples each
#     # sample indices
#     k = sample(1:18, replace = T, size = 18)  # 18 trials from 3 blocks
#     # take means of the kth samples and store into the xbars
#     take = (Kine.df[[i]][["Weight_Transfer_Duration"]])[k]
#     xbar.duration[j] = mean(take)
#     take = (Kine.df[[i]][["Weight_Transfer_Onset_Initiation_Time"]])[k]
#     xbar.onset[j] = mean(take)
#     take = (Kine.df[[i]][["Step_Initiation_Time"]])[k]
#     xbar.step.init[j] = mean(take)
#     take = (Kine.df[[i]][["Full_Step_Length"]])[k]
#     xbar.full.step[j] = mean(take)
#     take = (Kine.df[[i]][["Step_Height"]])[k]
#     xbar.step.height[j] = mean(take)
#   }
#   # fill the BS data.frames row-by-row
#   duration.bs = rbind(duration.bs,xbar.duration)
#   onset.bs = rbind(onset.bs,xbar.onset)
#   step.init.bs = rbind(step.init.bs,xbar.step.init)
#   full.step.bs = rbind(full.step.bs,xbar.full.step)
#   step.height.bs = rbind(step.height.bs,xbar.step.height)
# }
# # attach corresponding Patient IDs for each row in every BS data.frame
# duration.bs = cbind(names(Kine.df),duration.bs)
# onset.bs = cbind(names(Kine.df),onset.bs)
# step.init.bs = cbind(names(Kine.df),step.init.bs)
# full.step.bs = cbind(names(Kine.df),full.step.bs)
# step.height.bs = cbind(names(Kine.df),step.height.bs)


# demo <- read.csv('Demographics_.csv')
# age <- factor(demo$Group...1.young..2.older.adults.)
# sex <- factor(demo$Sex..1.male...2.female.)
# aov.data <- data.frame("grip.strength" = demo$Handgrip.Strenght.Test..kg.f.,
#                        age,
#                        sex)
# anova_model <- aov(grip.strength ~ age * sex, data=aov.data)
# age.cont <- demo$Age..years.old.
# lin_model <- lm(grip.strength ~ age.cont * sex, data = aov.data)
# summary(lin_model)
# summary(anova_model)
# library(tidyverse)
# ggplot(aov.data, aes(x=age, y=grip.strength, fill=sex)) +
#   stat_summary(fun=mean, geom='bar', position='dodge') +
#   stat_summary(fun.data=mean_se, geom='errorbar', position=position_dodge(width = 0.9), width=0.2) +
#   theme_minimal()

######################################################
#####   EDA/Data Summary and Data Visualization  #####
######################################################

## Load necessary libraries
library(readxl)
library(tidyverse)

## Load data from excel files provided by client
demographics <- read_excel("Demographics_.xlsx", sheet = "Sheet1")
muscle_data <- read_excel("Muscle_Data.xlsx", sheet = "Muscledata_")
kinematic_data <- read_excel("Kinematic_Data_EMG.xlsx", sheet = "Kinematic")

## Clean up datasets before merging them

# Standardize subject IDs across the data files 
muscle_data$ParticipantID[1:9] <- gsub("Pepper", "Pepper0",    # NOT ALL NEED A LEADING 0
                                  muscle_data$ParticipantID[1:9])

# Drop EMG columns from kinematic data for now? (Can undo this if we get this data)
kinematic_data_clean <- kinematic_data %>% select(-matches("Electric Potential"))

# get rid of 0 in Step Type
kinematic_data_clean$`Step_Type (1 Lat :  2 Fwd : 3 Bwd)` <- 
  gsub(0,1,kinematic_data_clean$`Step_Type (1 Lat :  2 Fwd : 3 Bwd)`)


## Make muscle data into Z-scores for composites

# Thickness measures 
muscle_data$Z_VL_Thickness <- (muscle_data$`VL_Thickness(cm)` - mean(muscle_data$`VL_Thickness(cm)`, na.rm =TRUE))/sd(muscle_data$`VL_Thickness(cm)`, na.rm =TRUE)
muscle_data$Z_VI_Thickness <- (muscle_data$`VI_Thickness(cm)` - mean(muscle_data$`VI_Thickness(cm)`, na.rm =TRUE))/sd(muscle_data$`VI_Thickness(cm)`, na.rm =TRUE)
muscle_data$Z_BF_Thickness <- (muscle_data$`BF_Thickness(cm)` - mean(muscle_data$`BF_Thickness(cm)`, na.rm =TRUE))/sd(muscle_data$`BF_Thickness(cm)`, na.rm =TRUE)
muscle_data$Z_GM_Thickness <- (muscle_data$`GM_Thickness(cm)` - mean(muscle_data$`GM_Thickness(cm)`, na.rm =TRUE))/sd(muscle_data$`GM_Thickness(cm)`, na.rm =TRUE)
muscle_data$Z_TFL_Thickness <- (muscle_data$`TFL_Thickness(cm)` - mean(muscle_data$`TFL_Thickness(cm)`, na.rm =TRUE))/sd(muscle_data$`TFL_Thickness(cm)`, na.rm =TRUE)
muscle_data$Z_MG_Thickness <- (muscle_data$`MG_Thickness(cm)` - mean(muscle_data$`MG_Thickness(cm)`, na.rm =TRUE))/sd(muscle_data$`MG_Thickness(cm)`, na.rm =TRUE)
muscle_data$Z_SOL_Thickness <- (muscle_data$`Sol_Thickness(cm)` - mean(muscle_data$`Sol_Thickness(cm)`, na.rm =TRUE))/sd(muscle_data$`Sol_Thickness(cm)`, na.rm =TRUE)
muscle_data$Z_TA_Thickness <- (muscle_data$`TA_Thickness(cm)` - mean(muscle_data$`TA_Thickness(cm)`, na.rm =TRUE))/sd(muscle_data$`TA_Thickness(cm)`, na.rm =TRUE)

# Quality measures  (*Note: reverse scored, higher au = worse)
muscle_data$Z_VL_Quality <- -(muscle_data$`VL_Quality(au)` - mean(muscle_data$`VL_Quality(au)`,na.rm =TRUE))/sd(muscle_data$`VL_Quality(au)`,na.rm =TRUE)
muscle_data$Z_VI_Quality <- -(muscle_data$`VI_Quality(au)` - mean(muscle_data$`VI_Quality(au)`,na.rm =TRUE))/sd(muscle_data$`VI_Quality(au)`,na.rm =TRUE)
muscle_data$Z_BF_Quality <- -(muscle_data$`BF_Quality(au)` - mean(muscle_data$`BF_Quality(au)`,na.rm =TRUE))/sd(muscle_data$`BF_Quality(au)`,na.rm =TRUE)
muscle_data$Z_GM_Quality <- -(muscle_data$`GM_quality(au)` - mean(muscle_data$`GM_quality(au)`,na.rm =TRUE))/sd(muscle_data$`GM_quality(au)`,na.rm =TRUE)
muscle_data$Z_TFL_Quality <- -(muscle_data$`TFL_Quality(au)` - mean(muscle_data$`TFL_Quality(au)`,na.rm =TRUE))/sd(muscle_data$`TFL_Quality(au)`,na.rm =TRUE)
muscle_data$Z_MG_Quality <- -(muscle_data$`MG_Quality(au)` - mean(muscle_data$`MG_Quality(au)`,na.rm =TRUE))/sd(muscle_data$`MG_Quality(au)`,na.rm =TRUE)
muscle_data$Z_SOL_Quality <- -(muscle_data$`SOL_quality(au)` - mean(muscle_data$`SOL_quality(au)`,na.rm =TRUE))/sd(muscle_data$`SOL_quality(au)`,na.rm =TRUE)
muscle_data$Z_TA_Quality <- -(muscle_data$`TA_quality(au)` - mean(muscle_data$`TA_quality(au)`,na.rm =TRUE))/sd(muscle_data$`TA_quality(au)`,na.rm =TRUE)

# Stiffness measures (Only available for select muscles, (*Note: reverse scored, higher m/s = worse))
muscle_data$Z_VL_Stiffness <- -(muscle_data$`SWVL(m/s)` - mean(muscle_data$`SWVL(m/s)`, na.rm =TRUE))/sd(muscle_data$`SWVL(m/s)`, na.rm =TRUE)
muscle_data$Z_BF_Stiffness <- -(muscle_data$`SWBF(m/s)` - mean(muscle_data$`SWBF(m/s)`, na.rm =TRUE))/sd(muscle_data$`SWBF(m/s)`, na.rm =TRUE)
muscle_data$Z_TFL_Stiffness <- -(muscle_data$`SWTFL(m/s)` - mean(muscle_data$`SWTFL(m/s)`, na.rm =TRUE))/sd(muscle_data$`SWTFL(m/s)`, na.rm =TRUE)
muscle_data$Z_MG_Stiffness <- -(muscle_data$`SWMG(m/s)` - mean(muscle_data$`SWMG(m/s)`, na.rm =TRUE))/sd(muscle_data$`SWMG(m/s)`, na.rm =TRUE)
muscle_data$Z_TA_Stiffness <- -(muscle_data$`SWTA(m/s)` - mean(muscle_data$`SWTA(m/s)`, na.rm =TRUE))/sd(muscle_data$`SWTA(m/s)`, na.rm =TRUE)

# Composite measures
muscle_data$Z_VL <- rowMeans(cbind(muscle_data$Z_VL_Quality , muscle_data$Z_VL_Thickness, muscle_data$Z_VL_Stiffness), na.rm = TRUE)
muscle_data$Z_VI <- rowMeans(cbind(muscle_data$Z_VI_Quality , muscle_data$Z_VI_Thickness), na.rm = TRUE)
muscle_data$Z_BF <- rowMeans(cbind(muscle_data$Z_BF_Quality , muscle_data$Z_BF_Thickness, muscle_data$Z_BF_Stiffness), na.rm = TRUE)
muscle_data$Z_GM <- rowMeans(cbind(muscle_data$Z_GM_Quality , muscle_data$Z_GM_Thickness), na.rm = TRUE)
muscle_data$Z_TFL <- rowMeans(cbind(muscle_data$Z_TFL_Quality , muscle_data$Z_TFL_Thickness, muscle_data$Z_TFL_Stiffness), na.rm = TRUE)
muscle_data$Z_MG <- rowMeans(cbind(muscle_data$Z_MG_Quality , muscle_data$Z_MG_Thickness, muscle_data$Z_MG_Stiffness), na.rm = TRUE)
muscle_data$Z_SOL <-rowMeans(cbind(muscle_data$Z_SOL_Quality , muscle_data$Z_SOL_Thickness), na.rm = TRUE)
muscle_data$Z_TA <- rowMeans(cbind(muscle_data$Z_TA_Quality , muscle_data$Z_TA_Thickness, muscle_data$Z_TA_Stiffness), na.rm = TRUE)

## Merge all datasets into one

# Sort demographic data by Subject before left joining
demographics <- arrange(demographics, Subject) # first make sure these are in order

# Merge into one by left joining dataframes
merged_df <- demographics %>% 
  left_join(muscle_data, by = c("Subject" = "ParticipantID")) %>%
  left_join(kinematic_data_clean, by = c("Subject" = "PID"))

## Find missing data
missing_summary <- colSums(is.na(merged_df))

# Display columns with missing values
missing_summary[missing_summary>0]


## Try some summary statistics for data exploration and visualization to follow
##  --->>> Need to determine what is relevant for summary stats!

summary_stats <- merged_df %>%
  summarise(
        Mean_Age = mean(`Age (years old)`, na.rm = TRUE),
        Mean_Step_Length = mean(Full_Step_Length, na.rm = TRUE),
        Mean_Step_Height = mean(Step_Height, na.rm = TRUE),
        Mean_Step_Initiation_Time = mean(Step_Initiation_Time, na.rm = TRUE),
        Mean_Weight_Transfer_Duration = mean(Weight_Transfer_Duration, na.rm = TRUE)
      )
    
print(summary_stats)


## Try some visualizations
##  --->>> Need to determine what is relevant for summary stats!

# # step length by age group
# ggplot(merged_df, aes(x = as.factor(`Group ( 1-young, 2-older adults)`), y = Full_Step_Length)) +
#   geom_boxplot(fill = "lightblue") +
#   labs(x = "Age Group (1 = Younger, 2 = Older)", y = "Full Step Length (m)", title = "Step Length by Age Group") +
#   theme_minimal()
# 
# # step height by age group
# ggplot(merged_df, aes(x = as.factor(`Group ( 1-young, 2-older adults)`), y = Step_Height)) +
#   geom_boxplot(fill = "lightcoral") +
#   labs(x = "Age Group (1 = Younger, 2 = Older)", y = "Step Height (m)", title = "Step Height by Age Group") +
#   theme_minimal()

# # distribution of sample vastus lateralis thickness
# ggplot(merged_df, aes(x = `VL_Thickness(cm)`)) +
#   geom_histogram(binwidth = 0.25, fill = "steelblue", color = "black") +
#   labs(x = "Vastus Lateralis Thickness (cm)", y = "Count", title = "Distribution of VL Thickness") +
#   theme_minimal()



### Questions about stepping kinematics for visual EDA

# Step length by age group
ggplot(merged_df, aes(x = as.factor(`Group ( 1-young, 2-older adults)`), 
                      y = Full_Step_Length, 
                      fill = as.factor(`Step_Type (1 Lat :  2 Fwd : 3 Bwd)`))) +
  geom_boxplot(na.rm = TRUE) +
  labs(x = "Age Group (1 = Younger, 2 = Older)", 
       y = "Full Step Length (m)", 
       title = "Step Length by Age Group") +
  theme_minimal() +
  scale_fill_discrete(labels = c("Lateral Step", 
                                 "Forward Step", 
                                 "Backward Step")) + # Custom legend labels
  scale_x_discrete(name = "Age Group", labels = c("Young","Older")) +
  theme(legend.title = element_blank())
ggsave(file="test.svg") # Save to file
  
# Step height by age group
ggplot(merged_df, aes(x = as.factor(`Group ( 1-young, 2-older adults)`), 
                      y = Step_Height, 
                      fill = as.factor(`Step_Type (1 Lat :  2 Fwd : 3 Bwd)`))) +
  geom_boxplot() +
  labs(x = "Age Group (1 = Younger, 2 = Older)", 
       y = "Step Height (m)", 
       title = "Step Height by Age Group") +
  theme_minimal() +
  scale_fill_discrete(labels = c("Lateral Step", 
                                 "Forward Step", 
                                 "Backward Step")) + # Custom legend labels
  scale_x_discrete(name = "Age Group", labels = c("Young","Older")) +
  theme(legend.title = element_blank())

# Step initiation time by age group
ggplot(merged_df, aes(x = as.factor(`Group ( 1-young, 2-older adults)`), 
                      y = Step_Initiation_Time, 
                      fill = as.factor(`Step_Type (1 Lat :  2 Fwd : 3 Bwd)`) )) +
  geom_boxplot() +
  labs(x = "Age Group (1 = Younger, 2 = Older)", 
       y = "Time to Step Initiation (s)", 
       title = "Step Initiation Time by Age Group") +
  theme_minimal() +
  scale_fill_discrete(labels = c("Lateral Step", 
                                 "Forward Step", 
                                 "Backward Step")) + # Custom legend labels
  scale_x_discrete(name = "Age Group", labels = c("Young","Older")) +
  theme(legend.title = element_blank())

# Weight Transfer Onset Initiation Time by age group
ggplot(merged_df, aes(x = as.factor(`Group ( 1-young, 2-older adults)`), 
                      y = Weight_Transfer_Onset_Initiation_Time, 
                      fill = as.factor(`Step_Type (1 Lat :  2 Fwd : 3 Bwd)`))) +
  geom_boxplot() +
  labs(x = "Age Group (1 = Younger, 2 = Older)", 
       y = "Weight Transfer Onset Initiation Time (s)", 
       title = "Weight Transfer Onset Initiation Time by Age Group") +
  theme_minimal() +
  scale_fill_discrete(labels = c("Lateral Step", 
                                 "Forward Step", 
                                 "Backward Step")) + # Custom legend labels
  scale_x_discrete(name = "Age Group", labels = c("Young","Older")) +
  theme(legend.title = element_blank())

# Weight Transfer Duration by age group
ggplot(merged_df, aes(x = as.factor(`Group ( 1-young, 2-older adults)`), 
                      y = Weight_Transfer_Duration, 
                      fill = as.factor(`Step_Type (1 Lat :  2 Fwd : 3 Bwd)`))) +
  geom_boxplot() +
  labs(x = "Age Group (1 = Younger, 2 = Older)", 
       y = "Weight Transfer Duration (s)", 
       title = "Weight Transfer Duration by Age Group") +
  theme_minimal()+
  scale_fill_discrete(labels = c("Lateral Step", 
                                 "Forward Step", 
                                 "Backward Step")) + # Custom legend labels
  scale_x_discrete(name = "Age Group", labels = c("Young","Older")) +
  theme(legend.title = element_blank())

### Visualization for questions about stepping kinematics vs. muscle characteristics for visual EDA

##### Prepare data, then make the plots #####

### STEP LENGTH ###

## Lateral, Step Leg, GM ##
summary_df <- merged_df %>%
  filter(`EMG_Leg (1 Stepping: 2 Stance)` == 1) %>%  # Filter for stepping leg trials
  filter(`Step_Type (1 Lat :  2 Fwd : 3 Bwd)` == 1) %>% # Filter for lateral steps
  group_by(Subject) %>%  # Group by Subject
  summarize(
    muscleGM = mean(Z_GM),  # Z_GM for each subject
    stepLength = mean(Full_Step_Length),  # Mean of Full_Step_Length for each subject
    ageGroup = mean(`Group ( 1-young, 2-older adults)`),  # Age group for each subject
    .groups = "drop"
  )

# Plot: Muscle qualities in GM x LATERAL STEPPING Leg x Old vs. Young
ggplot(summary_df, aes(x = muscleGM, y = stepLength, color = as.factor(ageGroup), shape = as.factor(ageGroup))) +
  geom_point(size = 2.5) +  # Adjust point size
  theme_minimal() +
  scale_color_hue(labels = c("Young", "Older")) +  # Custom legend labels
  scale_shape_manual(values = c(16,17), labels = c("Young", "Older")) +
  theme(legend.title = element_blank()) +
  labs(x = "Gluteus Medius composite (Z_GM) of STEPPING leg", 
       y = "Step Length (m)",
       title = "Step Length vs. GM Structure/Function",
       subtitle = "in LATERAL steps")  # Proper labels

## Lateral, Stance Leg, GM ##
summary_df <- merged_df %>%
  filter(`EMG_Leg (1 Stepping: 2 Stance)` == 2) %>%  # Filter for stepping leg trials
  filter(`Step_Type (1 Lat :  2 Fwd : 3 Bwd)` == 1) %>% # Filter for lateral steps
  group_by(Subject) %>%  # Group by Subject
  summarize(
    muscleGM = mean(Z_GM),  # Z_GM for each subject
    stepLength = mean(Full_Step_Length),  # Mean of Full_Step_Length for each subject
    ageGroup = mean(`Group ( 1-young, 2-older adults)`),  # Age group for each subject
    .groups = "drop"
  )

# Plot: Muscle qualities in GM x LATERAL STANCE Leg x Old vs. Young
ggplot(summary_df, aes(x = muscleGM, y = stepLength, color = as.factor(ageGroup), shape = as.factor(ageGroup))) +
  geom_point(size = 2.5) +  # Adjust point size
  theme_minimal() +
  scale_color_hue(labels = c("Young", "Older")) +  # Custom legend labels
  scale_shape_manual(values = c(16,17), labels = c("Young", "Older")) +
  theme(legend.title = element_blank()) +
  labs(x = "Gluteus Medius composite (Z_GM) of STANCE leg", 
       y = "Step Length (m)",
       title = "Step Length vs. GM Structure/Function",
       subtitle = "in LATERAL steps")  # Proper labels

## Forward, Step Leg, VL ##
summary_df <- merged_df %>%
  filter(`EMG_Leg (1 Stepping: 2 Stance)` == 1) %>%  # Filter for stepping leg trials
  filter(`Step_Type (1 Lat :  2 Fwd : 3 Bwd)` == 2) %>% # Filter for lateral steps
  group_by(Subject) %>%  # Group by Subject
  summarize(
    muscleVL = mean(Z_VL),  # Z_VL for each subject
    stepLength = mean(Full_Step_Length),  # Mean of Full_Step_Length for each subject
    ageGroup = mean(`Group ( 1-young, 2-older adults)`),  # Age group for each subject
    .groups = "drop"
  )

# Plot: Muscle qualities in VL x FORWARD STEPPING Leg x Old vs. Young
ggplot(summary_df, aes(x = muscleVL, y = stepLength, color = as.factor(ageGroup), shape = as.factor(ageGroup))) +
  geom_point(size = 2.5) +  # Adjust point size
  theme_minimal() +
  scale_color_hue(labels = c("Young", "Older")) +  # Custom legend labels
  scale_shape_manual(values = c(16,17), labels = c("Young", "Older")) +
  theme(legend.title = element_blank()) +
  labs(x = "Vastus Lateralis composite (Z_VL) of STEPPING leg", 
       y = "Step Length (m)",
       title = "Step Length vs. VL Structure/Function",
       subtitle = "in FORWARD steps")  # Proper labels

## Forward, Stance Leg, VL ##
summary_df <- merged_df %>%
  filter(`EMG_Leg (1 Stepping: 2 Stance)` == 2) %>%  # Filter for stepping leg trials
  filter(`Step_Type (1 Lat :  2 Fwd : 3 Bwd)` == 2) %>% # Filter for lateral steps
  group_by(Subject) %>%  # Group by Subject
  summarize(
    muscleVL = mean(Z_VL),  # Z_VL for each subject
    stepLength = mean(Full_Step_Length),  # Mean of Full_Step_Length for each subject
    ageGroup = mean(`Group ( 1-young, 2-older adults)`),  # Age group for each subject
    .groups = "drop"
  )

# Plot: Muscle qualities in VL x FORWARD STANCE Leg x Old vs. Young
ggplot(summary_df, aes(x = muscleVL, y = stepLength, color = as.factor(ageGroup), shape = as.factor(ageGroup))) +
  geom_point(size = 2.5) +  # Adjust point size
  theme_minimal() +
  scale_color_hue(labels = c("Young", "Older")) +  # Custom legend labels
  scale_shape_manual(values = c(16,17), labels = c("Young", "Older")) +
  theme(legend.title = element_blank()) +
  labs(x = "Vastus Lateralis composite (Z_VL) of STANCE leg", 
       y = "Step Length (m)",
       title = "Step Length vs. VL Structure/Function",
       subtitle = "in FORWARD steps")  # Proper labels

## Backward, Step Leg, BF ##
summary_df <- merged_df %>%
  filter(`EMG_Leg (1 Stepping: 2 Stance)` == 1) %>%  # Filter for stepping leg trials
  filter(`Step_Type (1 Lat :  2 Fwd : 3 Bwd)` == 3) %>% # Filter for lateral steps
  group_by(Subject) %>%  # Group by Subject
  summarize(
    muscleBF = mean(Z_BF),  # Z_VL for each subject
    stepLength = mean(Full_Step_Length),  # Mean of Full_Step_Length for each subject
    ageGroup = mean(`Group ( 1-young, 2-older adults)`),  # Age group for each subject
    .groups = "drop"
  )

# Plot: Muscle qualities in BF x BACKWARD STEPPING Leg x Old vs. Young
ggplot(summary_df, aes(x = muscleBF, y = stepLength, color = as.factor(ageGroup), shape = as.factor(ageGroup))) +
  geom_point(size = 2.5) +  # Adjust point size
  theme_minimal() +
  scale_color_hue(labels = c("Young", "Older")) +  # Custom legend labels
  scale_shape_manual(values = c(16,17), labels = c("Young", "Older")) +
  theme(legend.title = element_blank()) +
  labs(x = "Biceps Femoris composite (Z_BF) of STEPPING leg", 
       y = "Step Length (m)",
       title = "Step Length vs. BF Structure/Function",
       subtitle = "in BACKWARD steps")  # Proper labels

## Backward, Stance Leg, BF ##
summary_df <- merged_df %>%
  filter(`EMG_Leg (1 Stepping: 2 Stance)` == 2) %>%  # Filter for stepping leg trials
  filter(`Step_Type (1 Lat :  2 Fwd : 3 Bwd)` == 3) %>% # Filter for lateral steps
  group_by(Subject) %>%  # Group by Subject
  summarize(
    muscleBF = mean(Z_BF),  # Z_VL for each subject
    stepLength = mean(Full_Step_Length),  # Mean of Full_Step_Length for each subject
    ageGroup = mean(`Group ( 1-young, 2-older adults)`),  # Age group for each subject
    .groups = "drop"
  )

# Plot: Muscle qualities in BF x BACKWARD STANCE Leg x Old vs. Young
ggplot(summary_df, aes(x = muscleBF, y = stepLength, color = as.factor(ageGroup), shape = as.factor(ageGroup))) +
  geom_point(size = 2.5) +  # Adjust point size
  theme_minimal() +
  scale_color_hue(labels = c("Young", "Older")) +  # Custom legend labels
  scale_shape_manual(values = c(16,17), labels = c("Young", "Older")) +
  theme(legend.title = element_blank()) +
  labs(x = "Biceps Femoris composite (Z_BF) of STANCE leg", 
       y = "Step Length (m)",
       title = "Step Length vs. BF Structure/Function",
       subtitle = "in BACKWARD steps")  # Proper labels

### STEP HEIGHT ###

## Lateral, Step Leg, GM ##
summary_df <- merged_df %>%
  filter(`EMG_Leg (1 Stepping: 2 Stance)` == 1) %>%  # Filter for stepping leg trials
  filter(`Step_Type (1 Lat :  2 Fwd : 3 Bwd)` == 1) %>% # Filter for lateral steps
  group_by(Subject) %>%  # Group by Subject
  summarize(
    muscleGM = mean(Z_GM),  # Z_GM for each subject
    stepHeight = mean(Step_Height),  # Mean of Step_Height for each subject
    ageGroup = mean(`Group ( 1-young, 2-older adults)`),  # Age group for each subject
    .groups = "drop"
  )

# Plot: Muscle qualities in GM x LATERAL STEPPING Leg x Old vs. Young
ggplot(summary_df, aes(x = muscleGM, y = stepHeight, color = as.factor(ageGroup), shape = as.factor(ageGroup))) +
  geom_point(size = 2.5) +  # Adjust point size
  theme_minimal() +
  scale_color_hue(labels = c("Young", "Older")) +  # Custom legend labels
  scale_shape_manual(values = c(16,17), labels = c("Young", "Older")) +
  theme(legend.title = element_blank()) +
  labs(x = "Gluteus Medius composite (Z_GM) of STEPPING leg", 
       y = "Step Height (m)",
       title = "Step Height vs. GM Structure/Function",
       subtitle = "in LATERAL steps")  # Proper labels

## Lateral, Stance Leg, GM ##
summary_df <- merged_df %>%
  filter(`EMG_Leg (1 Stepping: 2 Stance)` == 2) %>%  # Filter for stepping leg trials
  filter(`Step_Type (1 Lat :  2 Fwd : 3 Bwd)` == 1) %>% # Filter for lateral steps
  group_by(Subject) %>%  # Group by Subject
  summarize(
    muscleGM = mean(Z_GM),  # Z_GM for each subject
    stepHeight = mean(Step_Height),  # Mean of Step_Height for each subject
    ageGroup = mean(`Group ( 1-young, 2-older adults)`),  # Age group for each subject
    .groups = "drop"
  )

# Plot: Muscle qualities in GM x LATERAL STANCE Leg x Old vs. Young
ggplot(summary_df, aes(x = muscleGM, y = stepHeight, color = as.factor(ageGroup), shape = as.factor(ageGroup))) +
  geom_point(size = 2.5) +  # Adjust point size
  theme_minimal() +
  scale_color_hue(labels = c("Young", "Older")) +  # Custom legend labels
  scale_shape_manual(values = c(16,17), labels = c("Young", "Older")) +
  theme(legend.title = element_blank()) +
  labs(x = "Gluteus Medius composite (Z_GM) of STANCE leg", 
       y = "Step Height (m)",
       title = "Step Height vs. GM Structure/Function",
       subtitle = "in LATERAL steps")  # Proper labels

## Forward, Step Leg, VL ##
summary_df <- merged_df %>%
  filter(`EMG_Leg (1 Stepping: 2 Stance)` == 1) %>%  # Filter for stepping leg trials
  filter(`Step_Type (1 Lat :  2 Fwd : 3 Bwd)` == 2) %>% # Filter for lateral steps
  group_by(Subject) %>%  # Group by Subject
  summarize(
    muscleVL = mean(Z_VL),  # Z_VL for each subject
    stepHeight = mean(Step_Height),  # Mean of Step_Height for each subject
    ageGroup = mean(`Group ( 1-young, 2-older adults)`),  # Age group for each subject
    .groups = "drop"
  )

# Plot: Muscle qualities in VL x FORWARD STEPPING Leg x Old vs. Young
ggplot(summary_df, aes(x = muscleVL, y = stepHeight, color = as.factor(ageGroup), shape = as.factor(ageGroup))) +
  geom_point(size = 2.5) +  # Adjust point size
  theme_minimal() +
  scale_color_hue(labels = c("Young", "Older")) +  # Custom legend labels
  scale_shape_manual(values = c(16,17), labels = c("Young", "Older")) +
  theme(legend.title = element_blank()) +
  labs(x = "Vastus Lateralis composite (Z_VL) of STEPPING leg", 
       y = "Step Height (m)",
       title = "Step Height vs. VL Structure/Function",
       subtitle = "in FORWARD steps")  # Proper labels

## Forward, Stance Leg, VL ##
summary_df <- merged_df %>%
  filter(`EMG_Leg (1 Stepping: 2 Stance)` == 2) %>%  # Filter for stepping leg trials
  filter(`Step_Type (1 Lat :  2 Fwd : 3 Bwd)` == 2) %>% # Filter for lateral steps
  group_by(Subject) %>%  # Group by Subject
  summarize(
    muscleVL = mean(Z_VL),  # Z_VL for each subject
    stepHeight = mean(Step_Height),  # Mean of Step_Height for each subject
    ageGroup = mean(`Group ( 1-young, 2-older adults)`),  # Age group for each subject
    .groups = "drop"
  )

# Plot: Muscle qualities in VL x FORWARD STANCE Leg x Old vs. Young
ggplot(summary_df, aes(x = muscleVL, y = stepHeight, color = as.factor(ageGroup), shape = as.factor(ageGroup))) +
  geom_point(size = 2.5) +  # Adjust point size
  theme_minimal() +
  scale_color_hue(labels = c("Young", "Older")) +  # Custom legend labels
  scale_shape_manual(values = c(16,17), labels = c("Young", "Older")) +
  theme(legend.title = element_blank()) +
  labs(x = "Vastus Lateralis composite (Z_VL) of STANCE leg", 
       y = "Step Height (m)",
       title = "Step Height vs. VL Structure/Function",
       subtitle = "in FORWARD steps")  # Proper labels

## Backward, Step Leg, BF ##
summary_df <- merged_df %>%
  filter(`EMG_Leg (1 Stepping: 2 Stance)` == 1) %>%  # Filter for stepping leg trials
  filter(`Step_Type (1 Lat :  2 Fwd : 3 Bwd)` == 3) %>% # Filter for lateral steps
  group_by(Subject) %>%  # Group by Subject
  summarize(
    muscleBF = mean(Z_BF),  # Z_VL for each subject
    stepHeight = mean(Step_Height),  # Mean of Step_Height for each subject
    ageGroup = mean(`Group ( 1-young, 2-older adults)`),  # Age group for each subject
    .groups = "drop"
  )

# Plot: Muscle qualities in BF x BACKWARD STEPPING Leg x Old vs. Young
ggplot(summary_df, aes(x = muscleBF, y = stepHeight, color = as.factor(ageGroup), shape = as.factor(ageGroup))) +
  geom_point(size = 2.5) +  # Adjust point size
  theme_minimal() +
  scale_color_hue(labels = c("Young", "Older")) +  # Custom legend labels
  scale_shape_manual(values = c(16,17), labels = c("Young", "Older")) +
  theme(legend.title = element_blank()) +
  labs(x = "Biceps Femoris composite (Z_BF) of STEPPING leg", 
       y = "Step Height (m)",
       title = "Step Height vs. BF Structure/Function",
       subtitle = "in BACKWARD steps")  # Proper labels

## Backward, Stance Leg, BF ##
summary_df <- merged_df %>%
  filter(`EMG_Leg (1 Stepping: 2 Stance)` == 2) %>%  # Filter for stepping leg trials
  filter(`Step_Type (1 Lat :  2 Fwd : 3 Bwd)` == 3) %>% # Filter for lateral steps
  group_by(Subject) %>%  # Group by Subject
  summarize(
    muscleBF = mean(Z_BF),  # Z_VL for each subject
    stepHeight = mean(Step_Height),  # Mean of Step_Height for each subject
    ageGroup = mean(`Group ( 1-young, 2-older adults)`),  # Age group for each subject
    .groups = "drop"
  )

# Plot: Muscle qualities in BF x BACKWARD STANCE Leg x Old vs. Young
ggplot(summary_df, aes(x = muscleBF, y = stepHeight, color = as.factor(ageGroup), shape = as.factor(ageGroup))) +
  geom_point(size = 2.5) +  # Adjust point size
  theme_minimal() +
  scale_color_hue(labels = c("Young", "Older")) +  # Custom legend labels
  scale_shape_manual(values = c(16,17), labels = c("Young", "Older")) +
  theme(legend.title = element_blank()) +
  labs(x = "Biceps Femoris composite (Z_BF) of STANCE leg", 
       y = "Step Height (m)",
       title = "Step Height vs. BF Structure/Function",
       subtitle = "in BACKWARD steps")  # Proper labels

