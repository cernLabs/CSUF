
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
muscle_data$participant_id[1:9] <- gsub("Pepper", "Pepper0",    # NOT ALL NEED A LEADING 0
                                  muscle_data$participant_id[1:9])

# Drop EMG columns from kinematic data for now? (Can undo this if we get this data)
kinematic_data_clean <- kinematic_data %>% select(-matches("Electric Potential"))

# get rid of 0 in Step Type
kinematic_data_clean$`step_type` <- 
  gsub(0,1,kinematic_data_clean$`step_type`)


## Make muscle data into Z-scores for composites

# Thickness measures 
muscle_data$Z_VL_Thickness <- (muscle_data$`vl_thickness` - mean(muscle_data$`vl_thickness`, na.rm =TRUE))/sd(muscle_data$`vl_thickness`, na.rm =TRUE)
muscle_data$Z_VI_Thickness <- (muscle_data$`vi_thickness` - mean(muscle_data$`vi_thickness`, na.rm =TRUE))/sd(muscle_data$`vi_thickness`, na.rm =TRUE)
muscle_data$Z_BF_Thickness <- (muscle_data$`bf_thickness` - mean(muscle_data$`bf_thickness`, na.rm =TRUE))/sd(muscle_data$`bf_thickness`, na.rm =TRUE)
muscle_data$Z_GM_Thickness <- (muscle_data$`gm_thickness` - mean(muscle_data$`gm_thickness`, na.rm =TRUE))/sd(muscle_data$`gm_thickness`, na.rm =TRUE)
muscle_data$Z_TFL_Thickness <- (muscle_data$`tfl_thickness` - mean(muscle_data$`tfl_thickness`, na.rm =TRUE))/sd(muscle_data$`tfl_thickness`, na.rm =TRUE)
muscle_data$Z_MG_Thickness <- (muscle_data$`mg_thickness` - mean(muscle_data$`mg_thickness`, na.rm =TRUE))/sd(muscle_data$`mg_thickness`, na.rm =TRUE)
muscle_data$Z_SOL_Thickness <- (muscle_data$`sol_thickness` - mean(muscle_data$`sol_thickness`, na.rm =TRUE))/sd(muscle_data$`sol_thickness`, na.rm =TRUE)
muscle_data$Z_TA_Thickness <- (muscle_data$`ta_thickness` - mean(muscle_data$`ta_thickness`, na.rm =TRUE))/sd(muscle_data$`ta_thickness`, na.rm =TRUE)

# Quality measures  (*Note: reverse scored, higher au = worse)
muscle_data$Z_VL_Quality <- -(muscle_data$`vl_quality` - mean(muscle_data$`vl_quality`,na.rm =TRUE))/sd(muscle_data$`vl_quality`,na.rm =TRUE)
muscle_data$Z_VI_Quality <- -(muscle_data$`vi_quality` - mean(muscle_data$`vi_quality`,na.rm =TRUE))/sd(muscle_data$`vi_quality`,na.rm =TRUE)
muscle_data$Z_BF_Quality <- -(muscle_data$`bf_quality` - mean(muscle_data$`bf_quality`,na.rm =TRUE))/sd(muscle_data$`bf_quality`,na.rm =TRUE)
muscle_data$Z_GM_Quality <- -(muscle_data$`gm_quality` - mean(muscle_data$`gm_quality`,na.rm =TRUE))/sd(muscle_data$`gm_quality`,na.rm =TRUE)
muscle_data$Z_TFL_Quality <- -(muscle_data$`tfl_quality` - mean(muscle_data$`tfl_quality`,na.rm =TRUE))/sd(muscle_data$`tfl_quality`,na.rm =TRUE)
muscle_data$Z_MG_Quality <- -(muscle_data$`mg_quality` - mean(muscle_data$`mg_quality`,na.rm =TRUE))/sd(muscle_data$`mg_quality`,na.rm =TRUE)
muscle_data$Z_SOL_Quality <- -(muscle_data$`sol_quality` - mean(muscle_data$`sol_quality`,na.rm =TRUE))/sd(muscle_data$`sol_quality`,na.rm =TRUE)
muscle_data$Z_TA_Quality <- -(muscle_data$`ta_quality` - mean(muscle_data$`ta_quality`,na.rm =TRUE))/sd(muscle_data$`ta_quality`,na.rm =TRUE)

# Stiffness measures (Only available for select muscles, (*Note: reverse scored, higher m/s = worse))
muscle_data$Z_VL_Stiffness <- -(muscle_data$`swvl` - mean(muscle_data$`swvl`, na.rm =TRUE))/sd(muscle_data$`swvl`, na.rm =TRUE)
muscle_data$Z_BF_Stiffness <- -(muscle_data$`swbf` - mean(muscle_data$`swbf`, na.rm =TRUE))/sd(muscle_data$`swbf`, na.rm =TRUE)
muscle_data$Z_TFL_Stiffness <- -(muscle_data$`swtfl` - mean(muscle_data$`swtfl`, na.rm =TRUE))/sd(muscle_data$`swtfl`, na.rm =TRUE)
muscle_data$Z_MG_Stiffness <- -(muscle_data$`swmg` - mean(muscle_data$`swmg`, na.rm =TRUE))/sd(muscle_data$`swmg`, na.rm =TRUE)
muscle_data$Z_TA_Stiffness <- -(muscle_data$`swta` - mean(muscle_data$`swta`, na.rm =TRUE))/sd(muscle_data$`swta`, na.rm =TRUE)

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

# Sort demographic data by subject before left joining
demographics <- arrange(demographics, subject) # first make sure these are in order

# Merge into one by left joining dataframes
merged_df <- demographics %>% 
  left_join(muscle_data, by = c("subject" = "participant_id")) %>%
  left_join(kinematic_data_clean, by = c("subject" = "pid"))

## Find missing data
missing_summary <- colSums(is.na(merged_df))

# Display columns with missing values
missing_summary[missing_summary>0]

# getting the outliers for the full_step_length
hist(kinematic_data_clean$step_length, breaks = 100)
hist(kinematic_data_clean$weight_transfer, breaks = 100)
hist(kinematic_data_clean$step_initiation_time, breaks = 100)
hist(kinematic_data_clean$weight_transfer_onset, breaks = 100)
hist(kinematic_data_clean$step_height, breaks = 100)

kinematic_data_clean %>% filter(step_length < .1 | weight_transfer < .015 | step_initiation_time < 0.3 ) %>% select(pid) %>% unique() 
kinematic_data_clean %>% filter(step_length < .1 ) %>% select(pid) %>% unique() 
kinematic_data_clean %>% filter(weight_transfer < .015) %>% select(pid) %>% unique() 
kinematic_data_clean %>% filter(step_initiation_time < 0.3 ) %>% select(pid) %>% unique() 
kinematic_data_clean %>% filter(weight_transfer_onset < .001) %>% select(pid) %>% unique() 
kinematic_data_clean %>% filter(step_height < 0.02 ) %>% select(pid) %>% unique() 

## Try some summary statistics for data exploration and visualization to follow
##  --->>> Need to determine what is relevant for summary stats!

summary_stats <- merged_df %>%
  summarise(
        Mean_Age = mean(`age`, na.rm = TRUE),
        Mean_Step_Length = mean(step_length, na.rm = TRUE),
        Mean_step_height = mean(step_height, na.rm = TRUE),
        Mean_step_initiation_time = mean(step_initiation_time, na.rm = TRUE),
        Mean_weight_transfer = mean(weight_transfer, na.rm = TRUE)
      )
    
print(summary_stats)


## Try some visualizations
##  --->>> Need to determine what is relevant for summary stats!


### Questions about stepping kinematics for visual EDA

# Step length by age group
ggplot(merged_df, aes(x = as.factor(`group`), 
                      y = step_length, 
                      fill = as.factor(`step_type`))) +
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
ggplot(merged_df, aes(x = as.factor(`group`), 
                      y = step_height, 
                      fill = as.factor(`step_type`))) +
  geom_boxplot() +
  labs(x = "Age Group (1 = Younger, 2 = Older)", 
       y = "Step height", 
       title = "Step Height by Age Group") +
  theme_minimal() +
  scale_fill_discrete(labels = c("Lateral Step", 
                                 "Forward Step", 
                                 "Backward Step")) + # Custom legend labels
  scale_x_discrete(name = "Age Group", labels = c("Young","Older")) +
  theme(legend.title = element_blank())

# Step initiation time by age group
ggplot(merged_df, aes(x = as.factor(`group`), 
                      y = step_initiation_time, 
                      fill = as.factor(`step_type`) )) +
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
ggplot(merged_df, aes(x = as.factor(`group`), 
                      y = weight_transfer_onset, 
                      fill = as.factor(`step_type`))) +
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
ggplot(merged_df, aes(x = as.factor(`group`), 
                      y = weight_transfer, 
                      fill = as.factor(`step_type`))) +
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
  filter(`EMG_leg` == 1) %>%  # Filter for stepping leg trials
  filter(`step_type` == 1) %>% # Filter for lateral steps
  group_by(subject) %>%  # Group by subject
  summarize(
    muscleGM = mean(Z_GM),  # Z_GM for each subject
    stepLength = mean(step_length),  # Mean of step_length for each subject
    ageGroup = mean(`group`),  # Age group for each subject
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
  filter(`EMG_leg` == 2) %>%  # Filter for stepping leg trials
  filter(`step_type` == 1) %>% # Filter for lateral steps
  group_by(subject) %>%  # Group by subject
  summarize(
    muscleGM = mean(Z_GM),  # Z_GM for each subject
    stepLength = mean(step_length),  # Mean of step_length for each subject
    ageGroup = mean(`group`),  # Age group for each subject
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
  filter(`EMG_leg` == 1) %>%  # Filter for stepping leg trials
  filter(`step_type` == 2) %>% # Filter for lateral steps
  group_by(subject) %>%  # Group by subject
  summarize(
    muscleVL = mean(Z_VL),  # Z_VL for each subject
    stepLength = mean(step_length),  # Mean of step_length for each subject
    ageGroup = mean(`group`),  # Age group for each subject
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
  filter(`EMG_leg` == 2) %>%  # Filter for stepping leg trials
  filter(`step_type` == 2) %>% # Filter for lateral steps
  group_by(subject) %>%  # Group by subject
  summarize(
    muscleVL = mean(Z_VL),  # Z_VL for each subject
    stepLength = mean(step_length),  # Mean of step_length for each subject
    ageGroup = mean(`group`),  # Age group for each subject
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
  filter(`EMG_leg` == 1) %>%  # Filter for stepping leg trials
  filter(`step_type` == 3) %>% # Filter for lateral steps
  group_by(subject) %>%  # Group by subject
  summarize(
    muscleBF = mean(Z_BF),  # Z_VL for each subject
    stepLength = mean(step_length),  # Mean of step_length for each subject
    ageGroup = mean(`group`),  # Age group for each subject
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
  filter(`EMG_leg` == 2) %>%  # Filter for stepping leg trials
  filter(`step_type` == 3) %>% # Filter for lateral steps
  group_by(subject) %>%  # Group by subject
  summarize(
    muscleBF = mean(Z_BF),  # Z_VL for each subject
    stepLength = mean(step_length),  # Mean of step_length for each subject
    ageGroup = mean(`group`),  # Age group for each subject
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

### STEP height ###

## Lateral, Step Leg, GM ##
summary_df <- merged_df %>%
  filter(`EMG_leg` == 1) %>%  # Filter for stepping leg trials
  filter(`step_type` == 1) %>% # Filter for lateral steps
  group_by(subject) %>%  # Group by subject
  summarize(
    muscleGM = mean(Z_GM),  # Z_GM for each subject
    stepHeight = mean(step_height),  # Mean of step_height for each subject
    ageGroup = mean(`group`),  # Age group for each subject
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
       y = "Step height",
       title = "Step Height vs. GM Structure/Function",
       subtitle = "in LATERAL steps")  # Proper labels

## Lateral, Stance Leg, GM ##
summary_df <- merged_df %>%
  filter(`EMG_leg` == 2) %>%  # Filter for stepping leg trials
  filter(`step_type` == 1) %>% # Filter for lateral steps
  group_by(subject) %>%  # Group by subject
  summarize(
    muscleGM = mean(Z_GM),  # Z_GM for each subject
    stepHeight = mean(step_height),  # Mean of step_height for each subject
    ageGroup = mean(`group`),  # Age group for each subject
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
       y = "Step height",
       title = "Step Height vs. GM Structure/Function",
       subtitle = "in LATERAL steps")  # Proper labels

## Forward, Step Leg, VL ##
summary_df <- merged_df %>%
  filter(`EMG_leg` == 1) %>%  # Filter for stepping leg trials
  filter(`step_type` == 2) %>% # Filter for lateral steps
  group_by(subject) %>%  # Group by subject
  summarize(
    muscleVL = mean(Z_VL),  # Z_VL for each subject
    stepHeight = mean(step_height),  # Mean of step_height for each subject
    ageGroup = mean(`group`),  # Age group for each subject
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
       y = "Step height",
       title = "Step Height vs. VL Structure/Function",
       subtitle = "in FORWARD steps")  # Proper labels

## Forward, Stance Leg, VL ##
summary_df <- merged_df %>%
  filter(`EMG_leg` == 2) %>%  # Filter for stepping leg trials
  filter(`step_type` == 2) %>% # Filter for lateral steps
  group_by(subject) %>%  # Group by subject
  summarize(
    muscleVL = mean(Z_VL),  # Z_VL for each subject
    stepHeight = mean(step_height),  # Mean of step_height for each subject
    ageGroup = mean(`group`),  # Age group for each subject
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
       y = "Step height",
       title = "Step Height vs. VL Structure/Function",
       subtitle = "in FORWARD steps")  # Proper labels

## Backward, Step Leg, BF ##
summary_df <- merged_df %>%
  filter(`EMG_leg` == 1) %>%  # Filter for stepping leg trials
  filter(`step_type` == 3) %>% # Filter for lateral steps
  group_by(subject) %>%  # Group by subject
  summarize(
    muscleBF = mean(Z_BF),  # Z_VL for each subject
    stepHeight = mean(step_height),  # Mean of step_height for each subject
    ageGroup = mean(`group`),  # Age group for each subject
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
       y = "Step height",
       title = "Step Height vs. BF Structure/Function",
       subtitle = "in BACKWARD steps")  # Proper labels

## Backward, Stance Leg, BF ##
summary_df <- merged_df %>%
  filter(`EMG_leg` == 2) %>%  # Filter for stepping leg trials
  filter(`step_type` == 3) %>% # Filter for lateral steps
  group_by(subject) %>%  # Group by subject
  summarize(
    muscleBF = mean(Z_BF),  # Z_VL for each subject
    stepHeight = mean(step_height),  # Mean of step_height for each subject
    ageGroup = mean(`group`),  # Age group for each subject
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
       y = "Step height",
       title = "Step Height vs. BF Structure/Function",
       subtitle = "in BACKWARD steps")  # Proper labels


# generate summary table

#### Summaries Table
Q1 <- demographics %>% 
  group_by(`Group ( 1-young, 2-older adults)`) %>%
  summarise(
    count = n(),
    age = paste0(round(mean(`Age (years old)`, na.rm = TRUE), 2), ' (', round(sd(`Age (years old)`, na.rm = TRUE), 2), ')'),
    `female count` = paste0(sum(`Sex (1-male , 2-female)` == 2), '/', round((sum(`Sex (1-male , 2-female)` == 2) / count) * 100, 2), '%'),
    `Right dominant` = paste0(sum(`Dominant Side (1- right, 2 -left)` == 1), '/', round((sum(`Dominant Side (1- right, 2 -left)` == 1) / count) * 100, 2), '%'),
    `Four Square Step Test` = paste0(round(mean(`4SST (time - s)`, na.rm = TRUE), 2), ' (', round(sd(`4SST (time - s)`, na.rm = TRUE), 2), ')'),
    `Handgrip Strength` = paste0(round(mean(`Handgrip Strenght Test (kg.f)`, na.rm = TRUE), 2), ' (', round(sd(`Handgrip Strenght Test (kg.f)`, na.rm = TRUE), 2), ')')
  )

Q2 <- demographics %>% 
  summarise(
    count = n(),
    age = paste0(round(mean(`Age (years old)`, na.rm = TRUE), 2), ' (', round(sd(`Age (years old)`, na.rm = TRUE), 2), ')'),
    `female count` = paste0(sum(`Sex (1-male , 2-female)` == 2), '/', round((sum(`Sex (1-male , 2-female)` == 2) / count) * 100, 2), '%'),
    `Right dominant` = paste0(sum(`Dominant Side (1- right, 2 -left)` == 1), '/', round((sum(`Dominant Side (1- right, 2 -left)` == 1) / count) * 100, 2), '%'),
    `Four Square Step Test` = paste0(round(mean(`4SST (time - s)`, na.rm = TRUE), 2), ' (', round(sd(`4SST (time - s)`, na.rm = TRUE), 2), ')'),
    `Handgrip Strength` = paste0(round(mean(`Handgrip Strenght Test (kg.f)`, na.rm = TRUE), 2), ' (', round(sd(`Handgrip Strenght Test (kg.f)`, na.rm = TRUE), 2), ')')
  )
######### Making table
# Extract the P-value
P_vals = c(
  '-',
  signif(t.test(`Age (years old)` ~ `Group ( 1-young, 2-older adults)`, data = demographics)$p.value,4),
  signif(chisq.test(matrix(c(2,5,9,4), byrow = TRUE))$p.value,4),
  signif(chisq.test(matrix(c(11,7,0,2), byrow = TRUE))$p.value,4),
  signif(t.test(`4SST (time - s)` ~ `Group ( 1-young, 2-older adults)`, data = demographics)$p.value,4),
  signif(t.test(`Handgrip Strenght Test (kg.f)` ~ `Group ( 1-young, 2-older adults)`, data = demographics)$p.value,4)
)

######### Bind Demographics Summary Table
Q = t(rbind(Q2,Q1[,2:7],P_vals))
colnames(Q) = c("All Participants","Younger","Older","P-Vals")
library(kableExtra)
kable(Q)




####### Kinematics Summary Table

## extract from kinematics data

QkinL <- kinematic_data %>%   
  summarise(
    count = n(),
    `Full Step Length` = paste0(round(mean(Full_Step_Length, na.rm = TRUE), 2), ' (',round(sd(Full_Step_Length, na.rm = TRUE), 2), ')'),
    `Step Height` = paste0(round(mean(Step_Height, na.rm = TRUE), 2), ' (',round(sd(Step_Height, na.rm = TRUE), 2), ')'),
    `Initiation Time` = paste0(round(mean(Step_Initiation_Time, na.rm = TRUE), 2), ' (',round(sd(Step_Initiation_Time, na.rm = TRUE), 2), ')'),
    `Weight Transfer Duration` = paste0(round(mean(Weight_Transfer_Duration, na.rm = TRUE), 2), ' (',round(sd(Weight_Transfer_Duration, na.rm = TRUE), 2), ')'),
    `Weight Transfer Onset` = paste0(round(mean(Step_Initiation_Time, na.rm = TRUE), 2), ' (',round(sd(Step_Initiation_Time, na.rm = TRUE), 2), ')')
    )

QkinR <- kinematic_data %>% group_by(`Age Group (1 Younger; 2 Older)`) %>%
  summarise(
    count = n(),
    `Full Step Length` = paste0(round(mean(Full_Step_Length, na.rm = TRUE), 2), ' (',round(sd(Full_Step_Length, na.rm = TRUE), 2), ')'),
    `Step Height` = paste0(round(mean(Step_Height, na.rm = TRUE), 2), ' (',round(sd(Step_Height, na.rm = TRUE), 2), ')'),
    `Initiation Time` = paste0(round(mean(Step_Initiation_Time, na.rm = TRUE), 2), ' (',round(sd(Step_Initiation_Time, na.rm = TRUE), 2), ')'),
    `Weight Transfer Duration` = paste0(round(mean(Weight_Transfer_Duration, na.rm = TRUE), 2), ' (',round(sd(Weight_Transfer_Duration, na.rm = TRUE), 2), ')'),
    `Weight Transfer Onset` = paste0(round(mean(Step_Initiation_Time, na.rm = TRUE), 2), ' (',round(sd(Step_Initiation_Time, na.rm = TRUE), 2), ')')
  )


P_valsK = c(
  '-',
  signif(t.test(Full_Step_Length ~ `Age Group (1 Younger; 2 Older)`, data = kinematic_data)$p.value,4),
  signif(t.test(Step_Height ~ `Age Group (1 Younger; 2 Older)`, data = kinematic_data)$p.value,4),
  signif(t.test(Step_Initiation_Time ~ `Age Group (1 Younger; 2 Older)`, data = kinematic_data)$p.value,4),
  signif(t.test(Weight_Transfer_Duration ~ `Age Group (1 Younger; 2 Older)`, data = kinematic_data)$p.value,4),
  signif(t.test(Weight_Transfer_Onset_Initiation_Time ~ `Age Group (1 Younger; 2 Older)`, data = kinematic_data)$p.value,4)
)


##### Bind Kinematics Summary Table 
QK = t(rbind(QkinL,QkinR[,2:7],P_valsK))
colnames(QK) = c("All Participants","Younger","Older","P-Vals")
kable(QK)



#######################################################################################################################
#############################  SUMMARIZING MUSCLE DATA  ###############################################################
#######################################################################################################################

# select the data you need

muscle_cummulatives <- merged_df %>% 
  mutate(thickness_added = 
           Z_VL_Thickness +
           Z_VI_Thickness +
           Z_BF_Thickness + 
           Z_GM_Thickness + 
           Z_SOL_Thickness +
           Z_TA_Thickness + 
           Z_TFL_Thickness + 
           Z_MG_Thickness,
         quality_added = 
           Z_VL_Quality +
           Z_VI_Quality +
           Z_BF_Quality +
           Z_GM_Quality +
           Z_SOL_Quality +
           Z_TA_Quality +
           Z_TFL_Quality +
           Z_MG_Quality,
         stiffness_added =
           Z_VL_Stiffness + 
           Z_BF_Stiffness +
           Z_TFL_Stiffness + 
           Z_MG_Stiffness + 
           Z_TA_Stiffness
         ) %>% 
  select(group = AgeGroup1Younger2Older,
                          thickness_added,
                          quality_added,
                          stiffness_added) %>% 
  mutate(group = case_when(
                          group == 1 ~ 'younger',
                          group == 2 ~ 'older'
                          ))


#### remove NAs for each vizual
Muscle_thickness_na_removed <- muscle_cummulatives %>% filter(!is.na(thickness_added))
Muscle_qual_na_removed <- muscle_cummulatives %>% filter(!is.na(quality_added))
Muscle_stiff_na_removed <- muscle_cummulatives %>%
  filter(!is.na(stiffness_added))
#### Thickness
ggplot(Muscle_thickness_na_removed, aes(x = thickness_added,
                                        fill = group)) +
  geom_density(alpha = 0.5) +
  labs(x = 'Muscle Thickness',
         title = "Composite Muscle Thickness between Older and Younger")+
  scale_fill_manual(values = c("younger" = "#f8766d", "older" = "#00bfc4"))

#### Quality 
ggplot(Muscle_qual_na_removed, aes(x = quality_added,
                                   fill = group)) +
  geom_density(alpha = 0.5)  +
  labs(x = 'Muscle Quality',
       title = "Composite Muscle Quality between Older and Younger") +
  scale_fill_manual(values = c("younger" = "#f8766d", "older" = "#00bfc4")) 

#### Stiffness

ggplot(Muscle_stiff_na_removed, aes(x = stiffness_added,
                                    fill = group)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("younger" = "#f8766d", "older" = "#00bfc4"))  +
  labs(x = 'Muscle Stiffness',
       title = "Composite Muscle Stiffness between Older and Younger")

library(ggplot2)
library(patchwork)
library(kableExtra)

#######################################################################################################################
#############################  SUMMARIZING MUSCLE DATA END ############################################################
#######################################################################################################################
