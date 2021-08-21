library(ggplot2)
library(tidyverse)
library(ggpubr)
library(rstatix)

# Dataframe
df <- Balan_Greta

# Variables transformation in factor
df$Factor <- as.factor(df$Factor)
#df$Blocks <- as.factor(df$Blocks)
df$`Nr/p.` <- as.factor(df$`Nr/p.`)

# Variable Medication
df$Medication <- df$Factor


# Descriptive statistics table
library(table1)

table1::table1(~. | Factor, data = df[, c(3:8, 15)], 
               render.continuous=c(.="Mean (SD)", 
                                   .="Median (IQR)", 
                                   .="[Min, Max]"),
               topclass="Rtable1-grid"
)

table1::table1(~. | Factor, data = df[, c(9:14, 15)], 
               render.continuous=c(.="Mean (SD)", 
                                   .="Median (IQR)", 
                                   .="[Min, Max]"),
               topclass="Rtable1-grid"
)


# Descriptive statistics graph

level_order <- c('Martor', 'B1', 'B2', 'C1', 'C2', 'C1+B1', 'C1+B2', 'C2+B1', 'C2+B2') 

p <- ggplot(df, aes(x = factor(Medication, level = level_order), y = `DAM, µM/L`, fill = Medication))+
  geom_violin(trim=FALSE, fill='lightPink', color="darkred")+
  geom_boxplot(width=0.1) + 
  theme_minimal()+
  xlab("Medicatie")+
  ylab("DAM, µM/L")

p + scale_fill_discrete(name = "Medicatie")

p <- ggplot(df, aes(x = factor(Medication, level = level_order), y = df$`PPOA,          µM/L`, fill = Medication))+
  geom_violin(trim=FALSE, fill='lightPink', color="darkred")+
  geom_boxplot(width=0.1) + 
  theme_minimal()+
  xlab("Medicatie")+
  ylab("PPOA, µM/L")

p + scale_fill_discrete(name = "Medicatie")

p <- ggplot(df, aes(x = factor(Medication, level = level_order), y = df$`SOD,     u/c`, fill = Medication))+
  geom_violin(trim=FALSE, fill='lightPink', color="darkred")+
  geom_boxplot(width=0.1) + 
  theme_minimal()+
  xlab("Medicatie")+
  ylab("SOD, u/c")

p + scale_fill_discrete(name = "Medicatie")

p <- ggplot(df, aes(x = factor(Medication, level = level_order), y = df$`Catalaza, µM/s.L`, fill = Medication))+
  geom_violin(trim=FALSE, fill='lightPink', color="darkred")+
  geom_boxplot(width=0.1) + 
  theme_minimal()+
  xlab("Medicatie")+
  ylab("Catalaza, µM/s.L")

p + scale_fill_discrete(name = "Medicatie")

p <- ggplot(df, aes(x = factor(Medication, level = level_order), y = df$`AAT cu ABTS, µM/L`, fill = Medication))+
  geom_violin(trim=FALSE, fill='lightPink', color="darkred")+
  geom_boxplot(width=0.1) + 
  theme_minimal()+
  xlab("Medicatie")+
  ylab("AAT cu ABTS, µM/L")

p + scale_fill_discrete(name = "Medicatie")

p <- ggplot(df, aes(x = factor(Medication, level = level_order), y = df$`G-S-T, nM/s.L`, fill = Medication))+
  geom_violin(trim=FALSE, fill='lightPink', color="darkred")+
  geom_boxplot(width=0.1) + 
  theme_minimal()+
  xlab("Medicatie")+
  ylab("G-S-T, nM/s.L")

p + scale_fill_discrete(name = "Medicatie")

p <- ggplot(df, aes(x = factor(Medication, level = level_order), y = df$`GPO, nM/s.L`, fill = Medication))+
  geom_violin(trim=FALSE, fill='lightPink', color="darkred")+
  geom_boxplot(width=0.1) + 
  theme_minimal()+
  xlab("Medicatie")+
  ylab("GPO, nM/s.L")

p + scale_fill_discrete(name = "Medicatie")

p <- ggplot(df, aes(x = factor(Medication, level = level_order), y = df$`GR, nM/s.L`, fill = Medication))+
  geom_violin(trim=FALSE, fill='lightPink', color="darkred")+
  geom_boxplot(width=0.1) + 
  theme_minimal()+
  xlab("Medicatie")+
  ylab("GR, nM/s.L")

p + scale_fill_discrete(name = "Medicatie")


p <- ggplot(df, aes(x = factor(Medication, level = level_order), y = df$`TNF, pg/ml`, fill = Medication))+
  geom_violin(trim=FALSE, fill='lightPink', color="darkred")+
  geom_boxplot(width=0.1) + 
  theme_minimal()+
  xlab("Medicatie")+
  ylab("TNF, pg/ml")

p + scale_fill_discrete(name = "Medicatie")

p <- ggplot(df, aes(x = factor(Medication, level = level_order), y = df$`IL-6, pg/ml`, fill = Medication))+
  geom_violin(trim=FALSE, fill='lightPink', color="darkred")+
  geom_boxplot(width=0.1) + 
  theme_minimal()+
  xlab("Medicatie")+
  ylab("IL-6, pg/ml")

p + scale_fill_discrete(name = "Medicatie")


p <- ggplot(df, aes(x = factor(Medication, level = level_order), y = df$`IL-10, pg/ml`, fill = Medication))+
  geom_violin(trim=FALSE, fill='lightPink', color="darkred")+
  geom_boxplot(width=0.1) + 
  theme_minimal()+
  xlab("Medicatie")+
  ylab("IL-10, pg/ml")

p + scale_fill_discrete(name = "Medicatie")


# Comparative evaluation Friedman test

?friedman.test

df$DAM <- df$`DAM, µM/L`

res.fried <- df %>% friedman_test(DAM ~ Medication | Blocks)
res.fried

result <- friedman.test(DAM ~ Medication | Blocks, data = df)
result

pairwise.wilcox.test(df$DAM, df$Medication, p.adjust.method = "BH", paired = T)


df %>% friedman_effsize(DAM ~ Medication | Blocks)

pwc <- df %>%
  wilcox_test(DAM ~ Medication, paired = TRUE, p.adjust.method = "BH")
pwc


result <- friedman.test(df$`PPOA,          µM/L` ~ Medication | Blocks, data = df)
result

pairwise.wilcox.test(df$`PPOA,          µM/L`, df$Medication, p.adjust.method = "BH", paired = T)

df %>% friedman_effsize(df$`PPOA,          µM/L` ~ Medication | Blocks)


result <- friedman.test(df$`SOD,     u/c` ~ Medication | Blocks, data = df)
result

pairwise.wilcox.test(df$`SOD,     u/c`, df$Medication, p.adjust.method = "BH", paired = T)

df %>% friedman_effsize(df$`SOD,     u/c` ~ Medication | Blocks)


result <- friedman.test(df$`Catalaza, µM/s.L` ~ Medication | Blocks, data = df)
result

pairwise.wilcox.test(df$`Catalaza, µM/s.L`, df$Medication, p.adjust.method = "BH", paired = T)

df %>% friedman_effsize(df$`Catalaza, µM/s.L` ~ Medication | Blocks)

result <- friedman.test(df$`AAT cu ABTS, µM/L` ~ Medication | Blocks, data = df)
result

pairwise.wilcox.test(df$`AAT cu ABTS, µM/L`, df$Medication, p.adjust.method = "BH", paired = T)

df %>% friedman_effsize(df$`AAT cu ABTS, µM/L` ~ Medication | Blocks)


result <- friedman.test(df$`G-S-T, nM/s.L` ~ Medication | Blocks, data = df)
result

pairwise.wilcox.test(df$`G-S-T, nM/s.L`, df$Medication, p.adjust.method = "BH", paired = T)

df %>% friedman_effsize(df$`G-S-T, nM/s.L` ~ Medication | Blocks)


result <- friedman.test(df$`GPO, nM/s.L` ~ Medication | Blocks, data = df)
result

pairwise.wilcox.test(df$`GPO, nM/s.L`, df$Medication, p.adjust.method = "BH", paired = T)

df %>% friedman_effsize(df$`GPO, nM/s.L` ~ Medication | Blocks)


result <- friedman.test(df$`GR, nM/s.L` ~ Medication | Blocks, data = df)
result

pairwise.wilcox.test(df$`GR, nM/s.L`, df$Medication, p.adjust.method = "BH", paired = T)

df %>% friedman_effsize(df$`GR, nM/s.L` ~ Medication | Blocks)


result <- friedman.test(df$`IL-1, pg/ml` ~ Medication | Blocks, data = df)
result

pairwise.wilcox.test(df$`IL-1, pg/ml`, df$Medication, p.adjust.method = "BH", paired = T)

df %>% friedman_effsize(df$`IL-1, pg/ml` ~ Medication | Blocks)


result <- friedman.test(df$`TNF, pg/ml` ~ Medication | Blocks, data = df)
result

pairwise.wilcox.test(df$`TNF, pg/ml`, df$Medication, p.adjust.method = "BH", paired = T)

df %>% friedman_effsize(df$`TNF, pg/ml` ~ Medication | Blocks)


result <- friedman.test(df$`IL-6, pg/ml` ~ Medication | Blocks, data = df)
result

pairwise.wilcox.test(df$`IL-6, pg/ml`, df$Medication, p.adjust.method = "BH", paired = T)

df %>% friedman_effsize(df$`IL-6, pg/ml` ~ Medication | Blocks)



result <- friedman.test(df$`IL-10, pg/ml` ~ Medication | Blocks, data = df)
result

pairwise.wilcox.test(df$`IL-10, pg/ml`, df$Medication, p.adjust.method = "BH", paired = T)

df %>% friedman_effsize(df$`IL-10, pg/ml` ~ Medication | Blocks)
