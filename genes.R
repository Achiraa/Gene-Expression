#luminal Progenitor markers code 
library(ggplot2)
setwd("C:/geo_data/october")

# Read the CSV file into a data frame
esrdata<- read.csv("Log2 Reads per million_progenitor markers.csv")

esrdata1=as.data.frame(esrdata)
dim(esrdata1)

esrdata12=esrdata1[1:28,1:34]
esrdata12


# Create a new variable to combine categories
esrdata12$CombinedClassification <- esrdata12$Classification2
esrdata12$CombinedClassification[esrdata12$Classification2 %in% c("Stromal.eGFP.UT", "Stromal.eGFP.E+P")] <- "Stromal.eGFP"
esrdata12$CombinedClassification[esrdata12$Classification2 %in% c("Luminal.eGFP.UT", "Luminal.eGFP.E+P")] <- "Luminal.eGFP"
esrdata12$CombinedClassification[esrdata12$Classification2 %in% c("Stromal.tdTomato.UT", "Stromal.tdTomato.E+P")] <- "Stromal.tdTomato"
esrdata12$CombinedClassification[esrdata12$Classification2 %in% c("Luminal.tdTomato.UT", "Luminal.tdTomato.E+P")] <- "Luminal.tdTomato"


# Convert the new variable to a factor
esrdata12$CombinedClassification <- factor(esrdata12$CombinedClassification)
View(esrdata12)

# Create the grouped barplot with the combined categories
esr1 <- ggplot(esrdata12, aes(x = CombinedClassification, y = Esr1, fill = Treatment)) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  theme_classic() +
  labs(title = "Esr1 Expression levels", x = "Cell Type", y = "Expression Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
esr1

# Create the grouped barplot with the combined categories
pgr<- ggplot(esrdata12, aes(x = CombinedClassification, y = Pgr, fill = Treatment)) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  theme_classic() +
  labs(title = "Pgr Expression levels", x = "Cell Type", y = "Expression Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
pgr

# Create the grouped barplot with the combined categories
kitl <- ggplot(esrdata12, aes(x = CombinedClassification, y = Kitl, fill = Treatment)) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  theme_classic() +
  labs(title = "Kitl Expression levels", x = "Cell Type", y = "Expression Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
kitl

# Create the grouped barplot with the combined categories
aldh1a7<- ggplot(esrdata12, aes(x = CombinedClassification, y = Aldh1a7, fill = Treatment)) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  theme_classic() +
  labs(title = "Aldh1a7 Expression levels", x = "Cell Type", y = "Expression Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
aldh1a7

# Create the grouped barplot with the combined categories
notch1 <- ggplot(esrdata12, aes(x = CombinedClassification, y = Notch1, fill = Treatment)) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  theme_classic() +
  labs(title = "Notch1 Expression levels", x = "Cell Type", y = "Expression Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
notch1

# Create the grouped barplot with the combined categories
elf5 <- ggplot(esrdata12, aes(x = CombinedClassification, y = Elf5, fill = Treatment)) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  theme_classic() +
  labs(title = "Elf5 Expression levels", x = "Cell Type", y = "Expression Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
elf5

# Create the grouped barplot with the combined categories
kit<- ggplot(esrdata12, aes(x = CombinedClassification, y = Kit, fill = Treatment)) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  theme_classic() +
  labs(title = "Kit Expression levels", x = "Cell Type", y = "Expression Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
kit

# Create the grouped barplot with the combined categories
krt19 <- ggplot(esrdata12, aes(x = CombinedClassification, y =Krt19, fill = Treatment)) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  theme_classic() +
  labs(title = "Krt19 Expression levels", x = "Cell Type", y = "Expression Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
krt19

# Create the grouped barplot with the combined categories
krt18 <- ggplot(esrdata12, aes(x = CombinedClassification, y =Krt18, fill = Treatment)) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  theme_classic() +
  labs(title = "Krt18 Expression levels", x = "Cell Type", y = "Expression Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
krt18

# Create the grouped barplot with the combined categories
itga6 <- ggplot(esrdata12, aes(x = CombinedClassification, y =Itga6, fill = Treatment)) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  theme_classic() +
  labs(title = "Itga6 Expression levels", x = "Cell Type", y = "Expression Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
itga6

# Create the grouped barplot with the combined categories
epcam <- ggplot(esrdata12, aes(x = CombinedClassification, y =Epcam, fill = Treatment)) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  theme_classic() +
  labs(title = "Epcam Expression levels", x = "Cell Type", y = "Expression Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
epcam

# Create the grouped barplot with the combined categories
krt14 <- ggplot(esrdata12, aes(x = CombinedClassification, y =Krt14, fill = Treatment)) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  theme_classic() +
  labs(title = "Krt14 Expression levels", x = "Cell Type", y = "Expression Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
krt14

# Create the grouped barplot with the combined categories
krt8 <- ggplot(esrdata12, aes(x = CombinedClassification, y =Krt8, fill = Treatment)) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  theme_classic() +
  labs(title = "Krt8 Expression levels", x = "Cell Type", y = "Expression Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
krt8

# Create the grouped barplot with the combined categories
cd24a <- ggplot(esrdata12, aes(x = CombinedClassification, y =Cd24a, fill = Treatment)) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  theme_classic() +
  labs(title = "Cd24a Expression levels", x = "Cell Type", y = "Expression Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
cd24a

# Create the grouped barplot with the combined categories
krt19 <- ggplot(esrdata12, aes(x = CombinedClassification, y =Krt19, fill = Treatment)) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  theme_classic() +
  labs(title = "Krt19 Expression levels", x = "Cell Type", y = "Expression Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
krt19

# Create the grouped barplot with the combined categories
aldh1a1 <- ggplot(esrdata12, aes(x = CombinedClassification, y =Aldh1a1, fill = Treatment)) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  theme_classic() +
  labs(title = "Aldh1a1 Expression levels", x = "Cell Type", y = "Expression Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
aldh1a1

# Create the grouped barplot with the combined categories
notch2 <- ggplot(esrdata12, aes(x = CombinedClassification, y = Notch2, fill = Treatment)) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  theme_classic() +
  labs(title = "Notch2 Expression levels", x = "Cell Type", y = "Expression Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
notch2

# Create the grouped barplot with the combined categories
notch3 <- ggplot(esrdata12, aes(x = CombinedClassification, y = Notch3, fill = Treatment)) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  theme_classic() +
  labs(title = "Notch3 Expression levels", x = "Cell Type", y = "Expression Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
notch3

# Create the grouped barplot with the combined categories
aldh1l1	 <- ggplot(esrdata12, aes(x = CombinedClassification, y =Aldh1l1, fill = Treatment)) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  theme_classic() +
  labs(title = "Aldh1l1 Expression levels", x = "Cell Type", y = "Expression Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
aldh1l1	

# Create the grouped barplot with the combined categories
aldh2	 <- ggplot(esrdata12, aes(x = CombinedClassification, y =Aldh2, fill = Treatment)) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  theme_classic() +
  labs(title = "Aldh2 Expression levels", x = "Cell Type", y = "Expression Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
aldh2	

# Create the grouped barplot with the combined categories
elf1 <- ggplot(esrdata12, aes(x = CombinedClassification, y = Elf1, fill = Treatment)) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  theme_classic() +
  labs(title = "Elf1 Expression levels", x = "Cell Type", y = "Expression Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
elf1

# Create the grouped barplot with the combined categories
elf5 <- ggplot(esrdata12, aes(x = CombinedClassification, y = Elf5, fill = Treatment)) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  theme_classic() +
  labs(title = "Elf5 Expression levels", x = "Cell Type", y = "Expression Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
elf5

# Create the grouped barplot with the combined categories
adrb2 <- ggplot(esrdata12, aes(x = CombinedClassification, y = Adrb2, fill = Treatment)) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  theme_classic() +
  labs(title = "Adrb2 Expression levels", x = "Cell Type", y = "Expression Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
adrb2

# Create the grouped barplot with the combined categories
adrb1<- ggplot(esrdata12, aes(x = CombinedClassification, y = Adrb1, fill = Treatment)) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  theme_classic() +
  labs(title = "Adrb1 Expression levels", x = "Cell Type", y = "Expression Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
adrb1

# Create the grouped barplot with the combined categories
adrb3 <- ggplot(esrdata12, aes(x = CombinedClassification, y = Adrb3, fill = Treatment)) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  theme_classic() +
  labs(title = "Adrb3 Expression levels", x = "Cell Type", y = "Expression Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
adrb3

# Create the grouped barplot with the combined categories
sparc<- ggplot(esrdata12, aes(x = CombinedClassification, y = Sparc, fill = Treatment)) +
  scale_fill_manual(values = c("black", "grey")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  theme_classic() +
  labs(title = "Sparc Expression levels", x = "Cell Type", y = "Expression Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
sparc
