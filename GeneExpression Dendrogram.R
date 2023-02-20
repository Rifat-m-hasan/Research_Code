# Load Data For Analysis ------------

expression_atlas_arabidopsis_thaliana <- read_excel("RNA-Seq Analysis/expression_atlas-arabidopsis_thaliana.xlsx")

inv_expression <- as.data.frame(t(expression_atlas_arabidopsis_thaliana))

# Inverse the data for making data for correct formate --------------

write_csv(inv_expression, file = "Inverse_Expression_data.csv")

Inverse_Expression_data <- read_csv("Inverse_Expression_data.csv")

# Normalization ------------------

x <- Inverse_Expression_data[,-c(1,1)] 
y <- apply(x,2,mean)
z <- apply(x,2,sd)

cluster <- scale(x,y,z)

# calculating Euclidian Distance

distance <- dist(cluster)

print(distance, digits = 3)

# Clustruring dindiogram

clus <- hclust(distance)

# Figure Generation

plot(clus)

plot(clus, labels = Inverse_Expression_data$`Plant Part`, hang = -1)
