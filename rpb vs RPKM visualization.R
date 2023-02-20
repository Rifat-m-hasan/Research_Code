#Calling Library-----------------------------

library(devtools)
library(tidyverse)
library(readxl)
library(ggthemes)
library(Rcpp)

# Loading Data into R ------------------------

rna_seq <- read_excel("Araport_11_RNA-seq_data-1619554329255.xlsx") 

# Making Plot Figure Using ggplot() ------------------------------

Reads.mapped.locus <- cut(rna_seq$`Reads mapped to locus`, 
                          breaks = c(25,725,1420,2110,2800,3500,7000),
                          labels = c("25 to 725","726 to 1420", "1420 to 2110",
                                     "2111 to 2800", "2801 to 3500", "more than 3500"))

Total.number.reads <- cut(rna_seq$`Total number of reads`, 
                          breaks = 10,
                          labels = c("3.91 to 11.10 megabase","11.10 to 18.10 megabase",
                                     "18.10 to 25.20 megabase", "25.20 to 32.30 megabase",
                                     "32.30 to 39.30 megabase","39.30 to 46.40 megabase",
                                     "46.40 to 53.50 megabase","53.50 to 60.50 megabase",
                                     "60.50 to 67.60 megabase","67.60 to 74.80 megabase"))


ggplot(data = rna_seq) +
  geom_jitter(mapping = aes(x = rpb, y = RPKM,
                            size = `Total.number.reads`,
                            color = `Reads.mapped.locus`,
                           alpha = 0.5)) +
  annotate("text", 
           label = "Leaf of 6 week old short-day-grown plant infiltrated with mock (virulent bacterium) and harvested 12h post-infiltration",
           x = 0.45,
           y = 127) +
  annotate("text", 
           label = "5day old dark-grown seedling", 
           x = 0.575, 
           y = 129.91) +
  annotate("text", 
           label = "5day old dark-grown seedling", 
           x = 0.575, 
           y = 127.18) +
  annotate("text", 
           label = "5day old etiolated seedling", 
           x = 0.518, 
           y = 124.97) +
  annotate("text", 
           label = "5day old etiolated seedling", 
           x = 0.518, 
           y = 119.45) +
  annotate("text", 
           label = "9 day-old long-day-grown seedling harvested 10h after light on on the 9th day", 
           x = 0.55 - 0.06, 
           y = 113.96) +
  annotate("text", 
           label = "9 day-old long-day-grown seedling harvested 10h after light on on the 9th day", 
           x = 0.54 - 0.06, 
           y = 110.19) +
  annotate("text", 
           label = "9 day-old long-day-grown seedling harvested 10h after light on on the 9th day", 
           x = 0.56 - 0.06, 
           y = 104.74) +
  annotate("text", 
           label = "7day old plate-grown long-day-grown seedling", 
           x = 0.29 - 0.015, 
           y = 2.19 + 3) +
  annotate("text", 
           label = "7day old plate-grown long-day-grown seedling", 
           x = 0.32 + 0.015, 
           y = 2.18 - 1) +
  annotate("text", 
           label = "4day old 24h light-grown seedling", 
           x = 0.41, 
           y = 13.15 -3) +
  
  xlim(.25, 0.6) +
  ylim(0, 150) +
  
  geom_hline(yintercept = mean(rna_seq$RPKM)) +
  annotate("text", 
           label = "Mean RPKM", 
           x = 0.29, 
           y = mean(rna_seq$RPKM)+5) +
  
  geom_vline(xintercept = mean(rna_seq$rpb)) +
  annotate("text", 
           label = "Mean rpb", 
           x = mean(rna_seq$rpb)+ 0.009, 
           y = 20) +
  
  ggtitle("RNA-Seq Expression Analysis of ACD11 gene", 
          subtitle = "Source: ATH1 micro arrey data from Arabidopsis plant") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, family = "mono", face = "bold.italic")) +
  theme(plot.subtitle = element_text(family = "mono")) +
  theme(legend.text = element_text(size = 9, family = "mono")) +
  theme(legend.title = element_text(size = 12, family = "mono", color = "black"))
  

# Plot_ly() ----------------------

library(plotly)

plot_ly(data = rna_seq,
        x = ~rpb,
        y = ~RPKM,
        type = "scatter",
        mode = "markers",
        color = ~`Reads mapped to locus`,
        size = ~`Total number of reads`,
        hoverinfo = "text",
        text = paste("Title: ", rna_seq$Title,
                     "<br>",
                     "rpb: ", rna_seq$rpb,
                     "<br>",
                     "RPKM: ", rna_seq$RPKM,
                     "<br>",
                     "Number of read mapped to locus: ", rna_seq$`Reads mapped to locus`,
                     "<br>",
                     "Total Number of Reads: ", rna_seq$`Total number of reads`),
        colors = c("white","yellow","green","red"),
        marker = list(line = list(color = "rgba(0, 0, 0, 1)",
                                  width = 1)),
        alpha = 1)







