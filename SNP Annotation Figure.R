# Fig1 ------------------------------

library(tidyverse)
library(readxl)


Fig1 <- read_xlsx("snp.xlsx")


Fig1$Snp_Name <- as.factor(Fig1$Snp_Name) 


Pal_col <- c("#5A9EDB", "#1E598F","#DBAA5A",
             "#FFCF82","#8F692C")



Fig1 %>%
  mutate(percent = Count * 100 / sum(Count)) %>%
  ggplot() +
  aes(x = Snp_Name, weight = percent) +
  geom_bar(fill = Pal_col, width = 0.5,
           position = position_dodge(0.2)) +
  ylim(0, 40) +
  xlab("") +
  ylab("% of Snp") +
  coord_flip() +
  theme(panel.grid.major.x = element_blank())+
  theme(panel.grid.minor.x = element_blank())+
  theme(panel.grid.major.y = element_blank())+
  theme(panel.background = element_blank()) +
  annotate("text",
           label = "10.3% ",
           x = 5,
           y = 10.3 + 2) +
  annotate("text",
           label = "32.1% ",
           x = 4,
           y = 32.1 + 2) +
  annotate("text",
           label = "5.13% ",
           x = 3,
           y = 5.13 + 2) +
  annotate("text",
           label = "32.1% ",
           x = 2,
           y = 32.1 + 2) +
  annotate("text",
           label = "20.5% ",
           x = 1,
           y = 20.5 + 2) 


ggsave("SNP Annatation.png", width = 7, height = 5, 
       units = "in",dpi = 350, bg = "white")


# Fig2 ---------------------------

Fig2 <- read_xlsx("snp.xlsx", sheet = "server")

Fig2$server_name <- as.factor(Fig2$server_name)
Fig2$effect <- as.factor(Fig2$effect)  
  


ggplot(Fig2) +
 aes(x = server_name, 
     fill = Effect, 
     weight = amount) +
 geom_bar(width = 0.6) +
  ylim(0,8) +
 scale_fill_manual(values = c(Bening = "#FFCCA8", 
                              Damaging = "#5ED1DB")) +
 labs(x = "Different Prediction Algorithms", 
      y = "Number of nsSNP Result") +
 coord_flip() +
 theme_minimal() +
 theme(axis.title.y = element_text(size = 12L, 
                                   face = "bold"), 
       axis.title.x = element_text(size = 12L, 
                                   face = "bold")) +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.x = element_blank())


ggsave("SNP Server Annatation.png", width = 7, height = 5,
       units = "in",dpi = 350, bg = "white")  

  