###Arnold Lambisia
##Laikipia MAnuscript figures
##copyright 2022


#load libraries
library(ggplot2); library(tidyverse); library(epiR); library(lubridate);library(data.table);library(artyfarty);library(scales); library(patchwork); library(rio); library(epitools)

###load data
LKP_dta <- import("DATA1.xlsx") %>% 
  mutate(date_collected = as.Date(date_collected, "%d/%m/%Y"))

#subset data for panel A
LKP_dta2 <- LKP_dta %>%
  filter(!is.na(VOC_lineage)) %>%
  mutate(datecollection=as.Date(date_collected, "%d/%m/%Y"))%>%
  mutate(Biweekly=as.Date(cut(datecollection, breaks= "1 month", start.on.monday=T))) %>% 
  mutate(present=1)%>%
  group_by(Biweekly,VOC_lineage)%>%
  summarise(Freq=sum(present))%>%
  arrange(Biweekly)

#colr panels
color_global=c("#808000","#CCCCFF", "orange", "purple", "#FF0000","blue","#F2D2BD","#000000","green","aquamarine",
               "#0F4D92","#55ACEE","#CD853F", "#0000FF","#808000","#FFFF00","magenta",
               "#8c510a", "#c7eae5", "#01665e", "#ffffff", "#fdae61", "purple", "#C0C0C0","salmon2",
               "#d9f0a3","#ffeda0",   "#8c6bb1", "aquamarine", "#525252", "#3690c0","#000080","#a50f15","#800000","#FA8072","#00FF00",
               "brown",
               "#330000")

# Set period to be observed on the graphics
min <-as.Date("2020-12-25")
max <-as.Date("2022-01-05")

#create panel a plot
plot_variant_counts <- ggplot(LKP_dta2)+
  geom_bar(aes(x=Biweekly, y=Freq, fill=VOC_lineage), stat = "identity", size = 4)+
  scale_fill_manual(values=color_global)+
  labs(y="Sequenced samples", x="")+
  theme_scientific()+
  scale_x_date(breaks ="1 month", date_minor_breaks="1 month", labels = date_format("%b-%Y"), limits = c(min,max))+
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 9, face="bold"),
        legend.position = "right",
        legend.key.size = unit(0.25, "cm"),
        legend.spacing.x = unit(0.25, 'cm'),
        legend.spacing.y = unit(0.25, 'cm'),
        legend.text = element_text(size = 20),
        legend.title =element_text(size = 20),
        legend.box.background = element_blank())+
  guides(fill=guide_legend(ncol=1, title = "Variant of concern"), size=T)
plot_variant_counts



###subset data for panel B

LKP_dta3 <- LKP_dta %>%
  filter(!is.na(VOC_lineage)) %>%
  filter(!lineage == "None") %>% 
  mutate(datecollection=as.Date(date_collected, "%d/%m/%Y"))%>%
  mutate(Biweekly=as.Date(cut(datecollection, breaks= "1 month", start.on.monday=T))) %>% 
  mutate(lineage = factor(lineage, levels = c("B.1.1.7", "AY.51", "AY.44",
                                              "AY.58","B.1.617.2", "AY.116","AY.46","AY.16", "BA.1", "BA.1.1")))%>%
  mutate(present=1)%>%
  group_by(Biweekly,lineage)%>%
  summarise(Freq=sum(present))%>%
  arrange(Biweekly) 

#create panel b plots
plot_lineage <- ggplot(LKP_dta3, aes(x=Biweekly, y=Freq, fill=lineage))+
  geom_bar(stat = "identity", position = "fill", size = 4)+
  scale_fill_manual(values=color_global)+
  labs(y="Proportion", x="Month in 2021")+
  theme_scientific()+
  scale_x_date(breaks ="1 month", date_minor_breaks="1 month", labels = date_format("%b-%Y"), limits = c(min,max))+
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 14, angle = 90),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 9, face="bold"),
        legend.position = "right",
        legend.key.size = unit(0.25, "cm"),
        legend.spacing.x = unit(0.25, 'cm'),
        legend.spacing.y = unit(0.25, 'cm'),
        legend.text = element_text(size = 20),
        legend.title =element_text(size = 20),
        legend.box.background = element_blank())+
  guides(fill=guide_legend(ncol=1, title = "lineages"), size=T)
plot_lineage

#merge plots
Figure2 <- plot_variant_counts / plot_lineage + 
  plot_annotation(tag_levels = "A")
Figure2




##### supplementary Figure 1
LKP_ct_dta <- read_excel("DATA2.xlsx") %>% 
rename("CT_Value" = daan_N_gene_ct)

#box plots
CT_box_plot.plot <- LKP_ct_dta %>% 
  mutate(sequenced = factor(sequenced, levels = c("Not sequenced", "Not sequenced (Low DNA Conc.)", "Sequenced but not classified", "Sequenced successfully"), ordered = T)) %>% 
  ggplot(aes(sequenced, as.integer(Ct_value)))+
  geom_boxplot()+
  geom_jitter(aes(), size = 5, width = .3)+
  labs(y = "Ct Value", x = "")+
  scale_y_continuous(breaks = seq(0,40, 5))+
  scale_x_discrete(guide = guide_axis(n.dodge=2, check.overlap = T))+
  theme_classic()+
  theme(panel.grid = element_blank())+
  theme(axis.title.x = element_text(size = 24, face = "bold"),
        axis.text = element_text(size = 24, face = "bold"),
        axis.text.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 24, face = "bold"),
        strip.text = element_text(size = 24, face = "bold"),
        strip.background = element_rect(linetype = "blank", fill = NA),
        legend.title = element_text(size = 24, face = "bold"),
        legend.text = element_text(size = 24, face = "bold"),
        legend.position = "right")
CT_box_plot.plot


