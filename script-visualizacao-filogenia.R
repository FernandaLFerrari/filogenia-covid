#!/bin/bash/R
#Desenvolvido por: Fernanda Luiza Ferrari


## Vizualização de filogenia gerada através do IQTREE

## Bibliotecas

library(ape)
library(Biostrings)
library(tidyverse)
library(ggtree)
library(ComplexHeatmap)
library(circlize)
library(colorspace)
library(GetoptLong)
library(cowplot)
library(RColorBrewer)
library(ggnewscale)

## Importando a árvore consenso e anotações

nwk <- read.tree("sars-cov2-genome-aling-cut.fasta.contree")
annot = read.csv("ars-cov2-genome-annot.csv", sep =";")

head(annot)


## Criando cores para as regiões


colors = structure(c( "grey", terrain_hcl(7) ), 
                    names = c("GISAID-referência", 
                              "Grande Florianópolis", 
                              "Sul Catarinense", 
                              "Oeste Catarinense", 
                              "Vale do Itajaí ", 
                              "Norte Catarinense", 
                              "Serrana"))


## Árvore circular

p <- ggtree(nwk,layout='circular') %<+% annot + 
  geom_tippoint(aes(color=Regiao,show.legend=FALSE), size=6) + 
  scale_color_manual(values=colors, name="Região")  +
  theme(legend.title=element_text(size=6))  +
  theme(legend.position="bottom", legend.background = element_rect()) +
  geom_tiplab2(aes(label=linhagem), align=T,size=2.5, linesize=0.5) 


## Importando anotações de data

df= read.csv("C:/covid-atual/data.csv", sep =";")

rownames(df2) <- nwk$tip.label


## Anotando as datas

p2 <- p + new_scale_fill()


nb.cols <- 20
mycolors <- colorRampPalette(brewer.pal(6, "PRGn"))(nb.cols)
mycolors <- brewer.pal(n=7, name="Purples")


p3 = gheatmap(p2, df,  width=0.05, offset=0.0002,
              colnames_angle=95, colnames_offset_y = .15) +
  theme(legend.text = element_text(size = 10), 
        legend.title=element_text(size=8)) +
  scale_fill_manual(values = mycolors, name="Período")


## Destacando nós

p3 + geom_hilight(node=c(2:14), fill="red", alpha=.03) 



