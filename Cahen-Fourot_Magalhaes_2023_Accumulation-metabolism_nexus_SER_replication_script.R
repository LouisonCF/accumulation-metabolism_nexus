rm(list=ls(all=TRUE))#Erase from R environment all variables created previously
cat("\014") #Clean the terminal

library(readxl)
library(tidyverse)
library(RColorBrewer)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

###This script reproduces figures in Cahen-Fourot L., Magalhães N. 2023, The accumulation-metabolism nexus: internationalization, labour-capital relations, and material flows of French capitalism since the post-war era, Socio-Economic Review###
###If you use this code and data, please check the licences specified on Louison Cahen-Fourot's github page where this code and the data are made available: https://github.com/LouisonCF/accumulation-metabolism_nexus
###For any question on the code please email Louison Cahen-Fourot at lcahenfo_at_ruc.dk###

#1. Loading and formatting data####

Accumetab <- read_excel("Cahen-Fourot_Magalhaes_2023_Accumulation-metabolism_nexus_SER_replication_data.xlsx", sheet = "Data") #loading the excel file

Accumetab_long <- Accumetab %>% #Formatting in long format for use with ggplot2
  pivot_longer(cols=!Year,names_to="variables",values_to="values")

#2. Figures in the article####
##2.1 Domestic material consumption and material footprint####

fig_dmc_mf <- Accumetab_long %>% 
  filter(variables== "DMC" | variables =="MF") %>%
  ggplot(aes(x=Year,y=values)) +
  ylab("Tonnes of material") +
  geom_line(aes(linetype = variables), linewidth = 0.5) +
  scale_x_continuous(breaks=seq(1948, 2015, 2)) +
  scale_y_continuous(n.breaks=8, limits = c(0, NA)) +
  theme_bw(base_size = 7) +
  theme(axis.line = element_line(colour = 'black', linewidth = 0.25),
        text=element_text(size=7,  family="Arial"),
        axis.text.x = element_text(angle = -50, vjust = 0.25, hjust=0),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "bottom",
        legend.title=element_blank())
fig_dmc_mf

ggsave(filename = "fig_1.pdf",
       plot = fig_dmc_mf,
       device = cairo_pdf,
       width = 4.79,
       height = 3.59,
       units = "in",
       dpi = "retina"
)

##2.2 Material intensity and footprint adjusted-material intensity####

fig_mi_fami <- Accumetab_long %>% 
  filter(variables== "MI" | variables =="FAMI") %>% 
  ggplot(aes(x=Year,y=values)) +
  ylab("Kilos of material per 2010 € of GVA") +
  geom_line(aes(linetype = variables), linewidth = 0.5) + 
  scale_linetype_manual(values=c("dashed", "solid"))+
  scale_x_continuous(breaks=seq(1950, 2015, 2)) +
  scale_y_continuous(n.breaks=8,limits = c(0, NA)) +
  theme_bw(base_size = 7) +
  theme(axis.line = element_line(colour = 'black', linewidth = 0.25),
        text=element_text(size=7,  family="Arial"),
        axis.text.x = element_text(angle = -50, vjust = 0.25, hjust=0),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "bottom",
        legend.title=element_blank())
fig_mi_fami

ggsave(file="fig_2.pdf",
       plot = fig_mi_fami,
       device = cairo_pdf,
       width = 4.79,
       height = 3.59,
       units = "in",
       dpi = "retina"
)

##2.3 Disaggregated domestic material consumption and material footprint####

dmc_mf_disagg_long <- Accumetab %>%
  pivot_longer(cols=!Year, names_to = c("variables","type"),names_pattern = "(.*)_(.*)",values_to="values") %>% #Formatting in long format with two columns to distinguish both variables and types (dmc and mf)
  filter(type =="dmc" & variables !="Kaccu-MATeffigains" | type=="mf" & variables !="Kaccu-MATeffigains") #filtering to keep only the variables relevant to this figure

variables_names_dmc_mf_disagg <- c(
  `Biomass` = "a: Biomass",
  `Fossil fuels` = "b: Fossil fuels",
  `Metals` = "c: Metals",
  `Non-metals` = "d: Non-metals"
)

fig_dmc_mf_disagg <- dmc_mf_disagg_long %>% 
  ggplot(aes(x=Year,y=values)) +
  ylab("Tonnes of material") +
  geom_line(aes(linetype = type),linewidth = 0.5) +
  scale_x_continuous(breaks=seq(1948, 2015, 4)) + 
  scale_y_continuous(n.breaks=15,limits = c(0, NA)) +
  theme_bw(base_size = 7) +
  facet_wrap(vars(variables), labeller = as_labeller(variables_names_dmc_mf_disagg)) + 
  theme(axis.line = element_line(colour = 'black', size = 0.25),
        text=element_text(size=7,  family="Arial"),
        axis.text.x = element_text(angle = -60, vjust = 0.15, hjust=0.25),
        axis.title.y = element_text(margin = margin(r = 10)),
        strip.background = element_blank(), 
        strip.placement = "outside",
        legend.position = "bottom",
        legend.title=element_blank())
fig_dmc_mf_disagg

ggsave(file="fig_3.pdf",
       plot = fig_dmc_mf_disagg,
       device = cairo_pdf,
       width = 4.79,
       height = 3.59,
       units = "in",
       dpi = "retina"
)

##2.4 Physical trade balance and raw trade balance####

variables_names_ptbrtb <- c(
  `PTB` = "a: PTB",
  `RTB` = "b: RTB",
  `RTB-PTB` = "c: RTB-PTB"
)

fig_ptbrtb <- Accumetab %>%
  pivot_longer(cols=!Year, names_to = c("variables","type"),names_pattern = "(.*)_(.*)",values_to = "values") %>%
  filter(variables== "PTB" & type!="ironore" | variables =="RTB" & type!="ironore"| variables == "RTB-PTB" & type!="ironore") %>% 
  ggplot(aes(x=Year,y=values)) +
  ylab("Tonnes of material") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) + #To have a darker and thicker line at 0
  geom_line(aes(linetype = type),linewidth = 0.5) + 
  scale_linetype_manual(values=c("dashed", "solid"))+
  scale_x_continuous(breaks=seq(1948, 2015, 5)) +
  scale_y_continuous(n.breaks=10,labels = scales::scientific) +
  theme_bw(base_size = 7) +
  facet_wrap(vars(variables),labeller=as_labeller(variables_names_ptbrtb)) +
  theme(axis.line = element_line(colour = 'black', size = 0.25),
        text=element_text(size=7,  family="Arial"),
        axis.text.x = element_text(angle = -60, vjust = 0.15, hjust=0.25),
        axis.title.y = element_text(margin = margin(r = 10)),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "bottom",
        legend.title=element_blank())
fig_ptbrtb

ggsave(file="fig_4.pdf",
       plot = fig_ptbrtb,
       device = cairo_pdf,
       width = 4.79,
       height = 3.59,
       units = "in",
       dpi = "retina"
)

##2.5 Synthesis figure####

fig_synthesis <- grViz(diagram="digraph flowchart {
  node [fontname = arial, shape=oval,fixedsize=false, fontsize=52, margin=0.5]#define node aesthetics
  
  #fixedsize=false
  
  graph [layout = dot]
  
  #set up node layout
  tab1 [label='@@1']
  tab2 [label = '@@2']
  tab3 [label='@@3', fontsize=92, penwidth=12]
  tab4 [label='@@4']
  tab5 [label='@@5']
  tab6 [label='@@6']
  tab7 [label='@@7']
  tab8 [label='@@8']
  tab9 [label='@@9',style=filled,color=darkslategrey,fontcolor=white]
  tab10 [label='@@10',style=filled,color=darkslategrey,fontcolor=white]
  tab11 [label='@@11',style=filled,color=darkslategrey,fontcolor=white]
  tab12 [label='@@12',style=filled,color=darkslategrey,fontcolor=white]
  tab13 [label='@@13',style=filled,color=darkslategrey,fontcolor=white]
  tab14 [label='@@14', shape=box, fontsize=92, style=filled,color=darkslategrey,fontcolor=white]
  tab15 [label='@@15',shape=box]
  tab16 [label='@@16',shape=box]
  tab17 [label='@@17',shape=box]
  tab18 [label='@@18',shape=box]
  tab19 [label='@@19',shape=box]
  tab20 [label='@@20',fontsize=92, shape=box,penwidth=12]
  tab21 [label='@@21',shape=box]
  tab22 [label='@@22']
  
  subgraph{
  tab1 -> {tab2 tab6} [penwidth=6, arrowsize=3];
  tab2 -> tab3 [penwidth=6, arrowsize=3];
  tab3 -> tab4 [penwidth=6, arrowsize=3];
  tab4 -> tab5 [penwidth=6, arrowsize=3];
  tab5 -> {tab6 tab9 tab11} [penwidth=6, arrowsize=3];
  tab6 -> {tab3 tab7 tab9 tab22} [penwidth=6, arrowsize=3];
  tab7 -> tab8 [penwidth=6, arrowsize=3];
  tab8 -> {tab5 tab12} [penwidth=6, arrowsize=3];
  tab9 -> {tab10 tab11 tab14} [penwidth=6, arrowsize=3];
  tab10 -> {tab11 tab14} [penwidth=6, arrowsize=3];
  tab11 -> tab14 [penwidth=6, arrowsize=3];
  tab13 -> tab14 [penwidth=6, arrowsize=3];
  tab22 -> {tab2 tab6} [penwidth=6, arrowsize=3];
  subgraph {
rank = same; tab12; tab13;
  }}
subgraph{
  tab14 -> tab15 [penwidth=6, arrowsize=3];
  tab15 -> tab16 [penwidth=6, arrowsize=3];
  tab16 -> tab14 [penwidth=6, arrowsize=3];
  tab12 -> tab16 [penwidth=6, arrowsize=3];
  tab16 -> tab17 [penwidth=6, arrowsize=3];
  tab15 -> tab18 [penwidth=6, arrowsize=3];
  tab16 -> tab15 [penwidth=6, arrowsize=3];
  tab15 -> tab19 [penwidth=6, arrowsize=3];
  tab18 -> tab16 [penwidth=6, arrowsize=3];
  tab19 -> tab20 [penwidth=6, arrowsize=3];
  tab17 -> tab20 [penwidth=6, arrowsize=3];
  tab20 -> tab21 [penwidth=6, arrowsize=3];
  tab21 -> tab16 [penwidth=6, arrowsize=3]}
}

[1]: 'Cheap and abundant fossil energy'
[2]: 'High productivity gains'
[3]: 'Fordist social compromise'
[4]: 'Welfare state'
[5]:'Income redistribution towards labor'
[6]:'Domestic mass prod-mass conso synchronization'
[7]:'Material basis predominantly domestic'
[8]:'Strong structural power of labour'
[9]: 'Saturation of domestic demand'
[10]: 'Decrease in productivity gains'
[11]:'Decrease in profit rates'
[12]: 'Shift in energy mix towards oil'
[13]: 'Collapse of Bretton-Woods'
[14]:'Offshoring-financialization nexus'
[15]:'Increase in foreign material flows'
[16]: 'Weakening structural power of labour'
[17]:'Shift in income distribution towards capital'
[18]:'Stagnation in domestic material basis'
[19]:'Cheap imported goods'
[20]:'Neoliberal social compromise'
[21]:'Transformation of the state'
[22]: 'High investment'
")
fig_synthesis

fig_synthesis %>%
  export_svg %>% 
  charToRaw %>% 
  rsvg_pdf("fig_5.pdf")

##2.6 Capital accumulation - material efficiency gains####

fig_kaccu_mateffigains <- Accumetab_long %>%
  filter(variables=="K accu rate - MF material effi gains (5y moving average)" | variables=="K accu rate - DMC material effi gains (5y moving average)") %>%
  ggplot(aes(x=Year,y=values)) +
  ylab("Percentage points") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_line(aes(linetype = variables), linewidth = 0.5) +
  scale_x_continuous(breaks=seq(1950, 2015, 2)) +
  scale_y_continuous(n.breaks=10) +
  theme_bw(base_size = 7) +
  theme(axis.line = element_line(colour = 'black', linewidth = 0.25),
        text=element_text(size=7,  family="Arial"),
        axis.text.x = element_text(angle = -50, vjust = 0.25, hjust=0),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "bottom",
        legend.title=element_blank()) +
  guides(linetype=guide_legend(ncol=1))
fig_kaccu_mateffigains

ggsave(file="fig_6.pdf",
       plot = fig_kaccu_mateffigains,
       device = cairo_pdf,
       width = 4.79,
       height = 3.59,
       units = "in",
       dpi = "retina"
)

#3. Figures in appendix####

##3.1 MF with and without 1997 correction####
fig_mf_1997 <- Accumetab_long %>%
  filter(variables == "MF_not1997corrected" | variables=="MF_1997corrected") %>%
  ggplot(aes(x=Year,y=values)) +
  ylab("Tonnes of material") +
  geom_line(aes(linetype = variables),linewidth = 0.5) +
  scale_x_continuous(breaks=seq(1970, 2015, 2),expand = c(0, 0), limits = c(1970,2015)) + 
  scale_y_continuous(n.breaks=20, label=scales::scientific,expand = c(0, 0),limits = c(0,1.5e+09)) +
  theme_bw(base_size = 7) +
  theme(axis.line = element_line(colour = 'black', linewidth = 0.25),
        text=element_text(size=7,  family="Arial"),
        axis.text.x = element_text(angle = -60, vjust = 0.15, hjust=0.25),
        axis.title.y = element_text(margin = margin(r = 10)),
        strip.background = element_blank(), 
        strip.placement = "outside",
        legend.position = "bottom",
        legend.title=element_blank())
fig_mf_1997

ggsave(file="fig_A1.pdf",
       plot = fig_mf_1997,
       device = cairo_pdf,
       width = 4.79,
       height = 3.59,
       units = "in",
       dpi = "retina"
)

##3.2 PTB of iron ore####
fig_ptb_ironore <- Accumetab_long %>%
  dplyr::filter(variables=="PTB_ironore") %>%
  ggplot(aes(x=Year,y=values)) +
  ylab("Tonnes of iron") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_line(aes(linetype = variables),size = 0.75) +
  scale_x_continuous(breaks=seq(1948, 1977, 2),expand = c(0, 0), limits = c(1948,1977)) + 
  scale_y_continuous(n.breaks=20, label=scales::scientific,expand = c(0, 0),limits = c(-2.6e+07,4e+06)) +
  theme_bw(base_size = 7) +
  theme(axis.line = element_line(colour = 'black', linewidth = 0.25),
        text=element_text(size=7,  family="Arial"),
        axis.text.x = element_text(angle = -60, vjust = 0.15, hjust=0.25),
        axis.title.y = element_text(margin = margin(r = 10)),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "bottom",
        legend.title = element_blank(),)
fig_ptb_ironore

ggsave(file="fig_A2.pdf",
       plot = fig_ptb_ironore,
       device = cairo_pdf,
       width = 4.79,
       height = 3.59,
       units = "in",
       dpi = "retina"
)

##3.3 Comparison of MF estimates until 1975####
fig_mf_19701975 <- Accumetab_long %>%
  filter(variables=="MF1970"|variables=="MF1971"|variables=="MF1972"|variables=="MF1973"|variables=="MF1974"|variables=="MF1975") %>%
  ggplot(aes(x=Year,y=values)) +
  ylab("Tonnes of material") +
  geom_line(aes(linetype = variables), linewidth = 0.5) +
  scale_x_continuous(breaks=seq(1948, 1975, 2),expand = c(0, 0), limits = c(1948,1975)) + 
  scale_y_continuous(n.breaks=10, label=scales::scientific,expand = c(0, 0),limits = c(0,1.2e+09)) +
  theme_bw(base_size = 7) +
  theme(axis.line = element_line(colour = 'black', linewidth = 0.25),
        text=element_text(size=7,  family="Arial"),
        axis.text.x = element_text(angle = -60, vjust = 0.15, hjust=0.25),
        axis.title.y = element_text(margin = margin(r = 10)),
        strip.background = element_blank(), 
        strip.placement = "outside",
        legend.position = "bottom",
        legend.title = element_blank())
fig_mf_19701975

ggsave(file="fig_A3.pdf",
       plot = fig_mf_19701975,
       device = cairo_pdf,
       width = 4.79,
       height = 3.59,
       units = "in",
       dpi = "retina"
)


##3.4 Comparison of MF metal estimates until 1981####
fig_mf_metal_19751981 <- Accumetab_long %>%
  filter(variables=="mf_metals_1975"|variables=="mf_metals_1976"|variables=="mf_metals_1977"|variables=="mf_metals_1978"|variables=="mf_metals_1979"|variables=="mf_metals_1980") %>%
  ggplot(aes(x=Year,y=values)) +
  ylab("Tonnes of material") +
  geom_line(aes(linetype = variables), linewidth = 0.5) +
  scale_x_continuous(breaks=seq(1948, 1981, 2),expand = c(0, 0), limits = c(1948,1981)) + 
  scale_y_continuous(n.breaks=10, label=scales::scientific,expand = c(0, 0),limits = c(0,7e07)) +
  theme_bw(base_size = 7) +
  theme(axis.line = element_line(colour = 'black', linewidth = 0.25),
        text=element_text(size=7,  family="Arial"),
        axis.text.x = element_text(angle = -60, vjust = 0.15, hjust=0.25),
        axis.title.y = element_text(margin = margin(r = 10)),
        strip.background = element_blank(), 
        strip.placement = "outside",
        legend.position = "bottom",
        legend.title = element_blank())
fig_mf_metal_19751981

ggsave(file="fig_A4.pdf",
       plot = fig_mf_metal_19751981,
       device = cairo_pdf,
       width = 4.79,
       height = 3.59,
       units = "in",
       dpi = "retina"
)


##3.5 MI and FAMI disaggregated#### 
mi_fami_disagg_long <- Accumetab %>%
  pivot_longer(cols=!Year, names_to = c("variables","type"),names_pattern = "(.*)_(.*)",values_to="values") %>% #Formatting in long format with two columns to distinguish both variables and types (dmc and mf)
  filter(type =="mi" & variables !="Kaccu-MATeffigains" | type=="fami" & variables !="Kaccu-MATeffigains") %>%
  filter(Year > 1949)

variables_names_mi_fami_disagg <- c(
  `Biomass` = "a: Biomass",
  `Fossil fuels` = "b: Fossil fuels",
  `Metals` = "c: Metals",
  `Non-metals` = "d: Non-metals"
)

fig_mi_fami_disagg <- mi_fami_disagg_long %>% 
  ggplot(aes(x=Year,y=values)) +
  ylab("Kilos of material per 2010 € of GVA") +
  geom_line(aes(linetype = type), linewidth = 0.5) +
  scale_linetype_manual(values=c("dashed", "solid"))+
  scale_x_continuous(breaks=seq(1950, 2015, 4)) + 
  scale_y_continuous(n.breaks=10,limits = c(0, NA)) +
  theme_bw(base_size = 7) +
  facet_wrap(vars(variables),labeller=as_labeller(variables_names_mi_fami_disagg)) + 
  theme(axis.line = element_line(colour = 'black', linewidth = 0.25),
        text=element_text(size=7,  family="Arial"),
        axis.text.x = element_text(angle = -60, vjust = 0.15, hjust=0.25),
        axis.title.y = element_text(margin = margin(r = 10)),
        strip.background = element_blank(), 
        strip.placement = "outside",
        legend.position = "bottom",
        legend.title = element_blank())
fig_mi_fami_disagg

ggsave(file="fig_A5.pdf",
       plot = fig_mi_fami_disagg,
       device = cairo_pdf,
       width = 4.79,
       height = 3.59,
       units = "in",
       dpi = "retina"
)

##3.6 DMC-to-DE and MF-to_DE####
fig_dmcmfde <- Accumetab_long %>%
  filter(variables == "DMC_DE" | variables == "MF_DE") %>%
  ggplot(aes(x=Year,y=values)) +
  ylab("DMC and MF in % of DE") +
  geom_line(aes(linetype = variables), linewidth = 0.5) +
  scale_x_continuous(breaks=seq(1948, 2015, 2),expand = c(0, 0), limits = c(1948,2015)) + 
  scale_y_continuous(n.breaks=20, label=scales::percent,expand = c(0, 0), limits = c(0,2.1)) +
  theme_bw(base_size = 7) +
  theme(axis.line = element_line(colour = 'black', linewidth = 0.25),
        text=element_text(size=7,  family="Arial"),
        axis.text.x = element_text(angle = -60, vjust = 0.15, hjust=0.25),
        axis.title.y = element_text(margin = margin(r = 10)),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "bottom",
        legend.title = element_blank())
fig_dmcmfde

ggsave(file="fig_A6.pdf",
       plot = fig_dmcmfde,
       device = cairo_pdf,
       width = 4.79,
       height = 3.59,
       units = "in",
       dpi = "retina"
)

##3.7 PTB and RTB vs. monetary trade balance####
fig_ptbrtbmtb <- Accumetab %>%
  dplyr::select(PTB_total,RTB_total,Mon_TB,Year) %>%
  mutate(PTB_total = PTB_total/1000) %>% #dividing by 1000 to express PTB and RTB in thousand of tonnes so scale matches with MTB
  mutate(RTB_total = RTB_total/1000) %>%
  pivot_longer(cols=!Year,names_to="variables",values_to="values") %>%
  filter(Year > 1949) %>%
  ggplot(aes(x=Year,y=values)) +
  ylab("Thousand of tonnes (PTB and RTB) \n and million of 2010 euros (MTB)") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_line(aes(linetype = variables),size = 0.75) +
  scale_x_continuous(breaks=seq(1950, 2015, 4)) + 
  scale_y_continuous(n.breaks=20, label=scales::scientific_format()) +
  theme_bw(base_size = 7) +
  theme(axis.line = element_line(colour = 'black', linewidth = 0.25),
        text=element_text(size=7,  family="Arial"),
        axis.text.x = element_text(angle = -60, vjust = 0.15, hjust=0.25),
        axis.title.y = element_text(margin = margin(r = 10)),
        strip.background = element_blank(), 
        strip.placement = "outside",
        legend.position = "bottom",
        legend.title = element_blank())
fig_ptbrtbmtb

ggsave(file="fig_A7.pdf",
       plot = fig_ptbrtbmtb,
       device = cairo_pdf,
       width = 4.79,
       height = 3.59,
       units = "in",
       dpi = "retina"
)

##3.8 Shares of imported intermediate consumption and final consumption in resp. total intermediate consumption and final demand####
fig_ic_fc_shares <- Accumetab_long %>% 
  filter(variables =="Interm_cons_basic" & Year > 1969 | variables=="Final_cons_basic" & Year > 1969 ) %>%
  ggplot(aes(x=Year,y=values)) +
  ylab("Shares of imported intermediate and final consumption \n in total intermediate and final consumption") +
  geom_line(aes(linetype = variables), linewidth = 0.5) + 
  scale_x_continuous(breaks=seq(1970, 2015, 2)) +
  scale_y_continuous(n.breaks=10, label=scales::percent) +
  theme_bw(base_size = 7) +
  theme(axis.line = element_line(colour = 'black', linewidth = 0.25),
        text=element_text(size=7,  family="Arial"),
        axis.text.x = element_text(angle = -60, vjust = 0.15, hjust=0.25),
        axis.title.y = element_text(margin = margin(r = 10)),
        strip.background = element_blank(), 
        strip.placement = "outside",
        legend.position = "bottom",
        legend.title = element_blank())
fig_ic_fc_shares

ggsave(file="fig_A8.pdf",
       plot = fig_ic_fc_shares,
       device = cairo_pdf,
       width = 4.79,
       height = 3.59,
       units = "in",
       dpi = "retina"
)


##3.9 Financialization: wage share, margin rate, investment rate, capital returns and financial assets of NFCs####
fig_fin_groups <- Accumetab_long %>% #creating grouping variables to facet related variables together
  filter(variables=="Wage_share" | variables=="Margin_rate" | variables== "Capital_returns_paid"| variables== "Capital_returns_received"|variables== "Capital_returns_paid_net" |variables== "Investment_rate"| variables== "Fin_assets_nonfin_assets" | variables=="Fin_assets_fixed_assets") %>%
  filter(Year ==1948) %>%
  dplyr::select(-c(Year,values))
fig_fin_groups$group <- c("Wage share","Margin and investment rates","Capital returns","Capital returns","Capital returns","Margin and investment rates","Financial assets","Financial assets")

group_names_financialization <- c(
  `Capital returns` = "a: Capital returns",
  `Financial assets` = "b: Financial assets",
  `Margin and investment rates` = "c: Margin and investment rates",
  `Wage share` = "d: Wage share"
)

fig_fin <- Accumetab_long %>%
  filter(variables=="Wage_share" | variables=="Margin_rate" | variables== "Capital_returns_paid"| variables== "Capital_returns_received"|variables== "Capital_returns_paid_net" |variables== "Investment_rate"| variables== "Fin_assets_nonfin_assets" | variables=="Fin_assets_fixed_assets") %>%
  left_join(fig_fin_groups,by="variables") %>%
  filter(Year > 1949) %>% 
  ggplot(aes(x=Year,y=values)) +
  ylab("Percentages") +
  geom_line(aes(linetype = variables), linewidth = 0.5) +
  scale_x_continuous(breaks=seq(1950, 2015, 4)) + 
  scale_y_continuous(n.breaks=8,label=scales::percent_format(accuracy = 1)) +
  theme_bw(base_size = 7) +
  facet_wrap(vars(group),scales=c("free"),labeller = as_labeller(group_names_financialization)) +
  theme(axis.line = element_line(colour = 'black', linewidth = 0.25),
        text=element_text(size=7,  family="Arial"),
        axis.text.x = element_text(angle = -60, vjust = 0.15, hjust=0.25),
        axis.title.y = element_text(margin = margin(r = 10)),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "bottom",
        legend.title = element_blank()) +
  guides(linetype=guide_legend(ncol=3))
fig_fin

ggsave(file="fig_A9.pdf",
       plot = fig_fin,
       device = cairo_pdf,
       width = 4.79,
       height = 3.59,
       units = "in",
       dpi = "retina"
)

##3.10 Comparison of capital accumulation rates computed with Penn World Tables data and Ameco data####
fig_kaccu_comparison <- Accumetab_long %>%
  filter(variables=="Accu_rate" | variables=="Accu_rate_NFCF_ameco" | variables=="Accu_rate_NCF_ameco") %>%
  ggplot(aes(x=Year,y=values)) +
  ylab("Capital accumulation rate") +
  geom_line(aes(linetype = variables, colour=variables), linewidth = 0.5) +
  scale_color_grey(start = 0.2,end = 0.8) +
  geom_point(aes(shape=variables,colour=variables),size=1) +
  scale_color_grey(start = 0.2,end = 0.8) +
  scale_x_continuous(breaks=seq(1950, 2015, 2)) +
  scale_y_continuous(n.breaks=10) +
  theme_bw(base_size = 7) +
  theme(axis.line = element_line(colour = 'black', linewidth = 0.25),
        text=element_text(size=7,  family="Arial"),
        axis.text.x = element_text(angle = -50, vjust = 0.25, hjust=0),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "bottom",
        legend.title=element_blank())
fig_kaccu_comparison

ggsave(file="fig_A10.pdf",
       plot = fig_kaccu_comparison,
       device = cairo_pdf,
       width = 4.79,
       height = 3.59,
       units = "in",
       dpi = "retina"
)

##3.11 Comparison of capital depreciation rates from Penn World Tables and Ameco####
fig_depkrate_comparison <- Accumetab_long %>%
  filter(variables=="depkrate" | variables=="depKrate_ameco") %>%
  ggplot(aes(x=Year,y=values)) +
  ylab("Capital accumulation rate") +
  geom_line(aes(linetype = variables), linewidth = 0.5) +
  scale_x_continuous(breaks=seq(1950, 2015, 2)) +
  scale_y_continuous(n.breaks=10) +
  theme_bw(base_size = 7) +
  theme(axis.line = element_line(colour = 'black', linewidth = 0.25),
        text=element_text(size=7,  family="Arial"),
        axis.text.x = element_text(angle = -50, vjust = 0.25, hjust=0),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "bottom",
        legend.title=element_blank())
fig_depkrate_comparison

ggsave(file="fig_A11.pdf",
       plot = fig_depkrate_comparison,
       device = cairo_pdf,
       width = 4.79,
       height = 3.59,
       units = "in",
       dpi = "retina"
)

