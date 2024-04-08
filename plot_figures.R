#### FIGURES

rm(list = ls())

library(readxl)
library(tidyr)
library(tidyverse)
#library(plyr) #rbind.fill
library(dbplyr)
library(dplyr) #arrange 
library(rlang)
library(ggthemes)
library(ggplot2)
library(scales)
library(xtable) #print descriptive tables 
library(psych) #descriptive statistics 
library(fixest) #fixed-effects panel data estimation 
library(data.table)

#install.packages("pdftools")
library(pdftools)

#install.packages("readr")
library(readr)

#library(openxlsx)
#library(httr)
#library(XML)

setwd("C:/Users/u0149894/Desktop/Code")

#########
#Load Data 
#########

load(paste0(getwd(),"/data/tariffs_pel_subreg.Rdata")) #subregional electricity prices
load(paste0(getwd(),"/data/npv_final.Rdata"))
load(paste0(getwd(),"/data/b_subreg.Rdata"))
load(paste0(getwd(),"/data/npv_final.Rdata")) #npv data 
load(paste0(getwd(),"/data/PV_total_estdata_monthly.Rdata")) #full data monthly 
load(paste0(getwd(),"/data/PV_total_estdata.Rdata")) #full data monthly 


yrange <- 2008:2019
start_date <- as.Date("2008-01-01")  # Specify the desired start date
end_date <- as.Date("2019-12-01")  # Specify the desired end date

################################################################################
#plot pel, benefits and npv 
################################################################################



#Plot regional electricity prices
pel_reg_df <- tariffs_pel_subreg %>% 
  subset(year %in% yrange) %>%
  select(year,month,date,region,pel_mix_kwh,pel_mix_retail_kwh) %>%
  group_by(year,month,date,region) %>%
  summarise(pel_mix_kwh = first(pel_mix_kwh),
            pel_mix_retail_kwh = first(pel_mix_retail_kwh)) %>%
  ungroup() %>%
  pivot_longer(cols =c(pel_mix_kwh,pel_mix_retail_kwh),
               names_to="pel",
               values_to="value")

pel_reg_df %>% group_by(region, pel) %>%
  summarise(sd = sd(value),
            mean = mean(value))

pel_reg_plot <- pel_reg_df %>%
  ggplot(aes(x = date, y = value, group = interaction(pel, region))) + 
  geom_line(aes(color = region, linetype = pel)) +
  labs(x = "", y = "Electricity price in EUR/KWh", color = "", linetype="") +
  coord_cartesian(xlim = c(start_date, end_date), expand = FALSE) + 
  scale_x_date(date_labels = "%b/%y", limits = c(start_date, end_date), breaks = "6 months") +
  theme_bw() +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'bottom',
        legend.text = element_text(size = 12)) +  # Adjust the font size here
  scale_linetype_manual(values = c("solid", "dashed"), label=c("w/ distr. tarif","w/o distr. tariff")) +
  labs(color = "")
print(pel_reg_plot)

ggsave("pel_IV_plot.pdf", pel_reg_plot, path = paste0(getwd(),"/data/plots"), height=5, width=8.75)  



#plot regional distribution tariffs 
pel_subreg_df <- tariffs_pel_subreg %>% 
  subset(year %in% yrange) %>%
  select(year,month,date,region,dso,pel_mix_regional_kwh,pel_mix_kwh) %>%
  subset(!(dso%in%c("AGEM","EV/GHA4")))

unique(pel_subreg_plot$dso)

pel_subreg_plot <- pel_subreg_df %>%
  ggplot(aes(x=date, group=dso, color=region)) + 
  geom_line(aes(y = pel_mix_regional_kwh), alpha=0.6, linetype="solid", size=0.2) +
  geom_point(aes(y = pel_mix_regional_kwh), alpha=0.2, size=0.2, shape=18) +
  geom_line(aes(y = pel_mix_kwh, linetype=region), color="black", alpha=0.6, size=0.4) + #
  geom_point(aes(y = pel_mix_kwh), color="black", alpha=0.4, size=0.4, shape=18) +
  labs(x="",y="Electricity price in EUR/KWh", color="") +
  coord_cartesian(xlim = c(start_date, end_date), expand=F) + 
  scale_x_date(date_labels = "%b/%y", limits = c(start_date, end_date), breaks="6 months") +
  theme_bw() +   #+ xlim(2008, 2019) +theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Rotate x-axis labels by 90 degrees
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = 'bottom') +
  theme(legend.title = element_blank()) +
  scale_fill_discrete(labels=c("Flanders average", "Wallonia average", "Flanders", "Wallonia"))
print(pel_subreg_plot)

ggsave("pel_subreg_plot.pdf", pel_subreg_plot, path = paste0(getwd(),"/data/plots"), height=5, width=8.75)  

pel_subreg_pres_plot <- pel_subreg_plot +
  theme(axis.text = element_text(size = 12), plot.title=element_text(hjust=0.5),
        legend.box.spacing = unit(-10, "pt")) +
  theme(axis.text = element_text(size = 12), plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.text = element_text(size = 12)) 
ggsave("pel_subreg_plot_pres.pdf",pel_subreg_pres_plot, path = paste0(getwd(),"/data/plots"), height=4, width=10)

#plot sub-regional benefits 
benefits_df <- full_join(b_subreg,npv_final,by=c("year","month","region"))

benefits_df  <- benefits_df %>%
  subset(year%in%2008:2019 & !(dso%in%c("AGEM","EV/GHA4"))) %>%
  select(c("dso","region","year","month","b_protar_subreg","b_qw_subreg","b_nm_subreg",
           "b_gc","b_tc","b_nm","b_protar","b_qw")) %>%
  #mutate(across(c("b_protar_subreg","b_qw_subreg","b_nm_subreg",
  #                "b_gc","b_tc","b_nm","b_protar","b_qw"),~if_else(is.na(.x),0,.x))) %>%
  mutate(b_net_agg = (b_gc+b_tc+b_nm+b_protar+b_qw)/1000,
         b_net = (b_gc+b_tc+b_nm_subreg+b_protar_subreg+b_qw_subreg)/1000,
         date = as.Date(paste(year,month,"01",sep="-")))  

benefits_plot <- benefits_df %>%
  ggplot(aes(x=date,y = b_net, color = dso, linetype=region)) + 
  geom_line() + 
  labs(x="",y="Discounted benefits (thousand EUR)") +
  theme_bw() +#+ xlim(2008, 2019)
  coord_cartesian(xlim = c(start_date, end_date), expand=F) + 
  scale_x_date(date_labels = "%b/%y", limits = c(start_date, end_date), breaks="6 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("benefits_plot.pdf", benefits_plot , path = paste0(getwd(),"/data/plots"), height=5, width=8.75)  



#pel_change <- pel_plot %>% subset(date%in%c(as.Date("2008-01-01"),as.Date("2019-12-01")))


################################################################################
# Area Plots 
################################################################################


#cap <- 4
#c_capkw <- paste0("c_",cap,"kw") #variable name based on capacity chosen 
#c_capkw_defl <- paste0(c_capkw,"_defl")


benefits <- c("b_gc","b_nm","b_protar","b_tc","b_qw")
benefits_defl <- paste0(benefits,"_defl")
#cost <- c(c_capkw,c_capkw_defl)
cost <- grep("^c_",colnames(npv_final), value=T)
c_capkw <- grep("_defl", cost, value=T, invert=T)
c_capkw_defl <- grep("_defl", cost, value=T, invert=F)
cap <- gsub("[^0-9]", "", c_capkw) %>% as.numeric


npv_vars <- c("npv","npv_defl")
#cost_defl <- "c_4kw_defl"

#-1- determine data set for plot 
#npv_df <- npv_final %>% subset(select=c("year","month","region",benefits, benefits_defl, cost, npv_vars),year%in%2008:2019)

#-1- Define ordering of area plot and rename 
plotorder <- c(5,2,1,3,4) #digits following the ordering of benefits: (i) gv, (ii) nm, (iii) protar, (iv) tc, (v) qw
benefits_ordered <- paste(plotorder,benefits,sep="_")

#-2- Determine data and rename 
npv_df <- npv_final %>% 
  rename_with( ~ gsub("[3]","",.x), starts_with("b_")) %>%
  subset(select=c("year","month","region",benefits, benefits_defl, cost, npv_vars),year%in%2008:2019) %>%
  mutate(across(all_of(c(benefits, benefits_defl, cost, npv_vars)), ~ .x/cap)) %>%
  rename(all_of(setNames(benefits, benefits_ordered))) %>%
  rename(all_of(setNames(benefits_defl, paste0(benefits_ordered,"_defl")))) %>%
  mutate(date=as.Date(paste(year,month,"01",sep="-")))

#-3- colors & labels 
col_val <- c("#E69F00","#56B4E9","#0072B2","#D55E00","#660000")
col_var <- c("b_tc","b_gc","b_qw","b_nm","b_protar")

col_var_defl <- paste0(col_var,"_defl")
col_lab <- c("tax credit","output-based \n incentive","capacity-based \n incentive","net metering","capacity-based cost")

npv_colors <- inner_join(data.frame(col_var,col_var_defl,col_lab,col_val),
                     data.frame(col_var=benefits,col_var_ordered=benefits_ordered),
                     by="col_var")
npv_colors <- npv_colors %>%
  mutate(fl = if_else(col_var=="b_qw",0,1),
         wa = if_else(col_var=="b_protar",0,1)) %>%
  arrange(col_var_ordered)


#-4- reshape data for area plot 
npv_df_long <- npv_df %>% 
  pivot_longer(cols=all_of(grep("^[1-5]",colnames(.),value=T)),names_to="benefits") %>%
  rename("c_capkw"=all_of(c_capkw), "c_capkw_defl"=all_of(c_capkw_defl))


benefits_list <- list()

benefits_list[["fl"]] <- unique(npv_df_long$benefits) %>% 
  grep("_qw",.,value=T,invert=T) %>% 
  grep("_defl",.,value=T,invert=T)
benefits_list[["fl_defl"]] <- unique(npv_df_long$benefits) %>% 
  grep("_qw",.,value=T,invert=T) %>% 
  grep("_defl",.,value=T,invert=F)

benefits_list[["wa"]] <- unique(npv_df_long$benefits) %>% 
  grep("_protar",.,value=T,invert=T) %>% 
  grep("_defl",.,value=T,invert=T)
benefits_list[["wa_defl"]] <- unique(npv_df_long$benefits) %>% 
  grep("_protar",.,value=T,invert=T) %>% 
  grep("_defl",.,value=T,invert=F)


#cats_fl_defl <- cats %>% grep("_qw",.,value=T,invert=T) %>% grep("_defl",.,value=T,invert=F)

#cats_wa <- cats %>% grep("_protar",.,value=T,invert=T) %>% grep("_defl",.,value=T,invert=T)
#cats_wa_defl <- cats %>% grep("_protar",.,value=T,invert=T) %>% grep("_defl",.,value=T,invert=F)

#setdiff(cats_wa,cats_fl)

npv_plots_list <- list()
regions_abrev <- c("fl","wa")
r <- "Flanders"

npv_df_long <- npv_df_long %>%
  mutate(value=if_else(benefits %in% c("1_b_protar","1_b_protar_defl") & value==0,NA_real_,value)) 

npv_yrange_list <- list("yrange_defl"=c(-5000/cap,49000/cap),"yrange"=c(-5000/cap,49000/cap))
scalef <- 1000

for(r in c("Flanders","Wallonia")){
  for(i in c("","_defl")){
    reg_abrev <- tolower(substr(r,1,2))
    reg_other <- grep(reg_abrev,regions_abrev,value=T,invert=T)
    
    b_na <- setdiff(benefits_list[[paste0(reg_other,i)]], benefits_list[[paste0(reg_abrev,i)]])
    yrange <- npv_yrange_list[[paste0("yrange",i)]]/scalef
    
    npv_plot <- npv_df_long %>%
      mutate(value = if_else(benefits == b_na,NA_real_, value)) %>% #|(benefits == setdiff(benefits_list[[reg_abrev]], benefits_list[[reg_other]]) & value == 0)
      mutate_if(is.numeric, ~.x/scalef) %>%
      subset(region == r & benefits %in% benefits_list[[paste0(reg_abrev,i)]]) %>%
      ggplot(aes(fill = benefits, y = value, x = date)) +
      geom_area(aes(fill=benefits), colour="black", alpha = 0.8, size=0.2) +
      geom_line(aes(y = !!as.name(paste0("c_capkw",i)), colour = "cap_kw"), size = 0.5, linetype="dashed") +
       #annotate(geom = "text", x = as.Date("2008-04-01"), y = cost_lab, label = "Cost", color = "#666666", size = 5) +
      geom_line(aes(y = !!as.name(paste0("npv",i)), colour = "npv"), size = 0.5, linetype = "twodash") +
      #annotate(geom = "text", x = as.Date("2008-04-01"), y = npvfl_lab, label = "NPV", size = 5) +\
      scale_fill_manual(values = npv_colors[npv_colors[[reg_abrev]] == 1, "col_val"],
                        labels = npv_colors[npv_colors[[reg_abrev]] == 1, "col_lab"]) +
      scale_color_manual(values = c(cap_kw = "red", npv = "black"), labels = c(cap_kw = "cost", npv = "npv")) + ##666666
      labs(x = "", y = "1,000 EUR") +
      theme_bw() +
      coord_cartesian(xlim = c(start_date, end_date), ylim=yrange, expand = FALSE) +
      scale_x_date(date_labels = "%Y", limits = c(start_date, end_date), breaks = "1 year") +
      #guides(fill = guide_legend(override.aes = list(colour = c("black", "black")))) +
      guides(colour = guide_legend(override.aes = list(fill = "white", linetype=c(2,4)))) +
      theme(legend.title = element_blank()) +
      theme(legend.position = 'bottom', axis.text = element_text(size = 12), legend.text=element_text(size=12))
    
    npv_plots_list[[paste0(reg_abrev,i)]] <- npv_plot
    #  theme(legend.position = 'none', axis.text = element_text(size = 12), plot.title=element_text(hjust=0.5)) + 
    print(npv_plots_list[[paste0(reg_abrev,i)]])
    ggsave(paste0("npv_area_",reg_abrev,i,".pdf"),npv_plot, path = paste0(getwd(),"/data/plots"), height=5, width=8.75)
    npv_plot <- npv_plot +
      theme(axis.text = element_text(size = 12), plot.title=element_text(hjust=0.5),
            legend.box.spacing = unit(-5, "pt")) +
      ggtitle(r)
    ggsave(paste0("npv_area_",reg_abrev,i,"_pres.pdf"),npv_plot, path = paste0(getwd(),"/data/plots"), height=3, width=10)
    ggsave(paste0("npv_area_",reg_abrev,i,"_pres.png"),npv_plot, path = paste0(getwd(),"/data/plots"), height=3, width=10)
    }
  }

################################################################################
# Plot monthly regional aggregates of PV installations
################################################################################

pv_monthly_df <- estimate.data.monthly %>%
  subset(select=c(year,month,region,dso,zip,municipality,pv_count))

pv_monthly_regagg_df <- pv_monthly_df %>% 
  group_by(year,month,region) %>%
  summarise(pv_count=sum(pv_count)) %>%
  ungroup() %>%
  arrange(region,year,month) %>%
  group_by(region) %>%
  mutate(pv_count_cumsum = cumsum(pv_count)) %>%
  ungroup() %>%
  mutate(date = as.Date(paste(year,month,"01",sep="-")))
  

coeff <- 20 
alph <- 0.5
siz <- 0.5


plot_regagg_PVts <- function(plot, alpha=alph, size=siz, coefficient = coeff, region="Wallonia"){
  plot <- plot +
    geom_line(aes(y = pv_count_cumsum / coeff /1000, color = "cumulated installations"), size = 1, show.legend = TRUE, alpha=alph, linetype="longdash") +
    geom_line(aes(y = pv_count / 1000, color = "monthly new installations"), size = 1, show.legend = TRUE) +
    geom_point(aes(y = pv_count / 1000, color = "monthly new installations"), size = 1, show.legend = F) +
    #annotate(geom = "text", x = as.Date("2019-04-01"), y = 6500, label = "Cumulated", color = "grey", size = 4) +
    #annotate(geom = "text", x = as.Date("2010-03-01"), y = 9500, label = "GC benefit change + 6 months", color = "red", size = 4, alpha = alph) +
    scale_y_continuous(
      name = "Number of installations (thousand)",
      sec.axis = sec_axis(~ . * coeff, name = "Number of installations (thousand)", labels = scales::label_number())
    ) +
    labs(x = "", y = "") + # Add this line to remove the original axis labels
    theme_bw() +
    theme(axis.text = element_text(size = 12), 
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom", 
          legend.text = element_text(size = 12),
          axis.title.y.right = element_text(angle = 90)  # Rotate the secondary y-axis label
    ) 
  
    if(region=="Wallonia"){
      plot <- plot +
        labs(fill = '') +
        coord_cartesian(xlim = c(start_date, end_date), expand = FALSE) +
        scale_x_date(date_labels = "%Y", limits = c(start_date, end_date), breaks = "1 year") +
        #scale_linetype_manual(
        #name = "",
        #values = c("solid", "dashed", "solid"),
        #labels = c("monthly new installations", "GC change + 6 months", "accumulated installations")
        #) +
        scale_color_manual(
        name = "",
        values = c("monthly new installations" = "black", "output-based benefit change + 6 months" = "red","capacity-based benefit change" = "red", "cumulated installations" = "#333333"),
        labels = c("monthly new \n installations", "output-based benefit \n change + 6 months","capacity-based \n benefit change", "accumulated \n installations (RHS)")
        ) +
        guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3,3))))
    }
    else if(region=="Flanders"){
      plot <- plot +
        labs(fill = '') +
        coord_cartesian(xlim = c(start_date, end_date), expand = FALSE) +
        scale_x_date(date_labels = "%Y", limits = c(start_date, end_date), breaks = "1 year") +
        #scale_linetype_manual(
        #  name = "",
        #  values = c("solid", "dashed", "solid"),
        #  labels = c("monthly new installations", "GC change + 6 months", "accumulated installations")
        #) +
        scale_color_manual(
          name = "",
          values = c("monthly new installations" = "black", "output-based benefit change" = "red", "cumulated installations" = "#333333"),
          labels = c("monthly new \n installations", "output-based benefit change", "accumulated \n installations (RHS)")
        ) +
        guides(colour = guide_legend(override.aes = list(linetype=c(1,1,3))))
      
    }
  return(plot)
}


pv_monthly_wa_plot <- pv_monthly_regagg_df %>%
  subset(year %in% yrange & region == "Wallonia") %>%
  ggplot(aes(x = date)) +
  geom_vline(xintercept = as.Date("2012-05-01"), color = "red", alpha = alph, size = siz, show.legend = TRUE) +
  #geom_vline(xintercept = as.Date("2012-09-01"), color = "red", linetype = "dashed", alpha = alph, size = siz) +
  #geom_vline(xintercept = as.Date("2013-02-01"), color = "red", linetype = "dashed", alpha = alph, size = siz) +
  #geom_vline(xintercept = as.Date("2013-09-01"), color = "red", linetype = "dashed", alpha = alph, size = siz) +
  #geom_vline(xintercept = as.Date("2014-08-01"), color = "red", linetype = "dashed", alpha = alph, size = siz) +
  geom_vline(xintercept = as.Date("2014-06-01"), color = "red", linetype = "dashed", alpha = alph, size = siz, show.legend = TRUE) +
  geom_vline(xintercept = as.Date("2014-12-01"), color = "red", linetype = "dashed", alpha = alph, size = siz) +
  geom_vline(xintercept = as.Date("2015-06-01"), color = "red", linetype = "dashed", alpha = alph, size = siz) +
  geom_vline(xintercept = as.Date("2015-12-01"), color = "red", linetype = "dashed", alpha = alph, size = siz) +
  geom_vline(xintercept = as.Date("2016-06-01"), color = "red", linetype = "dashed", alpha = alph, size = siz) +
  geom_vline(xintercept = as.Date("2016-12-01"), color = "red", linetype = "dashed", alpha = alph, size = siz) +
  geom_vline(xintercept = as.Date("2017-06-01"), color = "red", linetype = "dashed", alpha = alph, size = siz) +
  geom_vline(xintercept = as.Date("2017-12-01"), color = "red", linetype = "dashed", alpha = alph, size = siz) +
  geom_vline(xintercept = as.Date("2018-06-01"), color = "red", linetype = "dashed", alpha = alph, size = siz)


pv_monthly_wa_plot <- plot_regagg_PVts(pv_monthly_wa_plot, region="Wallonia")
print(pv_monthly_wa_plot)

ggsave("pv_wa_monthly.pdf", pv_monthly_wa_plot, path = paste0(getwd(),"/data/plots"), height=5, width=8.75)

pv_monthly_wa_pres_plot <- pv_monthly_wa_plot +
  theme(axis.text = element_text(size = 12), plot.title=element_text(hjust=0.5),
        legend.box.spacing = unit(-10, "pt")) +
  ggtitle("Wallonia")
ggsave("pv_wa_monthly_pres.pdf",pv_monthly_wa_pres_plot, path = paste0(getwd(),"/data/plots"), height=3, width=10)
ggsave("pv_wa_monthly_pres.png",pv_monthly_wa_pres_plot, path = paste0(getwd(),"/data/plots"), height=3, width=10)


pv_monthly_fl_plot <-  pv_monthly_regagg_df %>%
  subset(year %in% yrange & region == "Flanders") %>%
  ggplot(aes(x=date)) + 
  geom_vline(xintercept=as.Date("2009-12-01"), color="red", alpha=alph, size=siz, show.legend = TRUE) +
  geom_vline(xintercept=as.Date("2010-12-01"), color="red", alpha=alph, size=siz) +
  geom_vline(xintercept=as.Date("2011-06-01"), color="red", alpha=alph, size=siz) +
  geom_vline(xintercept=as.Date("2011-09-01"), color="red", alpha=alph, size=siz) +
  geom_vline(xintercept=as.Date("2011-12-01"), color="red", alpha=alph, size=siz) +
  geom_vline(xintercept=as.Date("2012-03-01"), color="red", alpha=alph, size=siz) +
  geom_vline(xintercept=as.Date("2012-06-01"), color="red", alpha=alph, size=siz) +
  geom_vline(xintercept=as.Date("2012-07-01"), color="red", alpha=alph, size=siz) +
  geom_vline(xintercept=as.Date("2012-12-01"), color="red", alpha=alph, size=siz) 

pv_monthly_fl_plot <- plot_regagg_PVts(pv_monthly_fl_plot, region="Flanders")

print(pv_monthly_fl_plot)

ggsave("pv_fl_monthly.pdf", pv_monthly_fl_plot, path = paste0(getwd(),"/data/plots"), height=5, width=8.75)

pv_monthly_fl_pres_plot <- pv_monthly_fl_plot +
  theme(axis.text = element_text(size = 12), plot.title=element_text(hjust=0.5),
        legend.box.spacing = unit(-10, "pt")) +
  ggtitle("Flanders")
ggsave("pv_fl_monthly_pres.pdf",pv_monthly_fl_pres_plot, path = paste0(getwd(),"/data/plots"), height=3, width=10)
ggsave("pv_fl_monthly_pres.png",pv_monthly_fl_pres_plot, path = paste0(getwd(),"/data/plots"), height=3, width=10)





################################################################################
# Plot monthly PV data by municipality 
################################################################################

pv_monthly_df <- estimate.data.monthly %>%
  subset(select=c(year,month,date,region,dso,zip,municipality,pv_count,pv_capkw,pv_meansize))

alph=0.6
siz=0.6

for(i in c("Flanders")){ #,"Wallonia"
  
  #monthly 
  plot.linets <- pv_monthly_df %>% 
    subset(region==i & date>=start_date & date<=end_date) %>%
    group_by(date) %>%
    mutate(pv_count_mean = mean(pv_count, na.rm=T),
           pv_count_med = median(pv_count, na.rm=T)) %>%
    ungroup() %>%
    ggplot(aes(x=date)) +
    geom_point(aes(y=pv_count, group=as.character(zip)), colour="grey", alpha=0.2) + 
    geom_vline(xintercept=as.Date("2009-12-01"), color="red", alpha=alph, size=siz, show.legend = TRUE) +
    geom_vline(xintercept=as.Date("2010-12-01"), color="red", alpha=alph, size=siz) +
    geom_vline(xintercept=as.Date("2011-06-01"), color="red", alpha=alph, size=siz) +
    geom_vline(xintercept=as.Date("2011-09-01"), color="red", alpha=alph, size=siz) +
    geom_vline(xintercept=as.Date("2011-12-01"), color="red", alpha=alph, size=siz) +
    geom_vline(xintercept=as.Date("2012-03-01"), color="red", alpha=alph, size=siz) +
    geom_vline(xintercept=as.Date("2012-06-01"), color="red", alpha=alph, size=siz) +
    geom_vline(xintercept=as.Date("2012-07-01"), color="red", alpha=alph, size=siz) +
    geom_vline(xintercept=as.Date("2012-12-01"), color="red", alpha=alph, size=siz) +
    geom_line(aes(y=pv_count_mean), colour="black") +
    geom_point(aes(y=pv_count_mean), colour="black", size=0.5) +
    geom_line(aes(y=pv_count_med), colour="black", linetype="dashed") +
    #scale_x_continuous(breaks=seq(2008,2019,by=1), minor_breaks=seq(2008,2019,by=1)) +
    coord_cartesian(ylim=c(0,150), expand=F) +
    labs(x="",y="new PV installations") +
    theme_light()
  
  
  plotname <- paste0("PV_ylinets_",tolower((substr(i,1,2))),"_municip.pdf")
  
  ggsave(plotname, plot.linets, path = paste0(getwd(),"/data/plots"), height=5, width=8.75)
  print(plot.linets)
  
  
  plot.linets <- pv_monthly_df %>% 
    subset(region==i & date>=start_date & date<=end_date & pv_meansize!=0) %>%
    group_by(date) %>%
    mutate(pv_meansize_mean = mean(pv_meansize, na.rm=T),
           pv_meansize_med = median(pv_meansize, na.rm=T)) %>% 
    ggplot(aes(x=date)) + 
    geom_point(aes(y=pv_meansize, group=as.character(zip)), colour="grey", alpha=0.2) + 
    geom_vline(xintercept=as.Date("2009-12-01"), color="red", alpha=alph, size=siz, show.legend = TRUE) +
    geom_vline(xintercept=as.Date("2010-12-01"), color="red", alpha=alph, size=siz) +
    geom_vline(xintercept=as.Date("2011-06-01"), color="red", alpha=alph, size=siz) +
    geom_vline(xintercept=as.Date("2011-09-01"), color="red", alpha=alph, size=siz) +
    geom_vline(xintercept=as.Date("2011-12-01"), color="red", alpha=alph, size=siz) +
    geom_vline(xintercept=as.Date("2012-03-01"), color="red", alpha=alph, size=siz) +
    geom_vline(xintercept=as.Date("2012-06-01"), color="red", alpha=alph, size=siz) +
    geom_vline(xintercept=as.Date("2012-07-01"), color="red", alpha=alph, size=siz) +
    geom_vline(xintercept=as.Date("2012-12-01"), color="red", alpha=alph, size=siz) +
    geom_line(aes(y=pv_meansize_mean), colour="black") + 
    geom_point(aes(y=pv_meansize_mean), colour="black", size=0.5) +
    geom_line(aes(y=pv_meansize_med), colour="black", linetype="dashed") +
    coord_cartesian(ylim=c(0,9), expand=F) +
    labs(x="",y="average capacity size") +
    theme_light()
  
  plotname <- paste0("PVmeancap_ylinets_",tolower((substr(i,1,2))),"_municip.pdf")
  
  ggsave(plotname, plot.linets, path = paste0(getwd(),"/data/plots"), height=5, width=8.75)
  print(plot.linets)
  
}

  
  
plot.linets <- estimate.data.final.ysset %>% 
  subset(select=c("zip","year","region","pv_capkw","pv_count","pv_meansize"),
         year%in%2009:2019 & region==i) %>%
  group_by(year) %>%
  mutate(pv_meansize_mean = mean(pv_meansize, na.rm=T),
         pv_meansize_med = median(pv_meansize, na.rm=T)) %>% 
  ggplot(aes(x=year)) + 
  geom_line(aes(y=pv_meansize, group=as.character(zip)), colour="grey", alpha=0.7) + 
  geom_line(aes(y=pv_meansize_mean), colour="black") + 
  geom_line(aes(y=pv_meansize_med), colour="black", linetype="dashed") +
  scale_x_continuous(breaks=seq(2008,2019,by=1), minor_breaks=seq(2008,2019,by=1)) +
  scale_y_continuous(breaks=seq(1,9,by=1))#, minor_breaks=seq(2008,2019,by=1)) +


print(plot.linets)

plot.linets <- estimate.data.monthly.final.ysset %>% 
  subset(select=c("zip","year","date","region","pv_capkw","pv_count","pv_meansize"), 
         year%in%2009:2019 & region==i &pv_meansize!=0) %>%
  group_by(date) %>%
  mutate(pv_meansize_mean = mean(pv_meansize, na.rm=T),
         pv_meansize_med = median(pv_meansize, na.rm=T)) %>% 
  ggplot(aes(x=date)) + 
  geom_line(aes(y=pv_meansize, group=as.character(zip)), colour="grey", alpha=0.7) + 
  geom_line(aes(y=pv_meansize_mean), colour="black") + 
  geom_line(aes(y=pv_meansize_med), colour="black", linetype="dashed") +
  scale_x_continuous(breaks=seq(2008,2019,by=1), minor_breaks=seq(2008,2019,by=1)) +
  scale_y_continuous(breaks=seq(1,9,by=1))#, minor_breaks=seq(2008,2019,by=1)) +

print(plot.linets)

  
###############################################################################
# Check for correlation between distribution tariffs and PV adoption, especially in Flanders
###############################################################################

scatter_ts <- estimate.data %>% 
  select(year,region,dso,zip,disttar_mixed,pv_count,hh_size) %>%
  mutate(pv_hh = pv_count/hh_size) %>%
  subset(year=2012)

plot.scatter_ts <- scatter_ts %>%
  ggplot(aes(x=pv_hh,y=disttar_mixed,color=region)) +
  geom_point()

print(plot.scatter_ts)

disttar_ts <- estimate.data %>% 
  select(year,region,dso,zip,disttar_mixed,pv_count,hh_size) %>%
  mutate(date = as.Date(paste0(year,"-01-01"))) %>%
  group_by(date,region,dso) %>%
  summarise(disttar_mixed=first(disttar_mixed)) %>%
  ungroup()

plot.disttar_ts <- disttar_ts %>%
  ggplot(aes(x=date,y=disttar_mixed,group=dso,color=region)) +
  geom_line()

print(plot.disttar_ts)


################################################################################
# Plot possible instrument
################################################################################

IV_df <- estimate.data.monthly %>%
  select(year,month,date,region,zip,b_nm_mix_retail_solw_defl,b_nm_solw_defl,b_nm_mix_retail_defl, b_nm_defl) %>%
  group_by(year,month,date,region) %>%
  summarise(across(all_of(grep("b_",colnames(.), value=T)), first)) %>%
  ungroup() %>%
  pivot_longer(cols=starts_with("b_"),
               names_to = "b_type",
               values_to= "value")


IV_plot <- IV_df %>%
  subset(b_type %in% c("b_nm_mix_retail_solw_defl","b_nm_solw_defl")) %>%
  ggplot(aes(x = date, y = value, group = interaction(b_type, region))) + 
  geom_line(aes(color = region, linetype = b_type)) +
  labs(x = "", y = "Electricity price in EUR/KWh", color = "", linetype="") +
  coord_cartesian(xlim = c(start_date, end_date), expand = FALSE) + 
  scale_x_date(date_labels = "%b/%y", limits = c(start_date, end_date), breaks = "6 months") +
  theme_bw() +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'bottom',
        legend.text = element_text(size = 12)) +  # Adjust the font size here
  scale_linetype_manual(values = c("solid", "dashed"), label=c("w/ distr. tarif","w/o distr. tariff")) +
  labs(color = "")
print(IV_plot)  

IV_plot <- IV_df %>%
  subset(b_type %in% c("b_nm_solw_defl","b_nm_defl")) %>%
  ggplot(aes(x = date, y = value, group = interaction(b_type, region))) + 
  geom_line(aes(color = region, linetype = b_type)) +
  labs(x = "", y = "Electricity price in EUR/KWh", color = "", linetype="") +
  coord_cartesian(xlim = c(start_date, end_date), expand = FALSE) + 
  scale_x_date(date_labels = "%b/%y", limits = c(start_date, end_date), breaks = "6 months") +
  theme_bw() +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'bottom',
        legend.text = element_text(size = 12)) +  # Adjust the font size here
  scale_linetype_manual(values = c("solid", "dashed"), label=c("w/ distr. tarif","w/o distr. tariff")) +
  labs(color = "")
print(IV_plot)  





