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
outputpath <- paste0(getwd(),"/data/plots")

#########
#Load Data 
#########

load(paste0(getwd(),"/data/tariffs_pel_subreg.Rdata")) #subregional electricity prices
load(paste0(getwd(),"/data/npv_final.Rdata")) #npv data 
load(paste0(getwd(),"/data/b_subreg.Rdata")) #npv data for surbregions 
load(paste0(getwd(),"/data/PV_total_estdata_monthly.Rdata")) #full data monthly 
#load(paste0(getwd(),"/data/PV_total_estdata.Rdata")) #full data monthly 


yrange <- 2008:2019
start_date <- as.Date("2008-01-01")  # Specify the desired start date
end_date <- as.Date("2019-12-01")  # Specify the desired end date

################################################################################
# Monthly regional aggregates of PV installations - Figure 1
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
yrange <- 2008:2019

plot_regagg_PVts <- function(data, alpha=alph, size=siz, coefficient = coeff, regionname="Wallonia"){
  
  plot <- data %>%
    subset(year %in% yrange & region == regionname) %>%
    ggplot(aes(x = date)) + 
    #geom_vline(data = vlines, aes(xintercept = xintercepts), linetype="dashed" ,color = "red", show.legend = F) + 
    geom_line(aes(y = pv_count_cumsum / coeff /1000, color = "cumulated installations"), size = 1, show.legend = T, alpha=alph, linetype="longdash") +
    geom_line(aes(y = pv_count / 1000, color = "monthly new installations"), size = 1, show.legend = TRUE) +
    geom_point(aes(y = pv_count / 1000, color = "monthly new installations"), size = 1, show.legend = F)+
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
    )  +
    labs(fill = '') +
    coord_cartesian(xlim = c(start_date, end_date), expand = FALSE) +
    scale_x_date(date_labels = "%Y", limits = c(start_date, end_date), breaks = "1 year") +
    scale_color_manual(
      name = "",
      values = c("monthly new installations" = "black", "cumulated installations" = "#333333"),
      labels = c("monthly new \n installations","accumulated \n installations (RHS)")
    ) +
    guides(colour = guide_legend(override.aes = list(linetype=c(1,2))))
  
  return(plot)
}

#Flanders
plot_PV_fl <- plot_regagg_PVts(pv_monthly_regagg_df, regionname = "Flanders")

vlines <- data.frame(
  xintercepts = as.Date(c("2009-12-01", "2010-12-01", "2011-06-01", "2011-09-01",
                          "2011-12-01", "2012-03-01", "2012-06-01", "2012-07-01", 
                          "2012-12-01")))
plot_PV_fl <- plot_PV_fl + geom_vline(data=vlines, aes(xintercept = xintercepts), color = "red")

print(plot_PV_fl)
#ggsave("plot_pv_fl_monthly.pdf", plot_PV_fl, path = outputpath, height=5, width=8.75)

plot_PV_fl_pres <- plot_PV_fl +
  theme(axis.text = element_text(size = 12), plot.title=element_text(hjust=0.5),
        legend.box.spacing = unit(-10, "pt")) +
  ggtitle("Flanders")
#print(plot_PV_fl_pres)
#ggsave("plot_pv_fl_monthly_pres.pdf",plot_PV_fl_pres, path = outputpath, height=3, width=10)
#ggsave("plot_pv_fl_monthly_pres.png",plot_PV_fl_pres, path = outputpath, height=3, width=10)

#Wallonia
plot_PV_wa <- plot_regagg_PVts(pv_monthly_regagg_df, regionname = "Wallonia")

vlines <- data.frame(
  xintercepts = as.Date(c("2015-06-01", "2015-12-01", "2016-06-01",
                          "2016-12-01", "2017-06-01", "2017-12-01", "2018-06-01")))

plot_PV_wa <- plot_PV_wa +
  geom_vline(xintercept = as.Date("2012-05-01"), color="red") + 
  geom_vline(data=vlines, aes(xintercept = xintercepts), color = "red", linetype="dashed")
print(plot_PV_wa)

#ggsave("plot_pv_wa_monthly.pdf", plot_PV_wa, path = outputpath, height=5, width=8.75)

plot_PV_wa_pres <- plot_PV_wa +
  theme(axis.text = element_text(size = 12), plot.title=element_text(hjust=0.5),
        legend.box.spacing = unit(-10, "pt")) +
  ggtitle("Wallonia")
#print(plot_PV_wa_pres)
#ggsave("plot_pv_wa_monthly_pres.pdf",plot_PV_wa_pres, path = outputpath, height=3, width=10)
#ggsave("plot_pv_wa_monthly_pres.png",plot_PV_wa_pres, path = outputpath, height=3, width=10)



################################################################################
# Area Plots - Figure 2
################################################################################

benefits <- c("b_gc","b_nm","b_protar","b_tc","b_qw")
benefits_defl <- paste0(benefits,"_defl")

cost <- grep("^c_",colnames(npv_final), value=T)
c_capkw <- grep("_defl", cost, value=T, invert=T)
c_capkw_defl <- grep("_defl", cost, value=T, invert=F)
cap <- gsub("[^0-9]", "", c_capkw) %>% as.numeric


npv_vars <- c("npv","npv_defl")


#-1- Define ordering of area plot and rename 
plotorder <- c(5,2,1,3,4) #digits following the ordering of benefits: (i) gc, (ii) nm, (iii) protar, (iv) tc, (v) qw
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


#-5- Plot 
npv_plots_list <- list()
regions_abrev <- c("fl","wa")

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
    #ggsave(paste0("npv_area_",reg_abrev,i,".pdf"),npv_plot, path = outputpath, height=5, width=8.75)
    
    #print in different format for presentations 
    npv_plot <- npv_plot +
      theme(axis.text = element_text(size = 12), plot.title=element_text(hjust=0.5),
            legend.box.spacing = unit(-5, "pt")) +
      ggtitle(r)
    #ggsave(paste0("npv_area_",reg_abrev,i,"_pres.pdf"),npv_plot, path = outputpath, height=3, width=10)
    #ggsave(paste0("npv_area_",reg_abrev,i,"_pres.png"),npv_plot, path = outputpath, height=3, width=10)
    }
  }





################################################################################
# Pel and instrument - Figure A2
################################################################################
yrange <- 2008:2019
start_date <- as.Date("2008-01-01")  # Specify the desired start date
end_date <- as.Date("2019-12-01")  # Specify the desired end date


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

#ggsave("pel_IV_plot.pdf", pel_reg_plot, path = outputpath, height=5, width=8.75)  

################################################################################
# Summary statistics: Table 1
################################################################################

check <- estimate.data.monthly %>% 
  subset(pv_count == 0)

function.pv_summary <- function(data, groupby = T){
  
  if(groupby == T){
    pv_summary_df <- data %>%
      group_by(region)
  }else{
    pv_summary_df <- data
  }
  
  pv_summary_df <- pv_summary_df %>%
    subset(year %in% yrange) %>%
    summarise(n_municp = length(unique(zip)),
              pv_total = sum(pv_count),
              n = n(),
              zerosh = sum(ifelse(pv_count==0,1,0))/n,
              pv_mean = mean(pv_count),
              pv_med = median(pv_count),
              pv_std = sqrt(var(pv_count)),
              pv_min = min(pv_count),
              pv_max = max(pv_count),
              cap_mean = mean(if_else(pv_meansize==0,NA_real_,pv_meansize), na.rm=T),
              cap_std = sqrt(var(if_else(pv_meansize==0,NA_real_,pv_meansize), na.rm=T)),
              cap_min = min(if_else(pv_meansize==0,NA_real_,pv_meansize), na.rm=T),
              cap_max = max(if_else(pv_meansize==0,NA_real_,pv_meansize), na.rm=T),.groups = "drop")
   
  return(pv_summary_df)
}

pv_summary <- rbind(
  function.pv_summary(estimate.data.monthly),
  function.pv_summary(estimate.data.monthly, groupby = F) %>% mutate(region="Total")
)

print("Table 1")
print(pv_summary)

################################################################################
# Summary statistics: Table A2
################################################################################


benefitvars <- c("b_gc_solw_defl","b_nm_solw_defl","b_protar_defl","b_qw_defl")

#in Thousand EUR
expvar_summary_benefits <- estimate.data.monthly %>%
  select(all_of(c("year","month","zip",benefitvars)))

expvar_summary_benefits <- expvar_summary_benefits %>%
  mutate(b_net_solw_defl = b_gc_solw_defl + b_nm_solw_defl + b_protar_defl + b_qw_defl)

benefitvars <- c("b_net_solw_defl",benefitvars)

expvar_summary_benefits <- expvar_summary_benefits %>%
  mutate(across(all_of(benefitvars), ~.x/4)) %>%
  mutate(b_net_solw_defl_log = log(b_net_solw_defl)) %>%
  mutate(across(all_of(benefitvars), ~.x/1000))
  
benefitvars <- c("b_net_solw_defl_log",benefitvars)

benefitvars <- c("Net benefits (log)"="b_net_solw_defl_log",
                 "Net benefits"="b_net_solw_defl",
                 "Output-based"="b_gc_solw_defl",
                 "Net metering"="b_nm_solw_defl",
                 "Capacity-based cost"="b_protar_defl",
                 "Capacity-based"="b_qw_defl")


expvar_summary_benefits <- expvar_summary_benefits %>% select(all_of(benefitvars)) %>% describe()


controlvars_labeled <- 
  c("Household size (log)"="hh_size_log",
    "Population density (log)"="popsize_dens_log",
    "Age:below 18 (sh.)"="popsize_age_0_17_sh",
    "Age:18-49 (sh.)"="popsize_age_18_49_sh",
    "Age:50-64 (sh.)"="popsize_age_50_64_sh",
    "Age:above 64 (sh.)"="popsize_age_above64_sh",
    "Nationals (sh.)"="popsize_nat_bel_sh",
    "Non-nationals (sh.)"="popsize_nat_nbel_sh",
    "Male (sh.)"="popsize_men_sh",
    "Female (sh.)"="popsize_women_sh",
    "Net median income (log)"="income_perdecl_med_defl_log", 
    "Hh single (sh.)"="popsize_living_alone_sh",
    "Hh single parent (sh.)"="popsize_living_singlepar_sh",
    "Hh couple /w children (sh.)"="popsize_living_couplechild_sh",
    "Hh couple w/o children (sh.)"="popsize_living_couplenochild_sh",
    "House age:until 1981 (sh.)"="buildings_until1981_sh",
    "House age:after 1981 (sh.)"="buildings_after1981_sh",
    "House type:apartments (sh.)"="housing_apartments_sh",
    "House type:single fam closed (sh.)"="housing_singfam_closed_sh",
    "House type:single fam semi-detached (sh.)"="housing_singfam_semidetached_sh",
    "House type:single fam open (sh.)"="housing_singfam_open_sh")


expvar_summary_controls <- estimate.data.monthly %>%
  select(all_of(c("year","month","zip",controlvars_labeled))) %>%
  group_by(year,zip) %>%
  summarise(across(everything(), ~mean(.x)), .groups = "drop") %>%
  select(all_of(names(controlvars_labeled))) %>%
  describe()

expvar_summary <- rbind(
  expvar_summary_benefits,
  expvar_summary_controls
)

expvar_summary <- expvar_summary %>% select(c(mean,sd,min,median,max,n))

print("Table A2")
print(expvar_summary)

################################################################################
# Additional: Plot monthly PV data by municipality 
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
  
  #ggsave(plotname, plot.linets, path = outputpath, height=5, width=8.75)
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
  
  #ggsave(plotname, plot.linets, path = outputpath, height=5, width=8.75)
  print(plot.linets)
  
}
