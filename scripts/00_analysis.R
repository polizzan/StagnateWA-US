##############################
# START PACKAGE INSTALLATION #
##############################

## (1) packages to be installed from cran
from.cran <- c("devtools", "data.table", "gdata",
               "ggh4x", "ggpubr", "ggrepel", 
               "here", "HMDHFDplus", "plyr", 
               "RColorBrewer", "svglite", "tidyverse", 
               "qpdf")

## (2) packages to be installed from github, paths listed separately
from.git <- NULL
from.git.path <- NULL

## check if installed, else install
for(i in c(from.cran, from.git)){
  
  ## cran packages
  if(i %in% from.cran){
    
    if(system.file(package = i) == ""){install.packages(i)}
    
  }
  
  ## github packages
  if(i %in% from.git){
    
    if(system.file(package = i) == ""){
      
      devtools::install_github(from.git.path[which(from.git == i)])
      
    }
  }
}

## load packages
library(ggh4x)
library(tidyverse)

############################
# END PACKAGE INSTALLATION #
############################

# ----

####################
# START USER INPUT #
####################

## color palettes
fig1.palette <- RColorBrewer::brewer.pal(n = 11, "RdBu")[c(2, 4, 8, 10)]
fig2.palette <- RColorBrewer::brewer.pal(n = 8, "Dark2")[c(8, 4, 1, 6)]

## select four HMD countries that will be shown alongside the US  
countries <- c("Italy", "Japan", "Switzerland", "United Kingdom")

##################
# END USER INPUT #
##################

# ----

##################
# START SET PATH #
##################

here::i_am("scripts/00_analysis.R")

################
# END SET PATH #
################

# ----

##############################
# START DIRECTORY GENERATION #
##############################

if(!dir.exists(here::here("out"))){dir.create(here::here("out"))}

if(!dir.exists(here::here("out", "fig1"))){dir.create(here::here("out", "fig1"))}
if(!dir.exists(here::here("out", "fig2"))){dir.create(here::here("out", "fig2"))}

for(i in c("svg", "pdf", "png")){

if(!dir.exists(here::here("out", "fig1", i))){dir.create(here::here("out", "fig1", i))}
if(!dir.exists(here::here("out", "fig2", i))){dir.create(here::here("out", "fig2", i))}

}  
  
############################
# END DIRECTORY GENERATION #
############################

# ----

###################
# START FUNCTIONS #
###################

## life expectancy function
ex.fun <- function(m, age = 25:100){
  
  n = c(diff(age), Inf)
  a = n * 0.5
  a[length(m)] = 1 / m[length(m)]
  q = n * m / (1 + (n - a) * m)
  q[length(m)] = 1
  p = 1 - q
  l = head(cumprod(c(1, p)), -1)
  L = n * l * p + a * l * q
  L[length(m)] = l[length(m)] / m[length(m)] 
  e = rev(cumsum(rev(L))) / l
  
  return(e)
  
}

#################
# END FUNCTIONS #
#################

# ----

####################
# START DATA INPUT #
####################

## load HMD data
source(here::here("scripts/08_get-hmd.R"))

## clear environment
gdata::keep(fig1.palette, fig2.palette, countries,
            ex.fun, HMD.select, sure = TRUE)

##################
# END DATA INPUT #
##################

# ----

#######################
# START QUALITY CHECK #
#######################

## check if mx values in 2000, 2009, 2010, and 2019 are permissible
check.df <-
  HMD.select %>% 
  filter(Year %in% c(2000, 2009, 2010, 2019), Age >= 25) %>% 
  filter(mx == 0 | is.na(mx) | is.infinite(mx))

if(dim(check.df)[1] > 0){
  
  warning("At least one mx value zero, missing, or infinite.", call. = FALSE)
  
}  

## remove from environment
rm(check.df)

#####################
# END QUALITY CHECK #
#####################

# ----

#############################
# START EDIT COUNTRY LABELS #
#############################

## order alphabetically by country names, putting USA first
aux.cntry <-
  HMD.select %>% 
  filter(!Code %in% "USA") %>% 
  group_by(Country) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  arrange(Country)

CNTRY <- c("USA", aux.cntry$Code)
CNTRY.labels <- c("USA", aux.cntry$Country)

## remove from environment
rm(aux.cntry)

###########################
# END EDIT COUNTRY LABELS #
###########################

# ----

###########################
# START DATA MANIPULATION #
###########################

## calculate rates of mortality improvement (ROMI), five-year age groups
ROMI.5 <-
  HMD.select %>% 
  filter(Year %in% c(2000, 2009, 2010, 2019)) %>% ## focus on two decades 2000-2009, 2010-2019
  filter(Age >= 25) %>% 
  mutate(Age = c(rep(seq(25, 95, 5), each = 5), 100), ## add age variable to aggregate into five-year age groups
         .by = c(Year, Country, Sex)) %>% 
  summarize(Deaths = sum(Deaths),
            Exposures = sum(Exposures),
            .by = c(Year, Country, Sex, Age)) %>% ## aggregate death and exposure counts
  mutate(mx = Deaths / Exposures) %>%  ## calculate death rates
  select(Country, Year, Sex, Age, mx) %>% 
  pivot_wider(names_from = "Year",
              values_from = "mx") %>% ## reshape to wide format
  mutate(!!paste0("2000", "\U2013", "2009") := - log(`2009` / `2000`) / 9, ## calculate ROMI
         !!paste0("2010", "\U2013", "2019") := - log(`2019` / `2010`) / 9) %>% ## add negative sign: mortality improvements have positive values
  select(-c(`2000`, `2009`, `2010`, `2019`)) %>% 
  pivot_longer(cols = c(paste0("2000", "\U2013", "2009"), paste0("2010", "\U2013", "2019")),
               names_to = "Period",
               values_to = "ROMI") %>%  ## reshape to long 
  mutate(Country = factor(Country, levels = CNTRY.labels))

## data frame with death rates
mx.1 <- 
  HMD.select %>% 
  select(-c(Deaths, Exposures))
  
## calculate rates of mortality improvement (ROMI), single age groups
ROMI.1 <-
  mx.1 %>% 
  filter(Year %in% c(2000, 2009, 2010, 2019)) %>% ## focus on two decades 2000-2009, 2010-2019 
  filter(Age >= 25) %>% 
  pivot_wider(names_from = "Year",
              values_from = "mx") %>% ## reshape to wide
  mutate(ROMI.2000.2009 = log(`2009` / `2000`) / 9, ## calculate ROMI
         ROMI.2010.2019 = log(`2019` / `2010`) / 9) %>% 
  mutate(ROMI = ifelse(Country == "USA", ROMI.2000.2009, ROMI.2010.2019)) %>% ## use 2000-2009 ROMI for within-US counterfactual, else use 2010-2019 ROMI
  select(Country, Age, Sex, ROMI) %>% 
  pivot_wider(names_from = "Country",
              values_from = "ROMI") ## reshape to wide

aux <- 
  mx.1 %>% 
  filter(Country == "USA" & Year %in% 2009:2019 & Age >= 25) %>% ## pull observed US mortality rates
  mutate(n = Year - 2009) %>% ## distance from year 2009
  left_join(ROMI.1) %>% ## add ROMI
  select(-c(Country, Code))

mx.ROMI.1 <-
 aux %>% 
   left_join(
     aux %>% 
       filter(Year == 2009) %>% 
       select(Age, Sex, mx.2009 = mx) ## merge 2009 US mortality rates
   ) %>% 
  rename(mx.obs = mx) ## rename mx variable

## baseline scenario (observed)
bl <- 
  mx.ROMI.1 %>% 
  mutate(across(all_of(CNTRY.labels), ~ mx.obs)) %>% ## take observed rates
  mutate(Counterfactual = "Baseline") ## add identifier for type of counterfactual

## counterfactual scenarios
cf.25plus <-
  mx.ROMI.1 %>% 
  mutate(across(all_of(CNTRY.labels), ~ ifelse(Age >= 25, mx.2009 * exp(. * n), mx.obs))) %>% ## if age criterion is met, adjust 2009 rates by ROMI, else take observed rates 
  mutate(Counterfactual = "25+") 

cf.25to49 <-
  mx.ROMI.1 %>% 
  mutate(across(all_of(CNTRY.labels), ~ ifelse(Age %in% 25:49, mx.2009 * exp(. * n), mx.obs))) %>% 
  mutate(Counterfactual = "25-49")

cf.50to64 <-
  mx.ROMI.1 %>% 
  mutate(across(all_of(CNTRY.labels), ~ ifelse(Age %in% 50:64, mx.2009 * exp(. * n), mx.obs))) %>% 
  mutate(Counterfactual = "50-64")

cf.25to64 <-
  mx.ROMI.1 %>% 
  mutate(across(all_of(CNTRY.labels), ~ ifelse(Age %in% 25:64, mx.2009 * exp(. * n), mx.obs))) %>% 
  mutate(Counterfactual = "25-64")

cf.65to84 <-
  mx.ROMI.1 %>% 
  mutate(across(all_of(CNTRY.labels), ~ ifelse(Age %in% 65:84, mx.2009 * exp(. * n), mx.obs))) %>% 
  mutate(Counterfactual = "65-84")

cf.65plus <-
  mx.ROMI.1 %>% 
  mutate(across(all_of(CNTRY.labels), ~ ifelse(Age >= 65, mx.2009 * exp(. * n), mx.obs))) %>% 
  mutate(Counterfactual = "65+")

cf.85plus <-
  mx.ROMI.1 %>% 
  mutate(across(all_of(CNTRY.labels), ~ ifelse(Age >= 85, mx.2009 * exp(. * n), mx.obs))) %>% 
  mutate(Counterfactual = "85+")

## append all counterfactuals, calculate life expectancy
full.ex <-
  bl %>% 
  add_row(cf.25plus) %>% 
  add_row(cf.25to49) %>% 
  add_row(cf.50to64) %>% 
  add_row(cf.25to64) %>%   
  add_row(cf.65to84) %>% 
  add_row(cf.65plus) %>% 
  add_row(cf.85plus) %>% 
  select(-c(mx.obs, n, mx.2009)) %>% 
  mutate(across(all_of(CNTRY.labels), ~ ex.fun(m = .)), 
         .by = c(Year, Sex, Counterfactual)) %>% 
  pivot_longer(cols = -c(Year, Sex, Age, Counterfactual),
               names_to = "Comparison",
               values_to = "ex") %>% ## reshape to long
  mutate(Comparison = factor(Comparison, levels = CNTRY.labels))

#########################
# END DATA MANIPULATION #
#########################

# ----

##################
# START PLOTTING #
##################

## arrange country names in matrix for easy looping across country names
x <- length(CNTRY.labels[CNTRY.labels != "USA"])%%4
x <- ifelse(x == 0, 4, x)

filter.matrix <-
  rbind(
    countries, ## add user-selected countries  
    matrix(
      c(
        CNTRY.labels[CNTRY.labels != "USA"],
        rep("", 4 - x) 
      ), 
      ncol = 4, byrow = TRUE
    )
  )

for(i in 1:dim(filter.matrix)[1]){

## select countries to plot
FIL <- c("USA", filter.matrix[i, ])  
  
## figure 1: ROMI 
## plot uses geom_step, so last age group needs to be added a second time
aux.ROMI <-
  ROMI.5 %>% 
  add_row(

  ROMI.5 %>% 
  filter(Age == 100) %>% 
  mutate(Age = 105)

  ) %>% 
  mutate(Country = factor(Country, levels = CNTRY.labels))
  
fig.1 <-
   ggplot(data = ROMI.5 %>% filter(Country %in% FIL), aes(x = Age, y = ROMI)) +
     geom_bar(data = . %>% filter(Period %in% paste0("2000", "\U2013", "2009")), 
              aes(fill = ROMI), 
              stat = "identity", show.legend = TRUE) + ## plot 2000-2009 as bars
     geom_step(data = aux.ROMI %>% 
                 filter(Country %in% FIL) %>% 
                 filter(Period %in% paste0("2010", "\U2013", "2019")) %>% 
                 mutate(ROMI = ifelse(ROMI > 0, 0, ROMI)), ## plot negative values for 2010-2019 as steps
               aes(x = Age - 2.5), 
               stat = "identity", 
               color = fig1.palette[1], 
               linetype = "solid", 
               linewidth = 0.5) +
     geom_step(data = aux.ROMI %>% 
                 filter(Country %in% FIL) %>%
                 filter(Period %in% paste0("2010", "\U2013", "2019")) %>% 
                 mutate(ROMI = ifelse(ROMI < 0, 0, ROMI)), ## plot positive values for 2010-2019 as steps
               aes(x = Age - 2.5), 
               stat = "identity", 
               color = fig1.palette[4], 
               linetype = "solid", 
               linewidth = 0.5) +
     geom_step(data = data.frame(x = c(25, 50, 100), y = c(0, 0, 0)), 
               aes(x = x, y = y, color = y), inherit.aes = FALSE) + ## plot zero data set to include correct legend
     geom_hline(yintercept = 0, color = "black") + ## add black vertical line at 0
     labs(x = "Age", y = "Rate of Mortality Improvement") +
     scale_x_continuous(breaks = seq(25, 100, 25),
                        minor_breaks = seq(25, 100, 5),
                        guide = "axis_minor") +
     scale_y_continuous(labels = function(x) ifelse(x > 0, sprintf("%+06.3f", x), 
                                                    ifelse(x < 0, sprintf("%06.3f", x), 
                                                           sprintf("%05.3f", x)))) + ## labels will have positive or negative sign
     binned_scale(name = paste0("2010", "\U2013", "2019"), ## add binned legend for steps
                  aesthetics = "color",
                  scale_name = "stepsn", 
                  palette = function(x) c(fig1.palette[1], fig1.palette[4]),
                  breaks = c(-100, 0, 100),
                  limits = c(-100, 100),
                  show.limits = FALSE, 
                  guide = "colorsteps",
                  labels = c("", paste0("\U2013", paste(rep(" ", 8), collapse = ""), "+"), "")
       ) +
     binned_scale(name = paste0("2000", "\U2013", "2009"), ## add binned legend for bars
                  aesthetics = "fill",
                  scale_name = "stepsn", 
                  palette = function(x) c(fig1.palette[2], fig1.palette[3]),
                  breaks = c(-100, 0, 100),
                  limits = c(-100, 100),
                  show.limits = FALSE, 
                  guide = "colorsteps",
                  labels = c("", paste0("\U2013", paste(rep(" ", 8), collapse = ""), "+"), "")
       ) +
     guides(color = guide_bins(title.position = "top", title.hjust = 0.5, order = 2, axis = FALSE, override.aes = list(fill = "transparent")), ## adjust legends for bars and steps
            fill = guide_bins(title.position = "top", title.hjust = 0.5, order = 1, axis = FALSE, override.aes = list(color = "transparent"))) +
     coord_flip() +
     facet_grid(Sex ~ Country) + 
     theme_bw() +
     theme(aspect.ratio = 1,
           panel.grid = element_blank(),
           strip.background = element_rect(fill = "white"),
           strip.text = element_text(size = 12),
           axis.title.x = element_text(size = 12, vjust = -2),
           axis.title.y = element_text(size = 12, vjust = 2),
           axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
           axis.text.y = element_text(size = 10),
           legend.text = element_text(size = 12, hjust = 0.5),
           legend.key.width = unit(0.5, "in"),
           legend.position = "bottom")

## figure 2: life expectancy in counterfactual scenarios
fig.2 <-
  ggplot(full.ex %>% 
           filter(Comparison %in% FIL) %>%
           filter(Age == 25, 
                  Counterfactual %in% c("25+", "25-64", "65+", "Baseline")) %>% ## focus on most important counterfactuals
           mutate(Counterfactual = factor(Counterfactual, 
                                          levels = c("Baseline", "25+", "25-64", "65+"))), ## order counterfactuals, baseline is plotted first  
         aes(x = Year, y = ex + 25)) + 
  geom_line(aes(color = Counterfactual, linetype = Counterfactual), linewidth = 0.75) +
  ggrepel::geom_text_repel(data = . %>% filter(Year == 2019), ## add label for life expectancy in 2019 
                           aes(label = sprintf("%03.1f", ex + 25)), ## make sure there is one decimal
                           size = 2, seed = 123456) +
  scale_x_continuous(breaks = c(2009, 2015, 2019),
                     minor_breaks = 2009:2019, 
                     guide = "axis_minor") +
  scale_color_manual("", 
                     breaks = c("Baseline", "25+", "25-64", "65+"), ## determine legend order
                     labels = c("Actual mortality",
                                "All ages counterfactual",
                                "25-64 counterfactual",
                                "65+ counterfactual"), ## change labels in accordance with Abrams et al.
                     values = c("Baseline" = fig2.palette[1],
                                "25+" = fig2.palette[2],
                                "25-64" = fig2.palette[3],
                                "65+" = fig2.palette[4])) + ## assign colors in accordance with Abrams et al.
  scale_linetype_manual(values = c("Baseline" = "solid",
                                   "25+" = "longdash",
                                   "25-64" = "longdash",
                                   "65+" = "longdash"), ## assign linetype in accordance with Abrams et al.
                        guide = "none") +
  labs(x = "", y = "Life expectancy") +
  facet_grid(Sex ~ Comparison, scales = "free_y") + ## scales may vary by sex
  theme_bw() +
  theme(aspect.ratio = 1,
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 12),
        axis.title.y = element_text(size = 12, vjust = 2),
        axis.text = element_text(size = 9),
        legend.text = element_text(size = 9),
        legend.position = "bottom") +
  coord_cartesian(xlim = c(2009, 2020))

## post-hoc adjustment of y-axis limits and labels in figure 2
g <- ggplot_build(fig.2)

for(k in 1:length(g$layout$panel_params)){ ## for each panel
  
  min <- plyr::round_any(g$layout$panel_params[[k]]$y$limits[1], f = floor, accuracy = 0.5) ## pull lower limit determined by facet_grid, round
  max <- plyr::round_any(g$layout$panel_params[[k]]$y$limits[2], f = ceiling, accuracy = 0.5) ## pull upper limit determined by facet_grid, round

  g$layout$panel_params[[k]]$y$limits <- c(min, max) ## adjust limits
  g$layout$panel_params[[k]]$y$continuous_range <- c(min, max)
  g$layout$panel_params[[k]]$y$breaks <- seq(min + 0.5, max - 0.5, 0.5) ## adjust labels
    
}

## turn back into ggplot
fig.2 <- ggpubr::as_ggplot(ggplot_gtable(g)) 

## save as .svg
ggsave(plot = fig.1, filename = here::here("out", "fig1", "svg", paste0("Figure-1-", i, ".svg")), width = 10 , height = 6, device = "svg")
ggsave(plot = fig.2, filename = here::here("out", "fig2", "svg", paste0("Figure-2-", i, ".svg")), width = 10 , height = 5, device = "svg")

## save as .png
ggsave(plot = fig.1, filename = here::here("out", "fig1", "png", paste0("Figure-1-", i, ".png")), width = 10 , height = 6, device = "png")
ggsave(plot = fig.2, filename = here::here("out", "fig2", "png", paste0("Figure-2-", i, ".png")), width = 10 , height = 5, device = "png")

## save as .pdf
cairo_pdf(filename = here::here("out", "fig1", "pdf", paste0("Figure-1-", i, ".pdf")), width = 10 , height = 6)
plot(fig.1)
dev.off()

cairo_pdf(filename = here::here("out", "fig2", "pdf", paste0("Figure-2-", i, ".pdf")), width = 10 , height = 5)
plot(fig.2)
dev.off()

}

## combine pdfs into one file
pdf.files.fig1 <- list.files(path = here::here("out", "fig1", "pdf"), pattern = ".pdf", full.names = TRUE)
pdf.files.fig2 <- list.files(path = here::here("out", "fig2", "pdf") , pattern = ".pdf", full.names = TRUE)

qpdf::pdf_combine(pdf.files.fig1, output = here::here("out", "fig1", "Figure-1-combined.pdf"))
qpdf::pdf_combine(pdf.files.fig2, output = here::here("out", "fig2", "Figure-2-combined.pdf"))

################
# END PLOTTING #
################

# ----

###########################
# START SOME CALCULATIONS #
###########################

## number of times e25 is higher if working-age vs. older-age mortality is shifted at counterfactual ROMI
full.ex %>% 
  filter(Year == 2019, Age == 25, Comparison != "USA", Counterfactual %in% c("25-64", "65+")) %>% 
  pivot_wider(names_from = "Counterfactual", 
              values_from = "ex") %>% 
  mutate(ALL = n(),
         NUM = sum(`25-64` > `65+`), .by = Sex) %>% 
  mutate(SHARE = NUM / ALL) %>% 
  group_by(Sex) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  select(Year, Sex, Age, ALL, NUM, SHARE)

#########################
# END SOME CALCULATIONS #
#########################
