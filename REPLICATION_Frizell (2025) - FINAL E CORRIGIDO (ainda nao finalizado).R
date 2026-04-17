############### CODE MARCO

##### LIBRARIES
library(haven)
library(dplyr)
library(readr)
library(fixest)
library(modelsummary)
library(htmltools)
library(ggplot2)
library(ggrepel)
library(car)
library(lmtest)
library(sandwich)
library(patchwork)
library(plm)
library(marginaleffects)
library(kableExtra)
library(tidyr)
library(tibble)
library(knitr)

base_dir <- "C:/Users/marco/OneDrive/Documentos/phd/FYP/Frizell rep"


df5                    <- read_csv(file.path(base_dir, "Dataset 5 years(final).csv"))
df_annual              <- read_csv(file.path(base_dir, "Dataset annual(final).csv"))
df5_with_iirating      <- read_csv(file.path(base_dir, "Dataset 5 years_IIrating(final).csv"))
df_annual_with_iirating <- read_csv(file.path(base_dir, "Dataset annual_IIrating(final).csv"))


## Expectation table


parameters_expectation <- data.frame(Variable = c("Conflict$_{it-1}$", "Post-Cold War", "Conflict $\\times$ Post-CW", "Conflict$_{it-1}$", "Post-Cold War", "IIRating$_{it-1}$", "Conflict $\\times$ Post-CW", "Conflict $\\times$ IIRating"),
                                     Parameter= c("$\\beta_1$", "$\\beta_2$", "$\\beta_3$", "$\\beta_1$", "$\\beta_2$", "$\\beta_3$", "$\\beta_4$", "$\\beta_5$"),
                                     Expectation =c("$> 0$", "$< 0$", "$< 0$","$> 0$", "$< 0$", "$= 0$","$< 0$", "$< 0$"))

kable(parameters_expectation, format = "html", escape = FALSE, booktabs = TRUE, caption = "Parameter expectations")


# Original MOLDES

df5$pid5 <- relevel(factor(df5_with_iirating$pid), ref = "5")

FE1 <- feols(toprate ~ L_major_dum * pcw90 + pid |ccode, data = df5, cluster = ~ccode)
FE2 <- feols(toprate ~ L_major_dum*pcw90 +L_left_dum + L_univ_suf+ L_lngdp_cap+ pid |ccode, data=df5, cluster = ~ccode)
FE3<- feols(toprate ~ L_major_dum * pcw90 +L_OILprc5 +L_ODA_GDP +L_EPRrel_nr +  L_terr_loss + L_recession + L_left_dum 
             + L_univ_suf + L_lngdp_cap + pid |ccode, data = df5, cluster = ~ccode)
FE4<-feols(toprate ~ L_major_dum*pcw90 + L_OILprc5 +L_ODA_GDP +L_EPRrel_nr +  L_terr_loss + L_recession + L_left_dum 
           + L_univ_suf + L_lngdp_cap + pid5 | ccode, data = df5, cluster = ~ccode)
LDV1<- lm(toprate ~ L_toprate + L_major_dum * pcw90 + pid, data = df5)
LDV2<- lm(toprate ~ L_toprate + L_major_dum * pcw90 + L_left_dum + L_univ_suf + L_lngdp_cap + pid, data = df5)
LDV3 <- lm(toprate ~ L_toprate + L_major_dum * pcw90 + L_OILprc5 + L_ODA_GDP + L_EPRrel_nr + L_terr_loss + L_recession + L_left_dum + L_univ_suf + L_lngdp_cap + pid,
           data = df5)
LDV4 <- lm(toprate ~ L_toprate + L_major_dum * pcw90 + L_OILprc5 + L_ODA_GDP + L_EPRrel_nr + L_terr_loss + L_recession + L_left_dum +
             L_univ_suf + L_lngdp_cap + pid5,data = df5)

### Table (I used modelsummary() because it works better with feols and clustered SEs)
model_names <- list("FE (1)" = FE1, "FE (2)" = FE2, "FE (3)" = FE3, "FE (4)" = FE4, "LDV (5)" = LDV1, "LDV (6)" = LDV2,
                  "LDV (7)" = LDV3, "LDV (8)" = LDV4)
variable_names <- c("L_toprate" = "L. Top PIT rate", "L_major_dum" = "L. Conflict", "pcw90" = "Post-Cold War", "L_major_dum:pcw90" = "L. Conflict × Post-CW",
                    "L_OILprc5"= "Oil income",  "L_ODA_GDP" = "External Aid Ratio", "L_EPRrel_nr"= "Ethnic power relations",
                    "L_terr_loss"= "Territorial Loss", "L_recession" = "Recession",  "L_left_dum"= "Left government",
                    "L_univ_suf"= "Democracy",  "L_lngdp_cap"= "log(GDP/cap)")
rows_table<- data.frame( term = c("Time trend", "Period dummies"), "FE (1)" = c("YES", "NO"), "FE (2)" = c("YES", "NO"), "FE (3)" = c("YES", "NO"), "FE (4)" = c("NO", "YES"), "LDV (5)" = c("YES", "NO"), "LDV (6)" = c("YES", "NO"), "LDV (7)" = c("YES", "NO"), "LDV (8)" = c("NO", "YES"), check.names = FALSE)

modelsummary(model_names, vcov = "HC1", coef_map = variable_names, add_rows = rows_table, stars = c("*"=0.1, "**"=0.05, "***"=0.01), title = "Table 1: War and top PIT rates: Country-FE and LDV",
             notes = "Clustered SEs (FE) and heteroskedasticity-robust SEs (LDV).", output = "html")
              

###cross tabulation
crosstab_ii<- df5_with_iirating %>%
  filter(!is.na(L_major_dum), !is.na(toprate), !is.na(L_iirating)) %>%
  mutate(conflict_bin = ifelse(L_major_dum > 0, "Major Conflict", "No Conflict"),period= ifelse(pcw90 == 1, "Post-Cold War", "Cold War"))

p25 <- quantile(crosstab_ii$L_iirating, 0.25, na.rm = TRUE)
p75 <- quantile(crosstab_ii$L_iirating, 0.75, na.rm = TRUE)
low <- paste0("Low (<=", round(p25, 1), ")")
medium<- paste0("Medium (", round(p25, 1), "-", round(p75, 1), ")")
high<- paste0("High (>=", round(p75, 1), ")")

crosstab_ii <- crosstab_ii %>%
  mutate(ii_group = case_when(L_iirating <= p25 ~ low, L_iirating >= p75 ~ high, TRUE~ medium))

means_period<- crosstab_ii %>% 
  group_by(period, conflict_bin) %>%
  summarise(cell = paste0(round(mean(toprate), 1), "pp (n=", n(), ")"),.groups = "drop") %>%
  pivot_wider(names_from = conflict_bin, values_from = cell) %>%
  rename(Group = period) %>%
  arrange(factor(Group, levels = c("Cold War", "Post-Cold War")))

means_ii<- crosstab_ii%>%
  group_by(ii_group, conflict_bin) %>%
  summarise(cell = paste0(round(mean(toprate), 1), "pp (n=", n(), ")"),.groups = "drop") %>%
  pivot_wider(names_from = conflict_bin, values_from = cell) %>%
  rename(Group = ii_group) %>%
  arrange(factor(Group, levels = c(low,medium,high)))

crosstabulation_full<- bind_rows(means_period, means_ii)
crosstabulation_full%>% 
 kable(format= "html", caption= "Mean Top PIT Rate by Conflict Status",col.names = c("", "Conflict", "No Conflict"), align= c("l", "c", "c"))
  

##Quantities of interest
#predicted values
predicted_fe <- predictions(FE3, newdata = datagrid(L_major_dum = c(0, 1), pcw90= c(0, 1),ccode= df5$ccode[1]), vcov = FALSE)%>% 
  as.data.frame() %>%
  select(L_major_dum, pcw90, FE_fit = estimate)

predicted_ldv <- predictions(LDV3,newdata = datagrid(L_major_dum = c(0, 1), pcw90= c(0, 1))) %>% 
  as.data.frame() %>%
  select(L_major_dum, pcw90, LDV_fit = estimate)

avg_country <- left_join(predicted_fe, predicted_ldv, by = c("L_major_dum", "pcw90")) %>%
  mutate(Period = factor(ifelse(pcw90 == 0, "Cold War", "Post-Cold War"), levels = c("Cold War", "Post-Cold War")),
  Status = ifelse(L_major_dum == 0, "No Conflict", "Conflict"))

#firstdiffe
firstdiff <- avg_country %>% 
  select(Period, Status, FE_fit, LDV_fit) %>%
  pivot_wider(names_from = Status, values_from = c(FE_fit, LDV_fit)) %>%
  mutate(`FE (3)`  = FE_fit_Conflict  - `FE_fit_No Conflict`,`LDV (7)` = LDV_fit_Conflict - `LDV_fit_No Conflict`) %>%
  select(Period, `FE (3)`, `LDV (7)`) %>%
  pivot_longer(-Period, names_to = "Model", values_to = "first_difference")

#plot
FV1 <- ggplot(avg_country, aes(x = Period, y = LDV_fit, color = Status, group = Status)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  geom_text(aes(label = paste0(round(LDV_fit, 1), "pp")),
            vjust = -1, size = 3.5, show.legend = FALSE) +
  scale_color_manual(values = c("Conflict" = "salmon", "No Conflict" = "steelblue")) +
  scale_y_continuous(labels = function(x) paste0(x, "pp")) +   # ← sem limits
  labs(title = "Fitted Values - LDV (7)", subtitle = "Typical country",
       x = NULL, y = "Predicted Top PIT Rate (pp)", color = NULL) +
  theme_classic(base_size = 10) + theme(legend.position = "bottom")

FD2 <- ggplot(firstdiff, aes(x = Period, y = first_difference, fill = Period, alpha = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("Cold War" = "#ea801c", "Post-Cold War" = "#1f6f6f")) +
  scale_alpha_manual(values = c("FE (3)" = 1.0, "LDV (7)" = 0.5)) +
  labs(title = "First Differences", subtitle = "FE (3) and LDV (7)",
       x = NULL, y = "Change in Top PIT Rate (pp)",
       fill = "Period", alpha = "Model") +
  theme_classic(base_size = 10) + theme(legend.position = "bottom")

FV1 + FD2

###Illustrative cases
df_cases_orig <- df5 %>%
  filter(!is.na(L_major_dum), !is.na(toprate)) %>%
  mutate(period = ifelse(pcw90 == 1, "Post-Cold War", "Cold War"), toprate_hi = ifelse(toprate >= median(toprate, na.rm = TRUE), "High Tax", "Low Tax"))

top_cases <- function(df, order = "desc") {if (order == "desc") head(arrange(df, desc(toprate)), 3)
  else head(arrange(df, toprate), 3)}


cases <- bind_rows(
  df_cases_orig %>% filter(L_major_dum >= 0.6, toprate_hi == "High Tax", period == "Cold War")      %>% top_cases("desc"),
  df_cases_orig %>% filter(L_major_dum >= 0.6, toprate_hi == "Low Tax",  period == "Cold War")      %>% top_cases("asc"),
  df_cases_orig %>% filter(L_major_dum >= 0.6, toprate_hi == "High Tax", period == "Post-Cold War") %>% top_cases("desc"),
  df_cases_orig %>% filter(L_major_dum >= 0.6, toprate_hi == "Low Tax",  period == "Post-Cold War") %>% top_cases("asc"),
  df_cases_orig %>% filter(L_major_dum == 0,   toprate_hi == "Low Tax",  period == "Post-Cold War") %>% top_cases("asc"),
  df_cases_orig %>% filter(L_major_dum == 0,   toprate_hi == "High Tax", period == "Cold War")      %>% top_cases("desc")) %>%
  mutate(label= paste0(country, " (", p5Y, ")"),ConflictYears = L_major_dum * 5, Era = period)


quadrants <- data.frame( x = c(2, 2, 0, 0), y = c(112, 30, 112, 30), label = c("Conflict + High Tax", "Conflict + Low Tax", "No Conflict + High Tax", "No Conflict + Low Tax"))

#plot
ggplot(cases, aes(x = ConflictYears, y = toprate, color = Era)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_text_repel(aes(label = label), size = 4, max.overlaps = 20,
                  min.segment.length = 0, box.padding = 0.8,
                  point.padding = 0.5, force = 15, show.legend = FALSE) +
  geom_hline(yintercept = 50, linetype = "dashed", alpha = 0.4) +
  geom_vline(xintercept = 1,  linetype = "dashed", alpha = 0.4) +
  geom_text(data = quadrants, aes(x = x, y = y, label = label),
            inherit.aes = FALSE, fontface = "bold", size = 4) +
  scale_color_manual(values = c("Cold War" = "#ea801c", "Post-Cold War" = "#1f6f6f")) +
  scale_x_continuous(limits = c(-0.5, 5.5), breaks = 0:5) +
  scale_y_continuous(limits = c(0, 115), labels = function(x) paste0(x, "pp")) +
  labs(title    = "Illustrative Cases: Conflict vs. Top PIT Rate",
       subtitle = "Dashed lines indicate quadrant classification",
       x     = "Years in conflict (out of a five-year period)",
       y     = "Top PIT Rate",
       color = "Period") +
  theme_classic(base_size = 11) +
  theme(legend.position = "bottom")


###Pathway cases
COEF3 <- coef(FE3)

pathway5<- df5 %>%
  filter(L_major_dum > 0, !is.na(toprate), !is.na(pcw90)) %>%
  mutate(counterfactual_change = COEF3["L_major_dum"] * L_major_dum + COEF3["L_major_dum:pcw90"] * L_major_dum * pcw90) %>%
  arrange(desc(counterfactual_change)) %>% head(5) %>%
  mutate(Country= country, Period= p5Y, Era = ifelse(pcw90 == 0, "Cold War", "Post-Cold War"),
         TopRate   = paste0(round(toprate, 2), "pp"), Conflict  = round(L_major_dum, 2), CounterfactualChange = paste0("+", round(counterfactual_change, 2), "pp")) %>%
  select(Country, Period, Era, TopRate, Conflict, CounterfactualChange)

#tabel
pathway5 %>% kable(format  = "latex", caption = "Top 5 Pathway Cases: Counterfactual Change in Top PIT Rate", align   = c("l", "c", "c", "c", "c", "c")) %>%
  kable_styling(full_width = FALSE) %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(6, bold = TRUE) %>%
  row_spec(0, bold = TRUE) %>%
  footnote(general = "Counterfactual change = predicted change in top PIT rate when moving from actual conflict intensity to zero conflict, holding all other variables at their actual values.", general_title = "Note:")

###marginal effects plots

b1 <- coef(FE3)["L_major_dum"]
b3 <- coef(FE3)["L_major_dum:pcw90"]


margeffects <- expand.grid(pcw90 = c(0, 1), conflict_val = c(0.2, 0.4, 0.6, 0.8, 1.0)) %>%
  mutate(period=factor(ifelse(pcw90 == 0, "Cold War", "Post-Cold War"), levels = c("Cold War", "Post-Cold War")), estimate = (b1 + b3 * pcw90) * conflict_val)

#plot
meffects_plot<- ggplot(margeffects, aes(x = period, y = estimate,
                               color = factor(conflict_val),
                               group = factor(conflict_val))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) +
  geom_text(aes(label = paste0(round(estimate, 1), "pp"),
                hjust = ifelse(pcw90 == 0, 1.3, -0.3)),
            size = 2.8, show.legend = FALSE) +
  scale_color_manual(
    values = c("0.2" = "#00BCD4", "0.4" = "#00ACC1", "0.6" = "#0097A7",
               "0.8" = "#00838F", "1"   = "#006064"),
    name   = "Conflict Intensity\n(proportion of years)",
    breaks = c("1", "0.8", "0.6", "0.4", "0.2"),
    labels = c("1.0", "0.8", "0.6", "0.4", "0.2")
  ) +
  scale_x_discrete(expand = c(0.3, 0.3)) +
  labs(title    = "Marginal Effect of Conflict on Top PIT Rate",
       subtitle = "FE (3) - by conflict intensity level",
       x = NULL, y = "Predicted Change in Top PIT Rate (pp)") +
  theme_classic(base_size = 11) +
  theme(legend.position = "right")

#histogram
meffects_histogram <- df5 %>%
  filter(L_major_dum > 0) %>%
  mutate(period = factor(ifelse(pcw90 == 0, "Cold War", "Post-Cold War"),
                         levels = c("Cold War", "Post-Cold War"))) %>%
  ggplot(aes(x = factor(round(L_major_dum, 1)), fill = period)) +
  geom_bar(position = "dodge", color = "white", alpha = 0.8) +
  scale_fill_manual(values = c("Cold War" = "#ea801c", "Post-Cold War" = "#1f6f6f"),
                    name = NULL) +
  labs(title = "Distribution of Conflict Intensity",
       x     = "Conflict Intensity (proportion of years in conflict)",
       y     = "Count") +
  theme_classic(base_size = 10) +
  theme(legend.position = "bottom")

meffects_plot + meffects_histogram



# EXTENSION MOLDEL

#I created the same period dummy that Frizell uses in his original work, but for 1980 (the first perdiod of the dataset with IIRating)
df5_with_iirating$pid8 <- relevel(factor(df5_with_iirating$pid), ref = "8")

FE_NEW1<- feols(toprate ~ L_major_dum * L_iirating + L_major_dum * pcw90 + pid | ccode,data = df5_with_iirating, cluster = ~ccode)
FE_NEW2<- feols(toprate ~ L_major_dum * L_iirating + L_major_dum * pcw90 + L_left_dum + L_univ_suf + L_lngdp_cap + pid | ccode, data = df5_with_iirating, cluster = ~ccode)
FE_NEW3<- feols(toprate ~ L_major_dum * L_iirating + L_major_dum * pcw90 + L_OILprc5 + L_ODA_GDP + L_EPRrel_nr + L_terr_loss + L_recession + L_left_dum +
                   L_univ_suf + L_lngdp_cap + pid | ccode, data = df5_with_iirating, cluster = ~ccode)
FE_NEW4<- feols(toprate ~ L_major_dum * L_iirating + L_major_dum * pcw90 +  L_OILprc5 + L_ODA_GDP + L_EPRrel_nr + L_terr_loss + L_recession + L_left_dum + L_univ_suf + L_lngdp_cap +
                 pid8| ccode, data = df5_with_iirating, cluster = ~ccode)
LDV_NEW5 <- lm(toprate ~ L_toprate + L_major_dum * L_iirating + L_major_dum * pcw90 + pid, data = df5_with_iirating)
LDV_NEW6 <- lm(toprate ~ L_toprate + L_major_dum * L_iirating + L_major_dum * pcw90 + L_left_dum + L_univ_suf + L_lngdp_cap + pid, data = df5_with_iirating)
LDV_NEW7 <- lm(toprate ~ L_toprate + L_major_dum * L_iirating + L_major_dum * pcw90 + L_OILprc5 + L_ODA_GDP + L_EPRrel_nr + L_terr_loss + L_recession +
                 L_left_dum + L_univ_suf + L_lngdp_cap + pid, data = df5_with_iirating)
LDV_NEW8 <- lm(toprate ~ L_toprate + L_major_dum * L_iirating + L_major_dum * pcw90 + L_OILprc5 + L_ODA_GDP + L_EPRrel_nr + L_terr_loss + L_recession +
                 L_left_dum + L_univ_suf + L_lngdp_cap + pid8, data = df5_with_iirating)

### REGRESSION Table
models_extension <- list("FE (1)" = FE_NEW1, "FE (2)" = FE_NEW2, "FE (3)" = FE_NEW3, "FE (4)" = FE_NEW4, "LDV (5)" = LDV_NEW5, "LDV (6)" = LDV_NEW6,
                    "LDV (7)" = LDV_NEW7, "LDV (8)" = LDV_NEW8)
varnames_extension <- c("L_toprate" = "L. Top PIT rate", "L_major_dum" = "L. Conflict", "pcw90" = "Post-Cold War", "L_major_dum:pcw90" = "L. Conflict × Post-CW",
                    "L_iirating" = "Credit Rating", "L_major_dum:L_iirating" = "Conflict x Credit Rating",
                    "L_OILprc5"= "Oil income",  "L_ODA_GDP" = "External Aid Ratio", "L_EPRrel_nr"= "Ethnic power relations",
                    "L_terr_loss"= "Territorial Loss", "L_recession" = "Recession",  "L_left_dum"= "Left government",
                    "L_univ_suf"= "Democracy",  "L_lngdp_cap"= "log(GDP/cap)")
rows_table_ext <- data.frame( term = c("Time trend", "Period dummies"), "FE (1)" = c("YES", "NO"), "FE (2)" = c("YES", "NO"),
  "FE (3)" = c("YES", "NO"), "FE (4)" = c("NO", "YES"), "LDV (5)" = c("YES", "NO"), "LDV (6)" = c("YES", "NO"), "LDV (7)" = c("YES", "NO"), "LDV (8)" = c("NO", "YES"), check.names = FALSE)
modelsummary(models_extension, vcov = list(NULL, NULL, NULL, NULL,"HC1", "HC1", "HC1", "HC1") ,coef_map = varnames_extension, add_rows = rows_table_ext, stars = c("*"=0.1, "**"=0.05, "***"=0.01), title = "Table 1: War and top PIT rates: Country-FE and LDV",
             notes = "Clustered SEs (FE) and heteroskedasticity-robust SEs (LDV).", output = "html")



##marginal efefcts plot

plot_margef1<- plot_slopes(LDV_NEW7,variables = "L_major_dum",condition = "L_iirating", vcov = "HC1") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title    = "Marginal Effect of Conflict on Top PIT Rate",
       subtitle = "Conditional on II Rating - LDV (7), 95% CI",
       x = "L. II Rating (Credit Access)",
       y = "Marginal Effect of Conflict (pp)") +
  theme_classic(base_size = 11)

hist_me1 <- ggplot(df5_with_iirating, aes(x = L_iirating)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white", alpha = 0.7) +
  labs(x = "L. II Rating (Credit Access)", y = "Count") +
  theme_classic(base_size = 10)

plot_margef2 <- plot_slopes(LDV_NEW7, variables = "L_iirating", condition = "L_major_dum", vcov = "HC1") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                     labels = c("0", "1", "2", "3", "4", "5")) +
  labs(title    = "Marginal Effect of II Rating on Top PIT Rate",
       subtitle = "Conditional on Conflict - LDV (7), 95% CI",
       x = "L. Major Conflict (number of years fought in a 5 years period)",
       y = "Marginal Effect of II Rating (pp)") +
  theme_classic(base_size = 11)

hist_me2 <- ggplot(df5_with_iirating, aes(x = L_major_dum)) +
  geom_histogram(bins = 6, fill = "salmon", color = "white", alpha = 0.7) +
  scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                     labels = c("0", "1", "2", "3", "4", "5")) +
  labs(x = "L. Major Conflict (number of years fought in a 5 years period)",
       y = "Count") +
  theme_classic(base_size = 10)

(plot_margef1 / hist_me1 + plot_layout(heights = c(3, 1))) |
  (plot_margef2 / hist_me2 + plot_layout(heights = c(3, 1)))

#####First differences - Credit Rating

avg_comparisons(LDV_NEW7, variables = "L_major_dum", newdata   = datagrid(L_iirating = c(quantile(df5_with_iirating$L_iirating, 0.25, na.rm = TRUE),
                                      mean(df5_with_iirating$L_iirating, na.rm = TRUE), quantile(df5_with_iirating$L_iirating, 0.75, na.rm = TRUE))), vcov= "HC1")

### First differences of the FE MODEL 3 (I had to calculate manually)

first_diffcases <- expand.grid(ii_val = c(ii_low1, ii_med2, ii_high3), pcw_val = c(0, 1))

fd_results <- do.call(rbind, lapply(1:nrow(first_diffcases), function(i) {first_diff_FE3(first_diffcases$ii_val[i], first_diffcases$pcw_val[i])}))


firstdiff_FE_NEW3 <- cbind(first_diffcases, fd_results) %>%
  mutate(Period= ifelse(pcw_val == 0, "Cold War", "Post-Cold War"),
    IIRating = case_when(ii_val == ii_low1  ~ paste0("Low (P25 = ", round(ii_low1, 1), ")"), ii_val == ii_med2  ~ paste0("Mean (", round(ii_med2, 1), ")"), ii_val == ii_high3 ~ paste0("High (P75 = ", round(ii_high3, 1), ")")),
    FirstDiff = paste0(round(estimate, 1), "pp"), SE= round(std.error, 2), CI= paste0("[", round(conf.low, 1), ", ", round(conf.high, 1), "]")) %>%
  select(Period, IIRating, FirstDiff, SE, CI)

View(firstdiff_FE_NEW3)


#####Pathway cases for credit rating x conflict

df_pathway <- filter(df5_with_iirating, L_major_dum > 0, !is.na(toprate), !is.na(L_iirating))
df_pathway$fitted_actual_values<- predict(LDV_NEW7, newdata = df_pathway)
df_pathway$fitted_counterfactual<- predict(LDV_NEW7, newdata = df_pathway %>%
                                             mutate(L_major_dum = 0))

df_pathway$counterfactual_change <- df_pathway$fitted_actual_values - df_pathway$fitted_counterfactual


pathwaycases <- pathway_top15 <- df_pathway %>%
  arrange(desc(counterfactual_change)) %>%
  head(15) %>%
  mutate(Country= country, Period= p5Y, Era=ifelse(pcw90 == 0, "Cold War", "Post-Cold War"), TopRate=paste0(round(toprate, 2), "pp"),
    Conflict=round(L_major_dum, 2), IIRating=round(L_iirating, 1), CounterfactualChange=paste0("+", round(counterfactual_change, 2), "pp") ) %>%
  select(Country, Period, Era, TopRate, Conflict, IIRating, CounterfactualChange)

View(pathwaycases)



###############Appendix


df5_appendix<- df5_with_iirating %>%
  filter(p5Y >= 1980, p5Y <= 2015)

vars_appendix <- c("toprate", "L_toprate", "L_major_dum", "pcw90", "L_iirating",
                  "L_OILprc5", "L_ODA_GDP", "L_EPRrel_nr", "L_terr_loss",
                  "L_recession", "L_left_dum", "L_univ_suf", "L_lngdp_cap")

###descriptive stats

datasummary(All(df5_appendix[, vars_appendix]) ~ N + Mean + SD + Min + Max, data = df5_appendix, output = "html")



