library(tidyverse)
library(stargazer)
library(corrplot)
library(viridisLite)
library(hrbrthemes)
library(viridis)
library(BSDA)
library(readxl)

FINAL_AID_DATA <- read_excel("../USA-COVID-19-aid/data/final/FINAL_AID_DATA.xlsx", sheet = "valid")

#перевірка того, як між собою корелюють показники W за Мескітою та Смітом та W, скориговане на індекс Polity V
cor(FINAL_AID_DATA$W_Polity, FINAL_AID_DATA$W_Mesquita, method = "pearson", use="complete.obs")

#виділення кожного типу режиму в окремі об'єкти для легшої калькуляції тестів
Autocracy <- dplyr::filter(FINAL_AID_DATA, type2 == "Autocracy")
Closed_Anocracy <- dplyr::filter(FINAL_AID_DATA, type2 == "Closed Anocracy")
Open_Anocracy <- dplyr::filter(FINAL_AID_DATA, type2 == "Open Anocracy")
Democracy <- dplyr::filter(FINAL_AID_DATA, type2 == "Democracy")
Full_democracy <- dplyr::filter(FINAL_AID_DATA, type2 == "Full democracy")

#z-тести кожного з типів режимів за Polity V порівняно з усім датасетом
z.test(Autocracy$USA_aid_per_capita,
      sigma.x=sd(FINAL_AID_DATA$USA_aid_per_capita), alternative = "two.sided", mu=mean(FINAL_AID_DATA$USA_aid_per_capita))
z.test(Closed_Anocracy$USA_aid_per_capita,
       sigma.x=sd(FINAL_AID_DATA$USA_aid_per_capita), alternative = "two.sided", mu=mean(FINAL_AID_DATA$USA_aid_per_capita))
z.test(Open_Anocracy$USA_aid_per_capita,
       sigma.x=sd(FINAL_AID_DATA$USA_aid_per_capita), alternative = "two.sided", mu=mean(FINAL_AID_DATA$USA_aid_per_capita))
z.test(Democracy$USA_aid_per_capita,
       sigma.x=sd(FINAL_AID_DATA$USA_aid_per_capita), alternative = "two.sided", mu=mean(FINAL_AID_DATA$USA_aid_per_capita))
z.test(Full_democracy$USA_aid_per_capita,
       sigma.x=sd(FINAL_AID_DATA$USA_aid_per_capita), alternative = "two.sided", mu=mean(FINAL_AID_DATA$USA_aid_per_capita))

#візуалізація допомоги на душу населення за типом режиму у вигляді boxplot
FINAL_AID_DATA %>%
  ggplot(aes(x=factor(type2, levels = c("Autocracy", "Closed Anocracy", "Open Anocracy", "Democracy", "Full democracy"), ordered=TRUE), y=USA_aid_per_capita, fill=type2)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=18)
  ) +
  ggtitle("Розмір допомоги залежно від типу режиму") +
  xlab("")

#збереження графіку
ggsave("../USA-COVID-19-aid/tables and visualization/boxplot.png", width = 8, height = 8, dpi = 300)

#виділення підмасиву зі змінними для кореляційної матриці + сама матриця?
predictors <- FINAL_AID_DATA %>%
  mutate(USA_aid_total=USA_aid_total/1000000, Total_trade=Total_trade/1000000, Population_2019=Population_2019/1000000) %>%
  select(c(3,13:14,16:19,23:25, 27:28))  
corrplot(cor(predictors, method = "pearson", use = "complete.obs"), method = c("square"), tl.cex = 0.8, tl.col="black")

#код для таблиці з дескриптивною статистикою по кожній зі змінних в LaTeX
stargazer(as.data.frame(predictors), type='latex', title="Descriptive statistics", omit.summary.stat = c("p25", "p75"))

#гістограми для візуальної перевірки того, як розподіляються дані (чи немає зміщення)
par(mfrow=c(3,3)) 
hist(FINAL_AID_DATA$Population_2019)
hist(FINAL_AID_DATA$population_density)
hist(FINAL_AID_DATA$USA_aid_total)
hist(FINAL_AID_DATA$GDP_per_capita)
hist(FINAL_AID_DATA$population_density)
hist(FINAL_AID_DATA$aged_65_older)
hist(FINAL_AID_DATA$GHS_Index)
hist(FINAL_AID_DATA$HAQ_Index)
hist(FINAL_AID_DATA$WGI_GovEff)

#моделі з W_Mesquita в якості залежної змінної
Model1 <- lm(log(USA_aid_total) ~  log(Population_2019) +  W_Mesquita, data= FINAL_AID_DATA)
Model2 <- lm(log(USA_aid_total) ~  log(Population_2019) +  W_Mesquita  + WGI_GovEff + GHS_Index + HAQ_Index, data= FINAL_AID_DATA)
Model3 <- lm(log(USA_aid_total) ~  log(Population_2019) +  W_Mesquita + WGI_GovEff + GHS_Index + HAQ_Index + 
               log(Total_trade) + log(GDP_per_capita) +  log(population_density) + 
               log(aged_65_older), data= FINAL_AID_DATA)

#моделі з W_Polity в якості залежної змінної
Model4 <- lm(log(USA_aid_total) ~  log(Population_2019) + W_Polity, data= FINAL_AID_DATA)
Model5 <- lm(log(USA_aid_total) ~  log(Population_2019) + W_Polity  + WGI_GovEff + GHS_Index + HAQ_Index, data= FINAL_AID_DATA)
Model6 <- lm(log(USA_aid_total) ~  log(Population_2019) + W_Polity + WGI_GovEff + GHS_Index + HAQ_Index + 
               log(Total_trade) + log(GDP_per_capita) +  log(population_density) + 
               log(aged_65_older), data= FINAL_AID_DATA)

#генерація коду в LaTeX для таблиць?
stargazer(Model1, Model2, Model3, type = "latex")
stargazer(Model4, Model5, Model6, type = "latex")

#перевірка залишків
par(mfrow=c(3,2)) 
plot(Model1$residuals, col = "red")
plot(Model2$residuals, col = "red")
plot(Model3$residuals, col = "red")
plot(Model4$residuals, col = "red")
plot(Model5$residuals, col = "red")
plot(Model6$residuals, col = "red")


