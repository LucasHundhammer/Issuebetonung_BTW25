
library(haven)
library(ggplot2)
library(lm.beta)
library(survey)
library(srvyr)
library(carData)
library(car)
library(dplyr)
library(GGally)
library(VGAM)
library(lmtest)
library(lme4)
library(fixest)
library(nnet)
library(broom)
library(sandwich)
library(tidyr)
library(rms)
library(performance)
devtools::install_version("frm","1.2.2")
library(frm)

###                                                           ###
###                                                           ###
###   Teil 1: Regressionen und Plots mit Tweets als Reihen    ###
###                                                           ###
###                                                           ###

## Datenverarbeitung

df = read.csv("Daten/kandis_uncombined.csv")




df <- df %>% 
  rename(Partei = GruppennameKurz) %>% 
  mutate(Union = case_when(Partei == "CDU/CSU" ~ 1, #Dummies für Parteizugehörigkeit
                           TRUE ~ 0),
         FDP = case_when(Partei == "FDP" ~ 1,
                         TRUE ~ 0),
         Grüne = case_when(Partei == "GRÜNE" ~ 1,
                           TRUE ~ 0),
         SPD = case_when(Partei == "SPD" ~ 1,
                         TRUE ~ 0),
         AfD = case_when(Partei == "AfD" ~ 1,
                         TRUE ~ 0),
         Linke = case_when(Partei == "LINKE" ~ 1,
                           TRUE ~ 0),
         BSW = case_when(Partei == "BSW" ~ 1,
                         TRUE ~ 0),
         Frau = case_when(Geschlecht == "w" ~ 1, #Dummy für Geschlecht
                          TRUE ~ 0),
         Ost = case_when(Ost_West == "Ost" ~ 1, #Dummy für Kandidierender in Ost- oder Westdeutschland
                         TRUE ~ 0),
         Direkt = case_when(Kennzeichen == "Kreiswahlvorschlag" ~ 1, #Dummy für Direkt- oder Listenkandidierender
                            TRUE ~ 0),
         date = as.Date.character(date),
         nach_Asch = case_when(date >= "2025-01-22" ~ 1,
                               TRUE ~ 0),                        #Dummy ob Post vor oder Nach dem Messerangriff in Aschaffenburg stattgefunden hat
         id_scraping = as.character(id_scraping),
         core_issue = case_when((D_1 == 1 & BSW == 1) | (D_2 == 1 & FDP == 1) | (D_3 == 1 & (Union == 1 | FDP == 1 )) | (D_4 == 1 & (SPD == 1 | Linke == 1 )) | (D_5 == 1 & Grüne == 1) | ((D_6 == 1 & (Union == 1 | AfD == 1))) |(D_7 == 1 & AfD == 1) ~ 1,
                               TRUE ~ 0),             #Issue Ownership
         date = as.Date(date)) 

df$alter <- 2025- df$Geburtsjahr                       # Alter-Variable
df$date_num <- df$date - as.Date("2024-11-06")
df$date_num <- as.numeric(df$date_num)                  #Datums-Variable


### 
### Explorativ
###

## Anzahl an Posts nach Parteien

df_Partei <-  df %>%  
  group_by(id_scraping, Partei) %>% 
  count() %>% 
  group_by(Partei) %>% 
  count()


df %>% 
  group_by(Partei) %>% 
  count(D_0) #D_0=1 heißt, ein Post ist unpolitisch, D_0=0 heißt, ein Post ist politisch. Daraus ergibt sich der untere Teil von Tab. 1


## Trennung nach Kategorien

df <- df %>% 
  filter(D_0==0)

  
df_1 <- df %>% 
  group_by(D_1) %>% 
  count(Partei) %>% 
  rename("Anzahl_1" = "n") %>% 
  filter(D_1 == 1)

df_2 <- df %>% 
  group_by(D_2) %>% 
  count(Partei) %>% 
  rename("Anzahl_2" = "n") %>% 
  filter(D_2 == 1)

df_3 <- df %>% 
  group_by(D_3) %>% 
  count(Partei) %>% 
  rename("Anzahl_3" = "n") %>% 
  filter(D_3 == 1)

df_4 <- df %>% 
  group_by(D_4) %>% 
  count(Partei) %>% 
  rename("Anzahl_4" = "n") %>% 
  filter(D_4 == 1)

df_5 <- df %>% 
  group_by(D_5) %>% 
  count(Partei) %>% 
  rename("Anzahl_5" = "n") %>% 
  filter(D_5 == 1)

df_6 <- df %>% 
  group_by(D_6) %>% 
  count(Partei) %>% 
  rename("Anzahl_6" = "n") %>% 
  filter(D_6 == 1)

df_7 <- df %>% 
  group_by(D_7) %>% 
  count(Partei) %>% 
  rename("Anzahl_7" = "n") %>% 
  filter(D_7 == 1)

df_comb <- merge(df_1,df_2, by="Partei")
df_comb <- merge(df_comb,df_3, by="Partei")
df_comb <- merge(df_comb,df_4, by="Partei")
df_comb <- merge(df_comb,df_5, by="Partei")
df_comb <- merge(df_comb,df_6, by="Partei")
df_comb <- merge(df_comb,df_7, by="Partei")

df_comb <- df_comb %>% 
  select(Partei,Anzahl_1,Anzahl_2,Anzahl_3,Anzahl_4,Anzahl_5,Anzahl_6,Anzahl_7) %>% 
  bind_rows(summarise(., across(where(is.numeric), sum),across(where(is.character), ~'Total')))

df_comb$Total <- rowSums(df_comb[2:8])

df_comb <- df_comb %>% 
  mutate(round((Anteil_1 = Anzahl_1/Total)*100,2),
         round((Anteil_2 = Anzahl_2/Total)*100,2),
         round((Anteil_3 = Anzahl_3/Total)*100,2),
         round((Anteil_4 = Anzahl_4/Total)*100,2),
         round((Anteil_5 = Anzahl_5/Total)*100,2),
         round((Anteil_6 = Anzahl_6/Total)*100,2),
         round((Anteil_7 = Anzahl_7/Total)*100,2)) #Hieraus ergibt sich Tab. 2 (die Anzahl der Themennennungen ist die letzte Reihe des columns "Total")


# Issue Anteile über Zeit
#Für jeden Tag wird die Summe der Themen berechnet und dann durch die Gesamtsumme der Themen geteilt. Daraus erhält man den Anteil eines Themas für jeden Tag

df_verlauf <- df %>% 
  select(date,id,id_scraping,D_1,D_2,D_3,D_4,D_5,D_6,D_7) %>% 
  group_by(date) %>% 
  summarise(D_1 = sum(D_1),                               
            D_2 = sum(D_2),
            D_3 = sum(D_3),
            D_4 = sum(D_4),
            D_5 = sum(D_5),
            D_6 = sum(D_6),
            D_7 = sum(D_7),
            summe = D_1+D_2+D_3+D_4+D_5+D_6+D_7,
            D_1 = D_1/summe,
            D_2 = D_2/summe,
            D_3 = D_3/summe,
            D_4 = D_4/summe,
            D_5 = D_5/summe,
            D_6 = D_6/summe,
            D_7 = D_7/summe)



df_verlauf <- df_verlauf %>% 
  pivot_longer(cols=c("D_1","D_2","D_3","D_4","D_5","D_6","D_7"),
               names_to="Thema",
               values_to="Anteile")


# Verlauf für alle Themen:

ggplot(df_verlauf, aes(x=date, y= Anteile, color=Thema)) +                                  #sehr unübersichtlich, außerdem verändert sich der Verlauf von D_1 (Aussen- und Verteidigungspolitik), D_4 (Wohlfahrt und Lebensqualität) und D_5 (Umwelt) kaum 
  geom_point(size=1.5) +                                                                    # Deshalb werden diese Themen entfernt
  geom_line(size=1) +
  geom_vline(xintercept = as.numeric(df_verlauf$date[dates]), color="black", size=1) +
  annotate("text", label="Messerangriff in Aschaffenburg",y=0.45, x=20092, size=4.5, fontface="bold") +
  scale_y_continuous(limit=c(0,0.5), labels = scales::percent) +
  labs(x="Verlauf des Wahlkampfes", y="Anteil an täglicher Themenbetonung") +
  theme_bw()                               


# Verlauf nur für Themen die sich eindeutig verändert haben nach Aschaffenburg 

df_verlauf <- df_verlauf %>% 
  filter(Thema == "D_2" | Thema == "D_3" | Thema== "D_6" | Thema == "D_7") %>% 
  mutate(Thema = recode(Thema, D_2 = "Freiheit, Demokratie und Politisches System",
                        D_3="Wirtschaft, Finanzen und Bürokratie", D_6="Sicherheit und Gesellschaftsgefüge",
                        D_7 = "Migration und Asyl"))

dates <- as.Date(c("2025-01-22")) # Messerangriff Aschaffenburg; 
dates <- which(df_verlauf$date %in% dates)


verlauf <- ggplot(df_verlauf, aes(x=date, y= Anteile, color=Thema)) +
  geom_point(size=1.5) +
  geom_line(size=1) +
  geom_vline(xintercept = as.numeric(df_verlauf$date[dates]), color="black", size=1) +
  annotate("text", label="Messerangriff in Aschaffenburg",y=0.45, x=20092, size=4.5, fontface="bold") +
  scale_y_continuous(limit=c(0,0.5), labels = scales::percent) +
  scale_color_manual(values = c("Freiheit, Demokratie und Politisches System"="red","Wirtschaft, Finanzen und Bürokratie"="violet",
                                "Sicherheit und Gesellschaftsgefüge"="#4caf51","Migration und Asyl"="blue"))+
  labs(x="Verlauf des Wahlkampfes", y="Anteil an täglicher Themenbetonung") +
  guides(color=guide_legend(nrow=1, byrow=TRUE)) +
  theme_bw() +
  theme(legend.position = "bottom",title = element_text(size= 12), plot.title = element_text(hjust = 0.5), legend.text = element_text(size=9), legend.title = element_blank())

verlauf

#ggsave("C:/Users/hundh/Desktop/Python/Graphiken/Verlauf.png",
#       plot = verlauf,
#       width = 3000,
#       height = 1500,
#       bg="white",
#       units = "px")



###
### Hypothesen mit Bezug auf Kandidierende (Modell 1)
###

## Logit-Regression mit Cluster-robusten Standardfehlern

df %>% 
  group_by(Partei) %>% 
  count()


logit <- glm(core_issue ~ Ost+Direkt+date_num+Union+SPD+AfD+BSW+FDP+Linke+alter+Frau+nach_Asch, df, family = binomial)
summary(logit)


robust_se <- coeftest(logit, vcov=vcovCL(logit, cluster=df$id_scraping))
robust_se


exp(cbind("Odds ratio" = coef(logit, vcov=vcovCL(logit, cluster=df$id_scraping)), confint.default(logit, level = 0.95)))     #Odds Ratios mit 95%-Konfidenzintervallen

nagel <- rms::lrm(core_issue ~ Ost+Direkt+date_num+FDP+Union+Linke+SPD+AfD+BSW+alter+Frau+nach_Asch, df)
print(nagel) #Nagelkerkes R2 unter 'Discrimination Indexes' erste Reihe


###
### Hypothesen mit Bezug auf Volksparteien (Modell 2)
### 

## Logit-Regression mit Cluster-robusten Standardfehlern

logit <- glm(core_issue ~ Ost+Direkt+date_num+Union+SPD+alter+Frau+nach_Asch, df, family = binomial)


robust_se <- coeftest(logit, vcov=vcovCL(logit, cluster=df$id_scraping))
robust_se

exp(cbind("Odds ratio" = coef(logit, vcov=vcovCL(logit, cluster=df$id_scraping)), confint.default(logit, level = 0.95)))     #Odds Ratios mit 95%-Konfidenzintervallen

nagel <- rms::lrm(core_issue ~ Union+SPD+Ost+Direkt+date_num+alter+Frau+nach_Asch, df)
print(nagel) #Nagelkerkes R2 unter 'Discrimination Indexes' erste Reihe


## Ende


###                                                         ###
###                                                         ###
###   Teil 2: Regressionen mit Kandidierenden als Reihen    ###
###                                                         ###
###                                                         ###



# Datenverarbeitung

df = read.csv("Daten/kandis_party_merge.csv")   #Hieraus ergibt sich die im Methodenteil erwähnte Gesamtanzahl (N=11865)
df <- df %>% 
  rename(Partei = GruppennameKurz)

df_nach = read.csv("Daten/kandis_party_merge_nach.csv")
df_nach <- df_nach %>% 
  rename(Partei = GruppennameKurz)

df_vor = read.csv("Daten/kandis_party_merge_vor.csv")
df_vor <- df_vor %>% 
  rename(Partei = GruppennameKurz)


df <- df %>% 
  mutate(D_1 = case_when(variable == "D_1" ~ 1,
                         TRUE ~ 0),
         D_2 = case_when(variable == "D_2" ~ 1,
                         TRUE ~ 0),
         D_3 = case_when(variable == "D_3" ~ 1,
                         TRUE ~ 0),
         D_4 = case_when(variable == "D_4" ~ 1,
                         TRUE ~ 0),
         D_5 = case_when(variable == "D_5" ~ 1,
                         TRUE ~ 0),
         D_6 = case_when(variable == "D_6" ~ 1,
                         TRUE ~ 0),
         D_7 = case_when(variable == "D_7" ~ 1,
                         TRUE ~ 0))

df_nach <- df_nach %>% 
  mutate(D_1 = case_when(variable == "D_1" ~ 1,
                         TRUE ~ 0),
         D_2 = case_when(variable == "D_2" ~ 1,
                         TRUE ~ 0),
         D_3 = case_when(variable == "D_3" ~ 1,
                         TRUE ~ 0),
         D_4 = case_when(variable == "D_4" ~ 1,
                         TRUE ~ 0),
         D_5 = case_when(variable == "D_5" ~ 1,
                         TRUE ~ 0),
         D_6 = case_when(variable == "D_6" ~ 1,
                         TRUE ~ 0),
         D_7 = case_when(variable == "D_7" ~ 1,
                         TRUE ~ 0))

df_vor <- df_vor %>% 
  mutate(D_1 = case_when(variable == "D_1" ~ 1,
                         TRUE ~ 0),
         D_2 = case_when(variable == "D_2" ~ 1,
                         TRUE ~ 0),
         D_3 = case_when(variable == "D_3" ~ 1,
                         TRUE ~ 0),
         D_4 = case_when(variable == "D_4" ~ 1,
                         TRUE ~ 0),
         D_5 = case_when(variable == "D_5" ~ 1,
                         TRUE ~ 0),
         D_6 = case_when(variable == "D_6" ~ 1,
                         TRUE ~ 0),
         D_7 = case_when(variable == "D_7" ~ 1,
                         TRUE ~ 0))
###
### Explorativ
###


plot_vor <- ggplot(data=df_vor, aes(x=Anteil_Partei, y=Anteil_Kand)) +
  geom_point(size=2, aes(color=Partei)) +
  scale_y_continuous(limit=c(0,1), labels = scales::percent) +
  scale_x_continuous(limit=c(0,0.4), labels = scales::percent) +
  scale_color_manual(breaks= c("LINKE","AfD","CDU/CSU","FDP","GRÜNE","SPD","BSW"),
                     values = c("CDU/CSU" = "black",
                                "SPD" = "#E3000F",
                                "GRÜNE" = "#46962b",
                                "FDP" = "#ffed00",
                                "LINKE" = "#6F003C",
                                "AfD" = "#009ee0",
                                "BSW" = "#FAACA8")) +
  theme_minimal() +
  guides(colour = guide_legend(nrow = 1)) +
  labs(x="Themenanteil im Wahlprogramm", y="Themenanteil in Kandidierendenposts", title= "Vor Veröffentlichung") +
  theme(legend.position = "bottom", title = element_text(size= 12), plot.title = element_text(hjust = 0.5))


plot_nach <- ggplot(data=df_nach, aes(x=Anteil_Partei, y=Anteil_Kand)) +
  geom_point(size=2, aes(color=Partei)) +
  scale_y_continuous(limit=c(0,1), labels = scales::percent) +
  scale_x_continuous(limit=c(0,0.4), labels = scales::percent) +
  scale_color_manual(breaks= c("LINKE","AfD","CDU/CSU","FDP","GRÜNE","SPD","BSW"),
                     values = c("CDU/CSU" = "black",
                                "SPD" = "#E3000F",
                                "GRÜNE" = "#46962b",
                                "FDP" = "#ffed00",
                                "LINKE" = "#6F003C",
                                "AfD" = "#009ee0",
                                "BSW" = "#FAACA8")) +
  theme_minimal() +
  guides(colour = guide_legend(nrow = 1)) +
  labs(x="Themenanteil im Wahlprogramm", y="Themenanteil in Kandidierendenposts", title= "Nach Veröffentlichung") +
  theme(legend.position = "bottom", title = element_text(size= 12), plot.title = element_text(hjust = 0.5))



plot_komb <- ggpubr::ggarrange(plot_vor, plot_nach, common.legend = TRUE, legend = c("bottom"), nrow=1)
plot_komb

ggsave("C:/Users/hundh/Desktop/Python/Graphiken/vor_nach.png",
       plot = plot_komb,
       width = 3000,
       height = 1500,
       bg="white",
       units = "px")

###
### Hypothese 5
###

## Fractional Logistic Regression
## Es wird das (mittlerweile nicht mehr auf CRAN verfügbare) package "frm" verwendet.
## Das entspricht dem Vorgehen von Berger und Jäger 2023 (die das gleiche package vom gleichen Autor in STATA verwenden).
## Es ist wichtig zu betonen, dass wir hier nicht versuchen, das "beste" Modell zu finden, sondern die Ergebnisse von Berger und Jäger (2024) für die BTW2025 so genau wie möglich nachprüfen wollen
## Ihr Vorgehen ist genauer in ihrem "supplementary Material" zu finden: https://journals.sagepub.com/doi/full/10.1177/13540688231194704


# Posts nach Veröffentlichung aller Wahlprogramme

y = df_nach$Anteil_Kand
x = cbind(df_nach$Anteil_Partei)
dimnames(x)[[2]] <- c("Anteil_Partei")


frm_nach <- frm(y,x, linkfrac = "loglog", table=TRUE)


# Posts vor Veröffentlichung aller Wahlprogramme

y = df_vor$Anteil_Kand
x = cbind(df_vor$Anteil_Partei)
dimnames(x)[[2]] <- c("Anteil_Partei")

frm_vor <- frm(y,x, linkfrac = "loglog", table=TRUE)
