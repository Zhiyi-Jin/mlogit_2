#libraries
library(socviz)
library(tidyverse)
library(nnet)
library(stargazer)
library(lmtest)
library(kableExtra)

#Part 1
#load the data
gss <- tibble(gss_sm)

#fit model
m1 <- multinom(relevel(degree, ref = "Lt High School") ~ padeg, data = gss, na.action = na.exclude)

#make a table
stargazer(m1, header = FALSE, type = "html",
          covariate.labels = c("FatherHigh School(Ref = < High School)",
                               "FatherJunior College",
                               "FatherBachelor",
                               "FatherGraduate"),
          dep.var.caption = "Children Educational Attainment(Ref = < High School)",
          notes = "Data from General Social Survey",
          title = "Table: Multinomial logistic regression")

#generate data for prediction
pred_dt <- expand_grid(padeg = c("Lt High School", "High School",
                                 "Junior College", "Bachelor", "Graduate"))
#make predictions
preds <- predict(m1, newdata = pred_dt, "probs") %>% 
  as_tibble() %>% 
  bind_cols(pred_dt)
#change to long format
preds_long <- preds %>% 
  pivot_longer(cols = c("Lt High School", "High School",
                        "Junior College", "Bachelor", "Graduate"),
               names_to = "degree",
               values_to = "preds")
#make a plot
theme_set(theme_light())

ggplot(preds_long, aes(y = preds)) +
  geom_col(aes(x = padeg, fill = degree)) + 
  labs(title = "Figure: Predicted Children Education Attainment",
       x = "Father's education level",
       y = "Predicted probability",
       fill = "Degree of Children") 
 
#fit a new model
m2 <- multinom(relevel(degree, ref = "Lt High School") ~ padeg + madeg, data = gss, na.action = na.exclude)#regression table
stargazer(m1,m2, header = FALSE, type = "html",
          covariate.labels = c("FatherHigh School(Ref = < High School)",
                               "FatherJunior College",
                               "FatherBachelor",
                               "FatherGraduate",
                               "MotherHigh School(Ref = < High School)",
                               "MotherJunior College",
                               "MotherBachelor",
                               "MotherGraduate"),
          dep.var.caption = "Children Educational Attainment(Ref = < High School)",
          notes = "Data from General Social Survey",
          title = "Table: Multinomial logistic regression")

#likelihood ratio test and BIC statistics
tibble(Model = list(m1, m2)) %>% 
  mutate(LogLik = map_dbl(Model, logLik), 
         df = map_int(Model, function(x) length(coef(x))) ,
         BIC = map_dbl(Model, BIC)) %>% 
  mutate(chi2 = abs(2*(LogLik - lag(LogLik))),
         chi2df = abs(df - lag(df)),
         p = pchisq(chi2, chi2df, lower.tail = F)) %>% 
  mutate(p = ifelse(p<.001, "<.001")) %>% 
  mutate(Model = c(1,2)) %>% 
  mutate(vs = lag(Model)) %>% 
  select(Model, LogLik, df, BIC, vs, chi2, chi2df, p) -> fittab

options(knitr.kable.NA = '-')
fittab %>% kbl(digits = 0, booktabs=T, align="c",
               col.names = c("Model",
                             "Log likelihood",
                             "Parameters",
                             "BIC",
                             "vs. Model",
                             "Chi sq.",
                             "df",
                             "p value"),
               caption = "Table: Fit statistics") %>% 
  kable_classic() %>% 
  kable_styling(full_width = F, position = "center") %>% 
  add_header_above(c(" " = 4,"Likelihood Ratio Tests" = 4))

#Part 2
#Split the sexual partners variable into a dichotomous indicator
n_gss <- gss %>% 
  mutate(partners = as.factor(partners),
         partners = na_if(partners,"1 or More, # Unknown"),
         partners = fct_recode(partners,
                               "1" = "5-10 Partners",
                               "1" = "11-20 Partners",
                               "1" = "21-100 Partners",
                               "0" = "No Partners",
                               "0" = "1 Partner",
                               "0" = "2 Partners",
                               "0" = "3 Partners",
                               "0" = "4 Partners",
                               "0" = "1 or More, # Unknown")) %>% 
  mutate(religion = fct_relevel(religion, "None"))

#Fit a model
m <- multinom(relevel(partners, ref = "0") ~ religion + marital, data = n_gss)

#Make a table
stargazer(m, header = FALSE, type = "html",
          covariate.labels = c("Protestant(Ref = None)",
                               "Catholic",
                               "Jewish",
                               "Other",
                               "Widowed(Ref = Married)",
                               "Divorced",
                               "Separated",
                               "Never Married"),
          dep.var.labels.include = F,
          dep.var.caption = "Sexual Partners(Ref = < 5)",
          notes = "Data from General Social Survey",
          title = "Multinomial logistic regression")
 
t1 <- with(n_gss, table(partners, religion))
t2 <- with(n_gss, table(partners, marital))

rownames(t1) <- c("Patners < 5", "Patners >= 5")
t1 %>% kbl(caption = "Table by Religion", 
           booktabs = T, align = "c") %>% 
  kable_classic() %>% 
  kable_styling(full_width = T, position = "center")

rownames(t2) <- c("Patners < 5", "Patners >= 5")
t2 %>% kbl(caption = "Table by Marital") %>% 
  kable_classic() %>% 
  kable_styling(full_width = T, position = "center")
 
#recode vatiables
m_gss <- n_gss %>% 
  mutate(religion2 = recode_factor(religion,
                                   "Jewish" = "Other"),
         marital2 = recode_factor(marital,
                                  "Separated" = "Divorced", 
                                  "Widowed" = "Married")) %>% 
  mutate(religion2 = fct_relevel(religion2, "None")) %>% 
  mutate(marital2 = fct_relevel(marital2, "Married"))

#fit a model
m3 <- multinom(relevel(partners, ref = "0") ~ religion2 + marital2, data = m_gss)

#make a table
stargazer(m3, header = FALSE, type = "html",
          covariate.labels = c("Other Religion(Ref = None)",
                               "Protestant",
                               "Catholic",
                               "Divorced(Ref = Married)",
                               "Never Married"),
          dep.var.labels.include = F,
          dep.var.caption = "Sexual Partners(Ref = < 5)",
          notes = "Data from General Social Survey",
          title = "Table: Multinomial logistic regression")
 
 
