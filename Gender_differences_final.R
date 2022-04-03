#Final assignment ----
#Lora Čuljak----
#Ajda Flisar----
#Ana-Maria Gore----
#Nikolina Vukšić----

#Data uploaded from OSF (Network Analysis course 2020)

library(lavaan)
library(dplyr)
library(semPlot)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Data <- na.omit(read.csv("NA_2020_data.csv"))

Data <- filter(Data, gender == c("Male", "Female"))

Model <- 'N  =~ Q2 + Q3 + Q4 + Q7
LS =~ Q63 + Q65 + Q66 + Q67'

fit <- cfa(Model, Data)
fit
# Model Test User Model:
#   
#   Test statistic                                30.678
#   Degrees of freedom                                19
#   P-value (Chi-square)                           0.044
### p-value is significant -> hypothesis of exact fit is rejected

#Improving the model ----

modificationindices(fit) %>% arrange(-mi)
#    lhs op rhs     mi    epc sepc.lv sepc.all sepc.nox
# 1   Q4 ~~  Q7 10.150  0.548   0.548    0.301    0.301
### the highest is residual correlation between Q4 and Q7

#Including Q4 ~~ Q7 in our model
Model2 <- 'N  =~ Q2 + Q3 + Q4 + Q7
LS =~ Q63 + Q65 + Q66 + Q67
Q4 ~~ Q7'

fit2 <- cfa(Model2, Data)
anova(fit, fit2)
# Chi-Squared Difference Test
# 
#      Df    AIC    BIC  Chisq Chisq diff Df diff Pr(>Chisq)   
# fit2 18 4542.8 4599.5 21.299                                 
# fit  19 4550.2 4603.7 30.678     9.3796       1   0.002194 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
### fit2 is a better model according to the p value, AIC and BIC

#Path diagram
semPaths(fit2,
         what = "std", 
         whatLabels = "est", 
         style = "lisrel", 
         residScale = 8, 
         theme = "colorblind", 
         rotation = 1, 
         layout = "tree2", 
         cardinal = "lat cov", 
         curvePivot = TRUE, 
         sizeMan = 4, 
         sizeLat = 10, 
         mar = c(5,5,5,5))

#Testing for measurement invariance----
#configural invariance
conf <- cfa(Model2, Data, group="gender", std.lv=TRUE)
conf
# Model Test User Model:
#   
#   Test statistic                                49.416
#   Degrees of freedom                                36
#   P-value (Chi-square)                           0.067
#   Test statistic for each group:
#     Female                                      15.019
#     Male                                        34.397
### configural holds (p>0.05)

#weak invariance
weak <- cfa(Model2, Data, group = "gender", 
            group.equal = "loadings", std.lv=TRUE)

#Compare configural and weak
anova(conf, weak) 
# Chi-Squared Difference Test
# 
#      Df    AIC    BIC  Chisq Chisq diff Df diff Pr(>Chisq)  
# conf 36 4549.5 4713.1 49.416                                
# weak 42 4548.4 4693.2 60.336      10.92       6    0.09087 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
### weak invariance accepted (p>0.05) and we move on

#Testing for strong invariance
strong <- cfa(Model2, Data, group="gender", 
              group.equal = c("loadings", "intercepts"), std.lv=TRUE)
#Compare 
anova(conf, weak, strong) 
# Chi-Squared Difference Test
# 
#        Df    AIC    BIC  Chisq Chisq diff Df diff Pr(>Chisq)  
# conf   36 4549.5 4713.1 49.416                                
# weak   42 4548.4 4693.2 60.336     10.920       6    0.09087 .
# strong 48 4552.5 4678.4 76.412     16.076       6    0.01335 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
### misfit spotted 

#Where is misfit?
residuals(strong)
# $Female$mean
#     Q2     Q3     Q4     Q7    Q63    Q65    Q66    Q67 
# -0.145  0.037  0.072 -0.039 -0.108  0.014  0.036  0.083 
# $Male$mean
#    Q2     Q3     Q4     Q7    Q63    Q65    Q66    Q67 
# 0.231 -0.203 -0.097  0.113  0.130 -0.064 -0.088 -0.137 
### biggest misfit in Q2, Q3, Q7, Q63 and Q67

lavTestScore(strong)$uni %>% arrange(-X2)
parameterEstimates(strong)[c(21, 1, 25),]
#    lhs op rhs block group label   est    se      z pvalue ci.lower ci.upper
# 21  Q2 ~1         1     1 .p21. 4.125 0.121 34.133      0    3.889    4.362
# 1    N =~  Q2     1     1  .p1. 0.817 0.131  6.215      0    0.559    1.074
# 25 Q63 ~1         1     1 .p25. 4.594 0.122 37.762      0    4.355    4.832
### the highest is intercept for Q63

#let's free up 'Q63 ~ 1' and do the partial invariance test
strong2 <- cfa(Model2, Data, group="gender", 
               group.equal = c("loadings", "intercepts"),
               group.partial = c("Q63~1"), std.lv=TRUE)
anova(conf, weak, strong, strong2)
# Chi-Squared Difference Test
# 
# Df    AIC    BIC  Chisq Chisq diff Df diff Pr(>Chisq)  
# conf    36 4549.5 4713.1 49.416                                
# weak    42 4548.4 4693.2 60.336    10.9204       6    0.09087 .
# strong2 47 4548.3 4677.3 70.202     9.8654       5    0.07914 .
# strong  48 4552.5 4678.4 76.412     6.2107       1    0.01270 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
### strong2 is better than strong

#no need for strict invariance test if we want to test for homogeneity

#choosing strong partial invariance
#testing whether variances and means are equal (females/males)
eqvars <- cfa(Model2, Data, group = "gender",
              group.equal = c("loadings","intercepts",
                              "lv.variances","lv.covariances"),
              group.partial = c("Q63~1"), std.lv=TRUE)
anova(strong2, eqvars)                       
# Chi-Squared Difference Test
# 
# Df    AIC    BIC  Chisq Chisq diff Df diff Pr(>Chisq)
# strong2 47 4548.3 4677.3 70.202                              
# eqvars  50 4545.1 4664.7 73.080     2.8786       3     0.4107
### variances are equal across groups (p = 0.4107)

eqvars_and_means <- cfa(Model2, Data, group = "gender",
                        group.equal = c("loadings","intercepts",
                                        "lv.variances","lv.covariances", "means"),
                        group.partial = c("Q63~1"), std.lv=TRUE)

anova(strong2, eqvars, eqvars_and_means)
# Chi-Squared Difference Test
# 
# Df    AIC    BIC  Chisq Chisq diff Df diff Pr(>Chisq)  
# strong2          47 4548.3 4677.3 70.202                                
# eqvars           50 4545.1 4664.7 73.080     2.8786       3    0.41073  
# eqvars_and_means 52 4547.6 4661.0 79.582     6.5020       2    0.03874 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
### means are not equal (p=0.039*)

#Let's see where is the difference (which factor/s and which direction?)
nrow(parameterEstimates(eqvars)) # [1] 60
parameterEstimates(eqvars)[59:60,]
#    lhs op rhs block group label    est    se      z pvalue ci.lower ci.upper
# 59   N ~1         2     2        0.423 0.197  2.152  0.031    0.038    0.809
# 60  LS ~1         2     2       -0.181 0.173 -1.048  0.294   -0.520    0.157

#N~1 differs across groups
#There is a significant difference in Narcissism factor (p value = 0.031)
#On our scale, men scored higher on Narcissism than women (testing on partial strong invariance model)


