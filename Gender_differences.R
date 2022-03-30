#Gender (male/female)
Model <- 'N  =~ Q2 + Q3 + Q4 + Q7
LS =~ Q63 + Q65 + Q66 + Q67'

Data <- read.csv("NA_2020.csv")
Data <- filter(Data, gender == c("Male", "Female"))

fit <- cfa(Model, Data)
fit

fitMeasures(fit)
modificationindices(fit) %>% arrange(-mi)

Model2 <- 'N  =~ Q2 + Q3 + Q4 + Q7
LS =~ Q63 + Q65 + Q66 + Q67
Q4 ~~ Q7'

fit2 <- cfa(Model2, Data)
anova(fit, fit2)
#fit2 is a better model according to p values, AIC and BIC

#path diagram
library("OpenMx")
library("semPlot")
semPaths(fit2,"col", "name", 
         as.expression = c("nodes","edges"), 
         style = "lisrel", 
         residScale = 10, 
         layout = "tree3", 
         cardinal = "lat cov", 
         curvePivot = TRUE, 
         sizeMan = 4,
         sizeLat = 10, 
         edge.label.cex = 1,
         mar = c(9,1,8,1), 
         reorder = FALSE, 
         width = 8,
         height = 5, 
         groups = "latents", 
         pastel = TRUE, 
         borders = FALSE)

#Testing for measurement invariance----
#configural invariance
conf <- cfa(Model2, Data, group="gender", std.lv=TRUE)
conf #holds (p>0.05)

#weak invariance
weak <- cfa(Model2, Data, group = "gender", 
            group.equal = "loadings", std.lv=TRUE)

#Compare configural and weak
anova(conf, weak) #weak invariance accepted (p>0.05) and we move on

#Testing for strong invariance
strong <- cfa(Model2, Data, group="gender", 
              group.equal = c("loadings", "intercepts"), std.lv=TRUE)
#Compare 
anova(conf, weak, strong) #misfit spotted 

#Where is misfit?
residuals(strong)
#biggest misfit in Q2, Q3, Q7, Q63 and Q67
lavTestScore(strong)$uni %>% arrange(-X2)
parameterEstimates(strong)[c(21, 1, 25, 8, 22),]
#the highest is intercept for Q63
#let's free up 'Q63 ~ 1'

strong2 <- cfa(Model2, Data, group="gender", 
               group.equal = c("loadings", "intercepts"),
               group.partial = c("Q63~1"), std.lv=TRUE)
anova(conf, weak, strong, strong2)
#strong2 is better
#don't need for strict invariance test if we want to examine homogeneity

#choosing strong partial invariance
eqvars <- cfa(Model2, Data, group = "gender",
              group.equal = c("loadings","intercepts",
                              "lv.variances","lv.covariances"),
              group.partial = c("Q63~1"), std.lv=TRUE)

anova(strong2, eqvars)                       
#variances are qual across groups (p = 0.4107)

eqvars_and_means <- cfa(Model2, Data, group = "gender",
                        group.equal = c("loadings","intercepts",
                                        "lv.variances","lv.covariances", "means"),
                        group.partial = c("Q63~1"), std.lv=TRUE)

anova(strong2, eqvars, eqvars_and_means) #means not equal
nrow(parameterEstimates(eqvars))
parameterEstimates(eqvars)[59:60,]

#N~1 differs
#significant difference in Narcissism factor (p value = 0.031)
#Men score higher on Narcissism

