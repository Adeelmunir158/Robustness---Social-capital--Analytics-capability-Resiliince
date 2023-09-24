

install.packages("goftest")
install.packages("seminr")
require(readxl)
require(seminr)
attach(Robustness)
View(Robustness)
#===========================================

measurements <- constructs(
  composite("Technical Capability",multi_items("TEC",1:3)),
  composite("Management Capability",multi_items("MAN",1:3)),
  composite("Human Resource Capability",multi_items("HUM",1:3)),
  composite("Data driven Culture",multi_items("DAT",1:3)),
  higher_composite("Analytics Capability",c("Technical Capability","Management Capability","Human Resource Capability","Data driven Culture"), method = "two stage"),
  composite("Robustness",multi_items("ROBUST",1:5)),
  composite("Social Capital",multi_items("SOCAP",1:4)),
  composite("Absorptive Capability",multi_items("ABCAP",1:5)),
  composite("Adaptive Capability",multi_items("ADCAP",1:5)),
  composite("Restorative Capability",multi_items("RESCAP",1:3)),
  higher_composite("Resilience",c("Absorptive Capability","Adaptive Capability","Restorative Capability"), method = "two stage"))

#------------------------------------------
# Quickly create multiple paths "from" and "to" sets of constructs

plot(measurements)

structure_pls = relationships(
  paths(from = "Robustness", to = "Analytics Capability"),
  paths(from = "Robustness", to = "Resilience"),
  paths(from = "Analytics Capability", to = "Resilience"),
  paths(from = "Social Capital", to = "Analytics Capability"),
  paths(from = "Social Capital", to = "Resilience"))

plot(structure_pls)



#===========================================

pls_model = estimate_pls(Robustness,
                         measurements,
                         structure_pls)





plot_scores(pls_model)
plot(pls_model, cex=3)
s=summary(pls_model)

s$loadings
s$descriptives
#Validity
s$validity
s$total_effects
s$it_criteria
s$fSquare
s$reliability
s$validity$fl_criteria
s$weights
s$vif_antecedents$Ambidexterity
s$descriptives$statistics$items
s$paths
#Reliability
sink("mock_test.doc")
s$reliability
sink()
#Loadings
s$loadings
s$iterations
s$loadings
#Structural Model
s$paths
s$total_effects
s$total_indirect_effects
s$vif_antecedents
s$fSquare
s$it_criteria
s$composite_scores
s$validity$htmt
# Bootstrap
p5=bootstrap_model(pls_model,nboot = 5000)
s2=summary(p5)
s2
s2$bootstrapped_HTMT
#===========================================