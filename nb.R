train = function(instances, labels, class_name) {
  model = list()
  
  prior.base = list()
  prior.evidence = list()
  prior.conditional = list()
  
  outcome_number = length(labels)
  for (i in 1:outcome_number) {
    outcome = labels[i]
    outcome_count = nrow(instances[ instances[[class_name]] == outcome, ])
    prior.base[[outcome]] = outcome_count/nrow(instances)
  }
  
  evidence_number = ncol(instances) - 1
  for (i in 1:evidence_number) {
    evidence = names(instances)[i]
    evidence_count = nrow(instances[ instances[[evidence]] == 1, ])
    prior.evidence[[evidence]] = evidence_count/nrow(instances)
  }
  
  for (i in 1:evidence_number) {
    conditional_prob = list()
    for (j in 1:outcome_number) {
      outcome = labels[j]
      out_given_evid = nrow(instances[ instances[i] == 1 & instances[[class_name]] == outcome, ])
      evidence_number = nrow(instances[ instances[[class_name]] == outcome, ])
      conditional_prob[[outcome]] = out_given_evid / evidence_number
    }
    evidence = names(instances)[i]
    prior.conditional[[evidence]] = conditional_prob
  }
  
  model$base = prior.base
  model$evidence = prior.evidence
  model$conditional = prior.conditional
  return(model)
}

test = function(model, instances, labels, class_name) {
  instance_count = nrow(instances)
  label_count = length(labels)
  evidence_count = length(names(instances)) - 1 # -1 is Fruit (class name)
  for (i in 1:instance_count) {
    instance = instances[i,]
    p = rep(1, label_count)
    p_evidence = 1
    for (j in 1:label_count) {
      label = labels[j] # Banana, Orange, Other
      for (k in 1:evidence_count) {
        evidence = instance[k] # Long, Sweet, Yellow
        p_evidence = p_evidence * model$evidence[[k]]
        if (evidence == 1) {
          p[j] = p[j] * model$conditional[[k]][[label]]
        } else {
          p[j] = p[j] * (1 - model$conditional[[k]][[label]])
        }
      }
      p[j] = p[j] * model$base[[label]]
    }
    highest_position = which.max(p)
    prediction = labels[highest_position]
    instances[i,label_count+1] = prediction
  }
  return(instances)
}

instances = as.data.frame(rbind(
  c(1,1,1,"Banana"),
  c(1,1,0,"Banana"),
  c(1,0,1,"Banana"),
  c(1,0,0,"Banana"),
  c(1,1,0,"Other"),
  c(0,1,1,"Other"),
  c(0,1,0,"Orange"),
  c(0,0,0,"Orange"),
  c(0,0,1,"Orange")))
names(instances) = c("Long", "Sweet", "Yellow", "Fruit")
labels = c("Banana", "Orange", "Other")

nb_model = train(instances, labels, "Fruit")

unseen_instances = as.data.frame(rbind(
  c(0,0,0,NaN),
  c(1,1,1,NaN)
))
names(unseen_instances) = c("Long", "Sweet", "Yellow", "Fruit")

predictions = test(nb_model, unseen_instances, labels, "Fruit")
