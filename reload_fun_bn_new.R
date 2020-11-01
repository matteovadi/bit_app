reload_fun_bn_new = function(){
  load("xannulla_bn.RData")
  save(h.max, residuo6180, residuo6250, duedate6180_bn, duedate6250_bn, X, scadenziario_bn, storia.residuo6180.bn, storia.residuo6250.bn, storia.date6180.bn, storia.date6250.bn, valore.soglia, ordinare, file = "save_bn.RData")
}