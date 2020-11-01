elim_fun_bn_new = function(selezione){
  if(length(selezione) != 0){
    load("save_bn.RData")
    save(h.max, residuo6180, residuo6250, duedate6180_bn, duedate6250_bn, X, scadenziario_bn, storia.residuo6180.bn, storia.residuo6250.bn, storia.date6180.bn,storia.date6250.bn, valore.soglia, ordinare, file = "xannulla_bn.RData")
    selezione = format(selezione, "%a %d %b %Y")
    storia.residuo6180.bn[which(scadenziario_bn$Data == selezione),] = NA
    storia.residuo6180.bn = na.omit(storia.residuo6180.bn)
    rownames(storia.residuo6180.bn) = c(as.character(1:nrow(storia.residuo6180.bn)))
    storia.residuo6250.bn[which(scadenziario_bn$Data == selezione),] = NA
    storia.residuo6250.bn = na.omit(storia.residuo6250.bn)
    rownames(storia.residuo6250.bn) = c(as.character(1:nrow(storia.residuo6250.bn)))
    storia.date6180.bn[which(scadenziario_bn$Data == selezione),] = NA
    storia.date6180.bn = na.omit(storia.date6180.bn)
    rownames(storia.date6180.bn) = c(as.character(1:nrow(storia.date6180.bn)))
    storia.date6250.bn[which(scadenziario_bn$Data == selezione),] = NA
    storia.date6250.bn = na.omit(storia.date6250.bn)
    rownames(storia.date6250.bn) = c(as.character(1:nrow(storia.date6250.bn)))
    ordinare[which(scadenziario_bn$Data == selezione),] = NA
    ordinare = na.omit(ordinare)
    for(x in 1:nrow(ordinare)){
      ordinare[x,] = x
    }
    rownames(ordinare) = c(as.character(1:nrow(ordinare)))
    scadenziario_bn = scadenziario_bn[,-1]
    scadenziario_bn$Data[which(scadenziario_bn$Data == selezione)] = NA
    scadenziario_bn = na.omit(scadenziario_bn)
    scadenziario_bn = cbind(ordinare, scadenziario_bn)
    colnames(scadenziario_bn) = c("#","Data", "Ordine", "Macchina", "Formato", "Titolo", "Editore", "Copie", "Click", "Tempo residuo")
    rownames(scadenziario_bn) = c(as.character(1:nrow(scadenziario_bn)))
    save(h.max, residuo6180, residuo6250, duedate6180_bn, duedate6250_bn, X, scadenziario_bn, storia.residuo6180.bn, storia.residuo6250.bn, storia.date6180.bn, storia.date6250.bn, valore.soglia, ordinare, file = "save_bn.RData")
    showNotification("Ben fatto! Hai completato un'altra giornata lavorativa!", type = "message")
  } else {
    showNotification("Attenzione! Data selezionata non valida!", type = "error")
    return()
  }

}