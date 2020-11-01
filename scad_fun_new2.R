scad_fun_new2 = function(num_click, editore, titolo, num_copie, codice_ord, formato){
  library(RcppQuantuccia)
  if(!is.na(num_click) & editore != "" & titolo != "" & !is.na(num_copie) & codice_ord != "" & formato != ""){
    load("save_bn.RData")
    save(h.max, residuo6180, residuo6250, duedate6180_bn, duedate6250_bn, X, scadenziario_bn, storia.residuo6180.bn, storia.residuo6250.bn, storia.date6180.bn,storia.date6250.bn, valore.soglia, ordinare, file = "xannulla_bn.RData")
    scadenziario_bn = scadenziario_bn[,-1]
    if(num_click < valore.soglia){
      macchina = "6180"
      if(formato == "25x35"| formato == "25X35"){
        prod.oraria = 6500
      } else {
        prod.oraria = 5200
      }
      tempo.lav = num_click/prod.oraria
      while(residuo6180 - tempo.lav < 0){
        if(residuo6180 == 0){
          duedate6180_bn = duedate6180_bn + 1
          #vincoli sui giorni festivi
          for(i in 1:14){
            if (duedate6180_bn == X[i]){
              duedate6180_bn = duedate6180_bn + 1
            }
          }
          #sabato
          if(isWeekend(duedate6180_bn)) {
            duedate6180_bn = duedate6180_bn + 1
          }
          #domenica
          if(isWeekend(duedate6180_bn)) {
            duedate6180_bn = duedate6180_bn + 1
          }
          residuo6180 = h.max
        } else {
          num_click_format = num_click - (tempo.lav - residuo6180)*prod.oraria
          #aggiorna scadenario
          duedate_format = format(duedate6180_bn, "%a %d %b %Y")
          ordinare = rbind(ordinare, as.numeric(nrow(ordinare)+1))
          storia.residuo6180.bn = rbind(storia.residuo6180.bn, 0)
          storia.residuo6250.bn = rbind(storia.residuo6250.bn, residuo6250)
          storia.date6180.bn = rbind(storia.date6180.bn, as.character(duedate6180_bn))
          storia.date6250.bn = rbind(storia.date6250.bn, as.character(duedate6250_bn))            
          scadenziario_bn = rbind(scadenziario_bn, c(duedate_format, codice_ord, macchina, formato, titolo, editore, num_copie, round(num_click_format,0), 0))
          
          tempo.lav = tempo.lav - residuo6180
          num_click = tempo.lav*prod.oraria
          duedate6180_bn = duedate6180_bn + 1
          #vincoli sui giorni festivi
          for(i in 1:14){
            if (duedate6180_bn == X[i]){
              duedate6180_bn = duedate6180_bn + 1
            }
          }
          #sabato
          if(isWeekend(duedate6180_bn)) {
            duedate6180_bn = duedate6180_bn + 1
          }
          #domenica
          if(isWeekend(duedate6180_bn)) {
            duedate6180_bn = duedate6180_bn + 1
          }
          residuo6180 = h.max
        }
      }
      residuo6180 = residuo6180 - tempo.lav
      duedate_format = format(duedate6180_bn, "%a %d %b %Y")
      ordinare = rbind(ordinare, as.numeric(nrow(ordinare)+1))
      storia.residuo6180.bn = rbind(storia.residuo6180.bn, residuo6180)
      storia.residuo6250.bn = rbind(storia.residuo6250.bn, residuo6250)
      storia.date6180.bn = rbind(storia.date6180.bn, as.character(duedate6180_bn))
      storia.date6250.bn = rbind(storia.date6250.bn, as.character(duedate6250_bn))
      scadenziario_bn = rbind(scadenziario_bn, c(duedate_format, codice_ord, macchina, formato, titolo, editore, num_copie, round(num_click,0), round(residuo6180,2)))
  
    } else {                                                                            ##############
      macchina = "6250"
      if(formato == "25x35"| formato == "25X35"){
        prod.oraria = 9400
      } else {
        prod.oraria = 7400
      }
      tempo.lav = num_click/prod.oraria
      while(residuo6250 - tempo.lav < 0){
        if(residuo6250 == 0){
          duedate6250_bn = duedate6250_bn + 1
          #vincoli sui giorni festivi
          for(i in 1:14){
            if (duedate6250_bn == X[i]){
              duedate6250_bn = duedate6250_bn + 1
            }
          }
          #sabato
          if(isWeekend(duedate6250_bn)) {
            duedate6250_bn = duedate6250_bn + 1
          }
          #domenica
          if(isWeekend(duedate6250_bn)) {
            duedate6250_bn = duedate6250_bn + 1
          }
          residuo6250 = h.max
        } else {
          num_click_format = num_click - (tempo.lav - residuo6250)*prod.oraria
          #aggiorna scadenario
          duedate_format = format(duedate6250_bn, "%a %d %b %Y")
          ordinare = rbind(ordinare, as.numeric(nrow(ordinare)+1))
          storia.residuo6180.bn = rbind(storia.residuo6180.bn, residuo6180)
          storia.residuo6250.bn = rbind(storia.residuo6250.bn, 0)
          storia.date6180.bn = rbind(storia.date6180.bn, as.character(duedate6180_bn))
          storia.date6250.bn = rbind(storia.date6250.bn, as.character(duedate6250_bn))            
          scadenziario_bn = rbind(scadenziario_bn, c(duedate_format, codice_ord, macchina, formato, titolo, editore, num_copie, round(num_click_format,0), 0))
          
          tempo.lav = tempo.lav - residuo6250
          num_click = tempo.lav*prod.oraria
          duedate6250_bn = duedate6250_bn + 1
          #vincoli sui giorni festivi
          for(i in 1:14){
            if (duedate6250_bn == X[i]){
              duedate6250_bn = duedate6250_bn + 1
            }
          }
          #sabato
          if(isWeekend(duedate6250_bn)) {
            duedate6250_bn = duedate6250_bn + 1
          }
          #domenica
          if(isWeekend(duedate6250_bn)) {
            duedate6250_bn = duedate6250_bn + 1
          }
          residuo6250 = h.max
        }
      }
      residuo6250 = residuo6250 - tempo.lav
      duedate_format = format(duedate6250_bn, "%a %d %b %Y")
      ordinare = rbind(ordinare, as.numeric(nrow(ordinare)+1))
      storia.residuo6180.bn = rbind(storia.residuo6180.bn, residuo6180)
      storia.residuo6250.bn = rbind(storia.residuo6250.bn, residuo6250)
      storia.date6180.bn = rbind(storia.date6180.bn, as.character(duedate6180_bn))
      storia.date6250.bn = rbind(storia.date6250.bn, as.character(duedate6250_bn))
      scadenziario_bn = rbind(scadenziario_bn, c(duedate_format, codice_ord, macchina, formato, titolo, editore, num_copie, round(num_click,0), round(residuo6250,2)))
    } #if soglia
    scadenziario_bn = cbind(ordinare, scadenziario_bn)
    colnames(scadenziario_bn) = c("#","Data", "Ordine", "Macchina", "Formato", "Titolo", "Editore", "Copie", "Click", "Tempo residuo")
    colnames(storia.residuo6180.bn) = "Residuo6180"
    colnames(storia.residuo6250.bn) = "Residuo6250"
    colnames(storia.date6180.bn) = "Data6180"
    colnames(storia.date6250.bn) = "Data6250"
    colnames(ordinare) = "#"
    save(h.max, residuo6180, residuo6250, duedate6180_bn, duedate6250_bn, X, scadenziario_bn, storia.residuo6180.bn, storia.residuo6250.bn, storia.date6180.bn, storia.date6250.bn, valore.soglia, ordinare, file = "save_bn.RData")
  } else {
    showNotification("Attenzione! Uno o più valori mancanti!", type = "error")
    return()
  }
}

