urgent_fun_new = function(num_click, editore, titolo, num_copie, codice_ord, formato, selezione){
 if(!is.na(num_click) & editore != "" & titolo != "" & !is.na(num_copie) & codice_ord != "" & formato != "" & !is.null(selezione)){
   library(RcppQuantuccia)
   load("save_bn.RData")
   save(h.max, residuo6180, residuo6250, duedate6180_bn, duedate6250_bn, X, scadenziario_bn, storia.residuo6180.bn, storia.residuo6250.bn, storia.date6180.bn, storia.date6250.bn, valore.soglia, ordinare, file = "xannulla_bn.RData")
   scadenziario_bn = scadenziario_bn[,-1]
   
   duedate6180_bn = as.Date(storia.date6180.bn[(selezione),])
   duedate6250_bn = as.Date(storia.date6250.bn[(selezione),])         
   if(min(which(scadenziario_bn$Macchina[selezione:nrow(scadenziario_bn)] == "6180"))+selezione-1 != Inf){
      posizione6180 = min(which(scadenziario_bn$Macchina[selezione:nrow(scadenziario_bn)] == "6180"))+selezione-1
      #calcolo residuo
      if(scadenziario_bn$Formato[posizione6180] == "25x35" | scadenziario_bn$Formato[posizione6180] == "25X35" ){
         residuo6180 = as.numeric(storia.residuo6180.bn[posizione6180,]) + (as.numeric(scadenziario_bn$Click[posizione6180])/6500)
      }else{
         residuo6180 = as.numeric(storia.residuo6180.bn[posizione6180,]) + (as.numeric(scadenziario_bn$Click[posizione6180])/5200)
      }
   } else {
      residuo6180 = as.numeric(storia.residuo6180.bn[max(which(scadenziario_bn$Macchina[1:selezione] == "6180")),])
   }
   
   if(min(which(scadenziario_bn$Macchina[selezione:nrow(scadenziario_bn)] == "6250"))+selezione-1 != Inf){
      posizione6250 = min(which(scadenziario_bn$Macchina[selezione:nrow(scadenziario_bn)] == "6250"))+selezione-1
      #calcolo residuo
      if(scadenziario_bn$Formato[posizione6250] == "25x35" | scadenziario_bn$Formato[posizione6250] == "25X35" ){
         residuo6250 = as.numeric(storia.residuo6250.bn[posizione6250,]) + (as.numeric(scadenziario_bn$Click[posizione6250])/9400)
      }else{
         residuo6250 = as.numeric(storia.residuo6250.bn[posizione6250,]) + (as.numeric(scadenziario_bn$Click[posizione6250])/7400)
      }
   } else {
      residuo6250 = as.numeric(storia.residuo6250.bn[max(which(scadenziario_bn$Macchina[1:selezione] == "6250")),])
   }
   
   n = nrow(scadenziario_bn)
   scadenziario_bn = rbind(scadenziario_bn, c(0,0,0,0,0,0,0,0,0))
   vettore_q = matrix(0, nrow = n+1, ncol = 1)
   scadenziario_bn = cbind(scadenziario_bn, vettore_q)
   
   for (j in selezione:n){
     if(scadenziario_bn$Titolo[j] == scadenziario_bn$Titolo[j+1] & scadenziario_bn$Editore[j] == scadenziario_bn$Editore[j+1]){
       scadenziario_bn$vettore_q[j] = 1
     }  
   }
   for(w in 1:n){
     if(scadenziario_bn$vettore_q[w] == 1){
       scadenziario_bn$Click[w+1] = as.numeric(scadenziario_bn$Click[w+1]) + as.numeric(scadenziario_bn$Click[w])
     }
   }
   scadenziario_bn[which(scadenziario_bn$vettore_q == 1),] = NA
   scadenziario_bn = na.omit(scadenziario_bn)
   p = nrow(scadenziario_bn)
   rownames(scadenziario_bn) = c(as.character(1:p))
   scadenziario_bn = scadenziario_bn[-p,-10]
   
   # immagazzina
   scad_bn_aux = data.frame(NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL)
   for (y in selezione:nrow(scadenziario_bn)){
     if (nrow(scad_bn_aux) == 0){
       scad_bn_aux = data.frame(scadenziario_bn$Data[y],scadenziario_bn$Ordine[y], scadenziario_bn$Macchina[y], scadenziario_bn$Formato[y], scadenziario_bn$Titolo[y],scadenziario_bn$Editore[y], scadenziario_bn$Copie[y], scadenziario_bn$Click[y], scadenziario_bn$`Tempo residuo`[y])
       scadenziario_bn[y,] = NA
     } else {
       scad_bn_aux = rbind(scad_bn_aux, c(scadenziario_bn$Data[y],scadenziario_bn$Ordine[y], scadenziario_bn$Macchina[y], scadenziario_bn$Formato[y], scadenziario_bn$Titolo[y],scadenziario_bn$Editore[y], scadenziario_bn$Copie[y], scadenziario_bn$Click[y], scadenziario_bn$`Tempo residuo`[y]))
       scadenziario_bn[y,] = NA
     }                              
   }
   colnames(scad_bn_aux) = c("Data", "Ordine", "Macchina", "Formato", "Titolo", "Editore", "Copie", "Click", "Tempo residuo")
   for(t in selezione:nrow(storia.date6180.bn) ){
     storia.date6180.bn[t,] = NA
     storia.date6250.bn[t,] = NA
     storia.residuo6180.bn[t,] = NA
     storia.residuo6250.bn[t,] = NA
     ordinare[t,] = NA
   }
   scadenziario_bn = na.omit(scadenziario_bn)
   storia.residuo6180.bn = na.omit(storia.residuo6180.bn)
   storia.residuo6250.bn = na.omit(storia.residuo6250.bn)
   storia.date6180.bn = na.omit(storia.date6180.bn)
   storia.date6250.bn = na.omit(storia.date6250.bn)
   ordinare = na.omit(ordinare)
   if(nrow(scadenziario_bn) != 0){
     rownames(scadenziario_bn) = c(as.character(1:nrow(scadenziario_bn)))
     rownames(storia.date6180.bn) = c(as.character(1:nrow(storia.date6180.bn)))
     rownames(storia.date6250.bn) = c(as.character(1:nrow(storia.date6250.bn)))
     rownames(storia.residuo6180.bn) = c(as.character(1:nrow(storia.residuo6180.bn)))
     rownames(storia.residuo6250.bn) = c(as.character(1:nrow(storia.residuo6250.bn)))
     rownames(ordinare) = c(as.character(1:nrow(ordinare)))
   }
   
   # aggiunta nuova commessa
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
         if(nrow(scadenziario_bn) == 0){
           ordinare = data.frame(as.numeric(1))
           storia.residuo6180.bn = data.frame(0)
           storia.residuo6250.bn = data.frame(residuo6250)
           storia.date6180.bn = data.frame(as.character(duedate6180_bn))
           storia.date6250.bn = data.frame(as.character(duedate6250_bn))
           scadenziario_bn = data.frame(duedate_format, codice_ord, macchina, formato, titolo, editore, num_copie, round(num_click_format,0), 0)
         } else {
           ordinare = rbind(ordinare, as.numeric(nrow(ordinare)+1))
           storia.residuo6180.bn = rbind(storia.residuo6180.bn, 0)
           storia.residuo6250.bn = rbind(storia.residuo6250.bn, residuo6250)
           storia.date6180.bn = rbind(storia.date6180.bn, as.character(duedate6180_bn))
           storia.date6250.bn = rbind(storia.date6250.bn, as.character(duedate6250_bn))
           scadenziario_bn = rbind(scadenziario_bn, c( duedate_format, codice_ord, macchina, formato, titolo, editore, num_copie, round(num_click_format,0), 0))
         }
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
     if(nrow(scadenziario_bn) == 0){
       ordinare = data.frame(as.numeric(1))
       storia.residuo6180.bn = data.frame(residuo6180)
       storia.residuo6250.bn = data.frame(residuo6250)
       storia.date6180.bn = data.frame(as.character(duedate6180_bn))
       storia.date6250.bn = data.frame(as.character(duedate6250_bn))
       scadenziario_bn = data.frame(duedate_format, codice_ord, macchina, formato, titolo, editore, num_copie, round(num_click,0), round(residuo6180,2))
     } else {
       ordinare = rbind(ordinare, as.numeric(nrow(ordinare)+1))
       storia.residuo6180.bn = rbind(storia.residuo6180.bn, residuo6180)
       storia.residuo6250.bn = rbind(storia.residuo6250.bn, residuo6250)
       storia.date6180.bn = rbind(storia.date6180.bn, as.character(duedate6180_bn))
       storia.date6250.bn = rbind(storia.date6250.bn, as.character(duedate6250_bn))
       scadenziario_bn = rbind(scadenziario_bn, c(duedate_format, codice_ord, macchina, formato, titolo, editore, num_copie, round(num_click,0), round(residuo6180,2)))
     }
     
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
         if(nrow(scadenziario_bn) == 0){
           ordinare = data.frame(as.numeric(1))
           storia.residuo6180.bn = data.frame(residuo6180)
           storia.residuo6250.bn = data.frame(0)
           storia.date6180.bn = data.frame(as.character(duedate6180_bn))
           storia.date6250.bn = data.frame(as.character(duedate6250_bn))
           scadenziario_bn = data.frame(duedate_format, codice_ord, macchina, formato, titolo, editore, num_copie, round(num_click_format,0), 0)
         } else {
           ordinare = rbind(ordinare, as.numeric(nrow(ordinare)+1))
           storia.residuo6180.bn = rbind(storia.residuo6180.bn, residuo6180)
           storia.residuo6250.bn = rbind(storia.residuo6250.bn, 0)
           storia.date6180.bn = rbind(storia.date6180.bn, as.character(duedate6180_bn))
           storia.date6250.bn = rbind(storia.date6250.bn, as.character(duedate6250_bn))
           scadenziario_bn = rbind(scadenziario_bn, c(duedate_format, codice_ord, macchina, formato, titolo, editore, num_copie, round(num_click_format,0), 0))
         }
         
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
     if(nrow(scadenziario_bn) == 0){
       ordinare = data.frame(as.numeric(1))
       storia.residuo6180.bn = data.frame(residuo6180)
       storia.residuo6250.bn = data.frame(residuo6250)
       storia.date6180.bn = data.frame(as.character(duedate6180_bn))
       storia.date6250.bn = data.frame(as.character(duedate6250_bn))
       scadenziario_bn = data.frame(duedate_format, codice_ord, macchina, formato, titolo, editore, num_copie, round(num_click,0), round(residuo6250,2))
     } else {
       ordinare = rbind(ordinare, as.numeric(nrow(ordinare)+1))
       storia.residuo6180.bn = rbind(storia.residuo6180.bn, residuo6180)
       storia.residuo6250.bn = rbind(storia.residuo6250.bn, residuo6250)
       storia.date6180.bn = rbind(storia.date6180.bn, as.character(duedate6180_bn))
       storia.date6250.bn = rbind(storia.date6250.bn, as.character(duedate6250_bn))
       scadenziario_bn = rbind(scadenziario_bn, c(duedate_format, codice_ord, macchina, formato, titolo, editore, num_copie, round(num_click,0), round(residuo6250,2)))
     }
   } #if soglia
   colnames(scadenziario_bn) = c("Data", "Ordine", "Macchina", "Formato", "Titolo", "Editore", "Copie", "Click", "Tempo residuo")
   colnames(storia.residuo6180.bn) = "Residuo6180"
   colnames(storia.residuo6250.bn) = "Residuo6250"
   colnames(storia.date6180.bn) = "Data6180"
   colnames(storia.date6250.bn) = "Data6250"
   colnames(ordinare) = "#"
   
   # aggiunta tutte quelle immagazzinate come cambio disp --> linea 119
   for(z in 1:nrow(scad_bn_aux)){
     codice_ord = as.character(scad_bn_aux$Ordine[z])
     titolo = as.character(scad_bn_aux$Titolo[z])
     editore = as.character(scad_bn_aux$Editore[z])
     num_copie = as.numeric(scad_bn_aux$Copie[z])
     num_click = as.numeric(scad_bn_aux$Click[z])
     formato = as.character(scad_bn_aux$Formato[z])
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
   } #for
  scadenziario_bn = cbind(ordinare, scadenziario_bn)
  colnames(scadenziario_bn) = c("#","Data", "Ordine", "Macchina", "Formato", "Titolo", "Editore", "Copie", "Click", "Tempo residuo")
  colnames(storia.residuo6180.bn) = "Residuo6180"
  colnames(storia.residuo6250.bn) = "Residuo6250"
  colnames(storia.date6180.bn) = "Data6180"
  colnames(storia.date6250.bn) = "Data6250"
  colnames(ordinare) = "#"
  save(h.max, residuo6180, residuo6250, duedate6180_bn, duedate6250_bn, X, scadenziario_bn, storia.residuo6180.bn, storia.residuo6250.bn, storia.date6180.bn, storia.date6250.bn, valore.soglia, ordinare, file = "save_bn.RData")
  showNotification("Hai inserito una commessa urgente!", type = "message")  
 } else {
   showNotification("Attenzione! Non hai selezionato la riga dove vuoi inserire l'urgenza o mancano uno o più valori della commessa urgente!", type = "error")
   return()
 }
}
