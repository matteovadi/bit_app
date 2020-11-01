setwd("/Users/matteovadi/Desktop/bookintime/BIT")
rm(list = ls())
library(shiny)
library(markdown)
library(shinythemes)
library(bootstrap)
library(fresh)
library(shinyWidgets)
library(rintrojs)
library(DT)
library(shinyjs)
library(shinyalert)
source("scad_fun_new2.R")
source("view_bn_new.R")
source("elim_fun_bn_new.R")
source("urgent_fun_new.R")
source("reload_fun_bn_new.R")
source("anticipo_fun.R")
options(shiny.port = 3111)

# Define UI ----
ui <- fluidPage(
  useShinyjs(),
  tags$style("#testo_urgente {font-family: Arial; font-size: 110%; color: #656565;}"),   
  tags$style("#testo_anticipo {font-family: Arial; font-size: 110%; color: #656565;}"),       
  useShinyalert(),
  use_theme(create_theme(
    theme = "united",
    bs_vars_button(
      default_bg = "#E95346",
      default_border = "#CC0000"
    ),
    bs_vars_navbar(
      default_bg = "#E95346", 
      default_link_active_bg  = "#CC0000"
    ), 
    bs_vars_input(
      border_focus = "#CC0000"
    ),
    bs_vars_nav(
      link_hover_bg = "#E95346"
    ),
    bs_vars_pills(
      active_link_hover_bg = "#E95346"
    )
  )
  ),

  navbarPage(title = p("BIT @ Digital Book SRL", style = "font-family: Chalkduster; font-size: 80%;"),
             selected = "Scadenzario",
             fluid = TRUE,
             id = "navbar",
             windowTitle = "BIT @ Digital Book SRL",
             tabPanel("Scadenzario",
                      icon = icon("calendar", "fa-1x", lib = "font-awesome"),
                      hr(),
                      DTOutput("table_bn"),
                      hr(),
                      fluidRow(
                        column(3, dateInput("date", label = h5("Hai completato le commesse del:"), value = as.Date(NA), format = "dd-mm-yyyy", language = "it", weekstart = 1)),
                        column(6, br(), br(), actionButton("elimina_bn", "Eliminare commesse alla data indicata" ,icon = icon("check-circle", "fa-1x", lib = "font-awesome"))),
                        br(), br(), actionButton("annulla_elim_bn", "Annulla eliminazione commesse completate", icon = icon("redo-alt", "fa-1x", lib = "font-awesome"))
                      )
             ),
             tabPanel("Nuova commessa",
                      icon = icon("plus-square", "fa-1x", lib = "font-awesome"),
                      sidebarLayout(
                        sidebarPanel(
                          helpText(p("Inserire le specifiche della nuova commessa in bianco/nero da schedulare come indicato.")), 
                          div(id = "all_bn",
                              numericInput("num1_bn", 
                                           h4("Numero di click:"),
                                           value = NULL
                              ),
                              textInput("text2_bn", 
                                        h4("Editore:"),
                                        value = "",
                                        placeholder = "Inserire l'editore..."
                              ),
                              textInput("text3_bn", 
                                        h4("Titolo:"),
                                        value = "",
                                        placeholder = "Inserire il titolo..."
                              ),
                              numericInput("num2_bn", 
                                           h4("Numero di copie:"),
                                           value = NULL
                              ),    
                              textInput("num3_bn", 
                                        h4("Ordine numero:"),
                                        value = "",
                                        placeholder = "Inserire il codice dell'ordine..."
                              ),
                              textInput("text4_bn", 
                                        h4("Formato:"),
                                        value = "",
                                        placeholder = "Inserire il formato..."
                              )
                          ),
                          actionButton("action_button_bn", "Aggiungi la nuova commessa")
                        ),
                        mainPanel(
                          fluidRow(column(6, h4("Specifiche nuova commessa:"))),
                          fluidRow(
                            column(3,textOutput("selected_num1_bn")),
                            column(3,textOutput("selected_num2_bn")),
                            column(6,textOutput("selected_text2_bn"))
                          ),
                          fluidRow(
                            column(3,textOutput("selected_num3_bn")),
                            column(3,textOutput("selected_text4_bn")),
                            column(6,textOutput("selected_text3_bn"))
                          ),
                          hr(),
                          DTOutput("table1"),
                          hr(),
                          actionButton("annulla_nuovacommessa_bn", "Annulla ultima commessa inserita", icon = icon("redo-alt", lib = "font-awesome"))
                        )
                      )
             ),
             tabPanel("Gestione urgenze",
                      icon = icon("clock", "fa-1x", lib = "font-awesome"),
                      verbatimTextOutput("testo_urgente"),
                      sidebarLayout( position = "right",
                                     sidebarPanel(
                                       helpText(p("Inserire le specifiche della commessa urgente in bianco/nero da schedulare come indicato.")), 
                                       div(id = "all_bn_urg",
                                           numericInput("num1_bn_urg", 
                                                        h4("Numero di click:"),
                                                        value = NULL
                                           ),
                                           textInput("text2_bn_urg", 
                                                     h4("Editore:"),
                                                     value = "",
                                                     placeholder = "Inserire l'editore..."
                                           ),
                                           textInput("text3_bn_urg", 
                                                     h4("Titolo:"),
                                                     value = "",
                                                     placeholder = "Inserire il titolo..."
                                           ),
                                           numericInput("num2_bn_urg", 
                                                        h4("Numero di copie:"),
                                                        value = NULL
                                           ),    
                                           textInput("num3_bn_urg", 
                                                     h4("Ordine numero:"),
                                                     value = "",
                                                     placeholder = "Inserire il codice dell'ordine..."
                                           ),
                                           textInput("text4_bn_urg", 
                                                     h4("Formato:"),
                                                     value = "",
                                                     placeholder = "Inserire il formato..."
                                           )
                                       ),
                                       actionButton("action_button_bn_urg", "Aggiungi la nuova commessa urgente")
                                     ), 
                                     mainPanel(
                                       fluidRow(column(6, h4("Specifiche nuova commessa:"))),
                                       fluidRow(
                                         column(3,textOutput("selected_num1_bn_urg")),
                                         column(3,textOutput("selected_num2_bn_urg")),
                                         column(6,textOutput("selected_text2_bn_urg"))
                                       ),
                                       fluidRow(
                                         column(3,textOutput("selected_num3_bn_urg")),
                                         column(3,textOutput("selected_text4_bn_urg")),
                                         column(6,textOutput("selected_text3_bn_urg"))
                                       ),
                                       hr(),
                                       DTOutput("table1_urg"),
                                       hr(),
                                       actionButton("annulla_urg_bn", "Annulla ultima commessa urgente inserita", icon = icon("redo-alt", lib = "font-awesome"))
                                     )
                      )
             ),
             tabPanel("Completamento in anticipo",
                      icon = icon("check-circle", "fa-1x", lib = "font-awesome"),
                      verbatimTextOutput("testo_anticipo"),
                      hr(),
                      DTOutput("table_anticipo"),
                      hr(),
                      fluidRow(
                        column(9, actionButton("anticipo", "Elimina commessa selezionata completata in anticipo", icon = icon("check-circle", lib = "font-awesome"))),
                        actionButton("annulla_anticipo", "Annulla eliminazione commessa in anticipo", icon = icon("redo-alt", lib = "font-awesome"))
                      )
             )
             )
  )
  
# Define server logic ----
server <- function(input, output) {
  
  output$selected_num1_bn <- renderText({ 
    paste("Numero di click:", input$num1_bn)
  })  
  output$selected_num1_bn_urg <- renderText({ 
    paste("Numero di click:", input$num1_bn_urg)
  })  
  output$selected_text2_bn <- renderText({ 
    paste("Editore:", input$text2_bn)
  })  
  output$selected_text2_bn_urg <- renderText({ 
    paste("Editore:", input$text2_bn_urg)
  })  
  output$selected_text3_bn <- renderText({ 
    paste("Titolo:", input$text3_bn)
  })
  output$selected_text3_bn_urg <- renderText({ 
    paste("Titolo:", input$text3_bn_urg)
  })
  output$selected_num2_bn <- renderText({ 
    paste("Numero di copie:", input$num2_bn)
  }) 
  output$selected_num2_bn_urg <- renderText({ 
    paste("Numero di copie:", input$num2_bn_urg)
  }) 
  output$selected_num3_bn <- renderText({ 
    paste("Ordine numero:", input$num3_bn)
  }) 
  output$selected_num3_bn_urg <- renderText({ 
    paste("Ordine numero:", input$num3_bn_urg)
  }) 
  output$selected_text4_bn <- renderText({ 
    paste("Formato:", input$text4_bn)
  })
  output$selected_text4_bn_urg <- renderText({ 
    paste("Formato:", input$text4_bn_urg)
  })
  
  
  
  observeEvent(input$action_button_bn, {
    scad_fun_new2(input$num1_bn,input$text2_bn,input$text3_bn,input$num2_bn,input$num3_bn, input$text4_bn)
    reset("all_bn")
  })
  observeEvent(input$elimina_bn, {
    elim_fun_bn_new(input$date)
  })
  observeEvent(input$action_button_bn_urg,{
    urgent_fun_new(input$num1_bn_urg, input$text2_bn_urg, input$text3_bn_urg, input$num2_bn_urg, input$num3_bn_urg, input$text4_bn_urg ,input$table1_urg_rows_selected)
    reset("all_bn_urg")
  })
  observeEvent(input$anticipo,{
    anticipo_fun(input$table_anticipo_rows_selected)
  })
  
  df_bn <- eventReactive({input$action_button_bn | input$elimina_bn | input$action_button_bn_urg | input$anticipo},{
    datatable(view_bn_new(), selection = 'single', rownames = FALSE, escape = TRUE, options = list(pageLength = 25, columnDefs = list(list(className = 'dt-center', targets = c(1,2,3,4,5,6,7,8,9))), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#CC0000', 'color': '#fff'});",
      "}")))
  })

  v <- reactiveValues(bn = 1)
  observeEvent(input$navbar,{
    v$bn = 1
  })
  observeEvent({input$action_button_bn | input$elimina_bn | input$action_button_bn_urg | input$anticipo},{
    v$bn = v$bn + 1
  })
  output$table1 <- renderDT({
    if(v$bn > 1){
      df_bn()
    } else if (v$bn == 0){
      datatable(view_bn_new(), selection = 'single', rownames = FALSE, escape = TRUE, options = list( pageLength = 25, columnDefs = list(list(className = 'dt-center', targets = c(1,2,3,4,5,6,7,8,9))), initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#CC0000', 'color': '#fff'});",
        "}")))
    } else if (v$bn == 1){
      datatable(view_bn_new(), selection = 'single', rownames = FALSE, escape = TRUE, options = list( pageLength = 25, columnDefs = list(list(className = 'dt-center', targets = c(1,2,3,4,5,6,7,8,9))), initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#CC0000', 'color': '#fff'});",
        "}")))
    }
  }, server = TRUE)
  output$table_bn <- renderDT({
    if(v$bn > 1){
      df_bn()
    } else if (v$bn == 0){
      datatable(view_bn_new(), selection = 'single', rownames = FALSE, escape = TRUE, options = list( pageLength = 25, columnDefs = list(list(className = 'dt-center', targets = c(1,2,3,4,5,6,7,8,9))), initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#CC0000', 'color': '#fff'});",
        "}")))
    } else if (v$bn == 1){
      datatable(view_bn_new(), selection = 'single', rownames = FALSE, escape = TRUE, options = list( pageLength = 25, columnDefs = list(list(className = 'dt-center', targets = c(1,2,3,4,5,6,7,8,9))), initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#CC0000', 'color': '#fff'});",
        "}")))
    }
  }, server = TRUE)
  output$table1_urg <- renderDT({
    if(v$bn > 1){
      df_bn()
    } else if (v$bn == 0){
      datatable(view_bn_new(), selection = 'single', rownames = FALSE, escape = TRUE, options = list( pageLength = 25, columnDefs = list(list(className = 'dt-center', targets = c(1,2,3,4,5,6,7,8,9))), initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#CC0000', 'color': '#fff'});",
        "}")))
    } else if (v$bn == 1){
      datatable(view_bn_new(), selection = 'single', rownames = FALSE, escape = TRUE, options = list( pageLength = 25, columnDefs = list(list(className = 'dt-center', targets = c(1,2,3,4,5,6,7,8,9))), initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#CC0000', 'color': '#fff'});",
        "}")))
    }
  }, server = TRUE)
  # da 638 in poi 
  output$testo_urgente <- renderPrint({
    cat("Qui puoi inserire le commesse urgenti. Per prima cosa seleziona la riga della tabella in corrispondenza della quale vuoi inserire la commessa. Poi, inserisci le specifiche della commessa urgente e\nclicca 'Aggiungi la nuova commessa urgente'. In questo modo la commessa viene aggiunta dove desiderato e le successive ne terranno conto in termini di potenzialità giornaliera dei macchinari. NB: la commessa nella riga selezionata scorrerà assieme a tutte le successive. NB2: non puoi inserire una commessa urgente nella prima riga dello scadenzario!") 
  })
  observeEvent(input$annulla_urg_bn,{
    shinyalert(title = "Annulla inserimento urgenza", text = "Sei sicuro di voler annulare l'inserimento dell'ultima commessa urgente in bianco/nero inserita?", type = "warning", closeOnEsc = TRUE, inputId = "alert_bn", confirmButtonText = "Vai!", showCancelButton = TRUE, cancelButtonText = "Indietro", confirmButtonCol = "#E95346")
  })
  observeEvent(input$alert_bn,{
    if(input$alert_bn == TRUE){
      reload_fun_bn_new()
      v$bn = 0
      showNotification("Hai annullato l'ultima commessa urgente in bianco/nero inserita", type = "message")
    } else {
      showNotification("Non hai annullato l'ultima commessa urgente in bianco/nero inserita", type = "warning")
    }
  })
  observeEvent(input$annulla_elim_bn,{
    shinyalert(title = "Annulla eliminazione commessa", text = "Sei sicuro di voler annulare l'eliminazione della commessa completata in bianco/nero selezionata in precedenza?", type = "warning", closeOnEsc = TRUE, inputId = "alert_elim_bn", confirmButtonText = "Vai!", showCancelButton = TRUE, cancelButtonText = "Indietro", confirmButtonCol = "#E95346")
  })
  observeEvent(input$alert_elim_bn,{
    if(input$alert_elim_bn == TRUE){
      reload_fun_bn_new()
      v$bn = 0
      showNotification("Hai annullato l'eliminazione della commessa completata in bianco/nero", type = "message")
    } else {
      showNotification("Non hai annullato l'eliminazione della commessa completata in bianco/nero", type = "warning")
    }
  })
  observeEvent(input$annulla_nuovacommessa_bn,{
    shinyalert(title = "Annulla inserimento commessa", text = "Sei sicuro di voler annulare l'inserimento dell'ultima commessa (ordinaria) in bianco/nero inserita?", type = "warning", closeOnEsc = TRUE, inputId = "alert_nuova_bn", confirmButtonText = "Vai!", showCancelButton = TRUE, cancelButtonText = "Indietro", confirmButtonCol = "#E95346")
  })
  observeEvent(input$alert_nuova_bn,{
    if(input$alert_nuova_bn == TRUE){
      reload_fun_bn_new()
      v$bn = 0
      showNotification("Hai annullato l'ultima commessa (ordinaria) in bianco/nero inserita", type = "message")
    } else {
      showNotification("Non hai annullato l'ultima commessa (ordinaria) in bianco/nero inserita", type = "warning")
    }
  })
  
  output$testo_anticipo <- renderPrint({
    cat("In questa sezione, a differenza di ciò che accade in 'Scadenziario', la singola commessa che decidi di eliminare influirà sulle successive. Qui infatti puoi eliminare le commesse che intendi completare in anticipo e le successive ne terranno conto in termini di disponibilità giornaliera dei macchinari (click ricalcolati in questo caso).") 
  })
  observeEvent(input$annulla_anticipo,{
    shinyalert(title = "Annulla eliminazione anticipo selezionato", text = "Sei sicuro di voler annulare l'eliminazione della commessa completata in anticipo?", type = "warning", closeOnEsc = TRUE, inputId = "alert_antici", confirmButtonText = "Vai!", showCancelButton = TRUE, cancelButtonText = "Indietro", confirmButtonCol = "#E95346")
  })
  observeEvent(input$alert_antici,{
    if(input$alert_antici == TRUE){
      reload_fun_bn_new()
      v$bn = 0
      showNotification("Hai annullato l'eliminazione della commessa completata in anticipo", type = "message")
    } else {
      showNotification("Non Hai annullato l'eliminazione della commessa completata in anticipo", type = "warning")
    }
  })
  output$table_anticipo <- renderDT({
    if(v$bn > 1){
      df_bn()
    } else if (v$bn == 0){
      datatable(view_bn_new(), selection = 'single', rownames = FALSE, escape = TRUE, options = list( pageLength = 25, columnDefs = list(list(className = 'dt-center', targets = c(1,2,3,4,5,6,7,8,9))), initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#CC0000', 'color': '#fff'});",
        "}")))
    } else if (v$bn == 1){
      datatable(view_bn_new(), selection = 'single', rownames = FALSE, escape = TRUE, options = list( pageLength = 25, columnDefs = list(list(className = 'dt-center', targets = c(1,2,3,4,5,6,7,8,9))), initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#CC0000', 'color': '#fff'});",
        "}")))
    }
  }, server = TRUE)
}

# Run the app ----
shinyApp(ui = ui, server = server)
  
