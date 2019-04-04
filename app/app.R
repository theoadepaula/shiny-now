#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require("shiny")
require("shinydashboard")
library("tidyverse")
require("knitr")
require("zip")
require("rmarkdown")
require("scales")
require("purrr")

rm_accent <- function(str,pattern="all") {
  # Rotinas e funções úteis V 1.0
  # rm.accent - REMOVE ACENTOS DE PALAVRAS
  # Função que tira todos os acentos e pontuações de um vetor de strings.
  # Parâmetros:
  # str - vetor de strings que terão seus acentos retirados.
  # patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
  #            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
  #            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
  #            Outras palavras aceitas: "all" (retira todos os acentos, que são "´", "`", "^", "~", "¨", "ç")
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  
  return(str)
}

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Gerador de Relatórios"),
  dashboardSidebar(
      sidebarMenu(
        menuItem("Planilhas",tabName="planilhas"),
        menuItem("Matérias",tabName="materias"),
        menuItem("Relatórios",tabName="relatorios")
      )

  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      tabItem(tabName ="planilhas",
              h2("Planilhas"),
              fluidRow(
                
                box(width = 4,
                    fileInput("resp", "Escolha a planilha de respostas",
                              multiple = FALSE,
                              accept = c(".xls",".xlsx",".csv"),
                              buttonLabel="Escolha",
                              placeholder="Escolha o Arquivo de respostas.")
                ),
                box(width = 4,
                    fileInput("gab", "Escolha a planilha de gabarito da Matéria",
                              multiple = FALSE,
                              accept = c(".xls",".xlsx",".csv"),
                              buttonLabel="Escolha",
                              placeholder="Escolha o Arquivo de gabarito.")
                ),
                box(width = 4,
                    actionButton("carregar","Carregar Planilhas"))
                
              ),
              fluidRow(
                box(width = 12,
                    span(textOutput("ncol"), style="font-size: 30px"),
                    span(textOutput("lerxlsx"), style="color:red; font-size: 22px")
                )
              )
              ),
      tabItem(tabName ="materias",
              h2("Matérias"),
              fluidRow(
                
                box(width = 4,
                    numericInput("port01", "Onde começa a questão de Língua Portuguesa:",1)
                ),
                box(width = 4,
                    numericInput("port02", "Onde termina a questão de Língua Portuguesa:",10)
                )),
              fluidRow(
                
                box(width = 4,
                    numericInput("redo01", "Onde começa a questão de Redação Oficial:",11)
                ),
                box(width = 4,
                    numericInput("redo02", "Onde termina a questão de Redação Oficial:",13)
                )),
              fluidRow(
                
                box(width = 4,
                    numericInput("lodf01", "Onde começa a questão de Lei Orgânica do DF:",14)
                ),
                box(width = 4,
                    numericInput("lodf02", "Onde termina a questão de Lei Orgânica do DF:",15)
                )),
              fluidRow(
                
                box(width = 4,
                    numericInput("eca01", "Onde começa a questão de Estatuto da Criança e do Adolescente:",16)
                ),
                box(width = 4,
                    numericInput("eca02", "Onde termina a questão de Estatuto da Criança e do Adolescente:",18)
                )),
              fluidRow(
                
                box(width = 4,
                    numericInput("ride01", "Onde começa a questão de Ride:",19)
                ),
                box(width = 4,
                    numericInput("ride02", "Onde termina a questão de Ride:",20)
                )),
              fluidRow(
                
                box(width = 4,
                    numericInput("lc84001", "Onde começa a questão de LC 840:",21)
                ),
                box(width = 4,
                    numericInput("lc84002", "Onde termina a questão de LC 840:",25)
                )),
              fluidRow(
                
                box(width = 4,
                    numericInput("diradm01", "Onde começa a questão de Direito Administrativo:",26)
                ),
                box(width = 4,
                    numericInput("diradm02", "Onde termina a questão de Direito Administrativo:",30)
                )),
              fluidRow(
                
                box(width = 4,
                    numericInput("cp01", "Onde começa a questão de Conhecimentos Pedagógicos:",31)
                ),
                box(width = 4,
                    numericInput("cp02", "Onde termina a questão de Conhecimentos Pedagógico:",100)
                ))
              ,
              fluidRow(
                
                box(actionButton("confirmar_materias","Confirmar matérias"))
                )
              ),
      tabItem(tabName ="relatorios",
              fluidRow(
                box(width = 12, uiOutput("nome_pessoas"))
              ),
              fluidRow(
                box(width = 3,
                    actionButton("confirmar_pessoas","Confirmar Pessoas")),
                box(width = 3,
                    uiOutput("download_button"))
              ),
              fluidRow(
                box(width = 3,
                    textOutput("print_nome_pessoas"))
              )
        )
      )
    )

)

server <- function(input, output) {
  
  output$ncol <- renderText({
    req(input$resp)
    texto= eventReactive(input$carregar,{
      isolate({
        infile=input$resp
        respostas= isolate(readxl:: read_xlsx(infile$datapath))
        print(paste0("Essa planilha de respostas tem ",ncol(respostas)-3," questões!"))
      })
    })
    texto()
  })
  
  confirmar <- eventReactive(input$carregar,{
    req(input$resp,input$gab)
    if(is.null(input$resp) & is.null(input$gab)){
      print("As planilhas de respostas e de gabarito não estão inseridas.")
    } else if(is.null(input$resp)){
      print("A planilha de respostas não está inserida.")
    } else if(is.null(input$gab)){
      print("A planilha de gabarito não está inserida.")
    } else {
      print("Tudo está ok!")
    }
  })
  
  output$lerxlsx <- renderText({
    confirmar()
  })
  
  show_materias= eventReactive(input$confirmar_materias,{
    valores=c(input$port01,input$port02,input$redo01,input$redo02,input$lodf01,input$lodf02,
              input$eca01,input$eca02,input$ride01,input$ride02,input$lc8401,input$lc8402,
              input$diradm01,input$diradm02,input$cp01,input$cp02)
    questoes <- c(1:max(valores))
    materias_nome=c(rep("LÍNGUA PORTUGUESA",-input$port01+input$port02+1),
                    rep("REDAÇÃO OFICIAL",-input$redo01+input$redo02+1),
                    rep("LEI ORGÂNICA DO DF",-input$lodf01+input$lodf02+1),
                    rep("ECA",-input$eca01+input$eca02+1),
                    rep("ATUALIDADES RIDE E DF",-input$ride01+input$ride02+1),
                    rep("LEI COMPLEMENTAR 840/11",-input$lc84001+input$lc84002+1),
                    rep("DIREITO ADMINISTRATIVO",-input$diradm01+input$diradm02+1),
                    rep("CONHECIMENTOS PEDAGÓGICOS",-input$cp01+input$cp02+1))
    materias= cbind(questoes,materias_nome)
    colnames(materias)=c("Questão","Area")
    data.frame(materias) %>% mutate(`Questão`=as.numeric(`Questão`))
  })


  gabarito=eventReactive(input$carregar,{
    infile=input$gab
    gabarito_bruto= readxl:: read_xlsx(infile$datapath)
    gabarito=gabarito_bruto %>% mutate(`Questão`=as.numeric(`Questão`))
    gabarito
  })

  resposta=eventReactive(input$carregar,{
    infile=input$resp
    resposta_bruto= readxl:: read_xlsx(infile$datapath)
    resposta_long=resposta_bruto %>% select(3:ncol(resposta_bruto)) %>% gather(`Questão`,Resposta,-Nome) %>% mutate(`Questão`=parse_number(`Questão`))%>%
      mutate(Resposta=if_else(Resposta=="Certo","C","E"))
    resposta_long
  })

  resposta_merge= reactive({
    req(resposta(),show_materias())
    resp=resposta()
    mat=show_materias()
    resposta_area= resp %>% left_join(mat)
    resposta_area
  })

  conferencia= reactive({
    req(resposta_merge(),gabarito())
    conferencia= resposta_merge() %>% left_join(gabarito()) %>% mutate(acerto=if_else(Resposta==Gabarito,1,0),
                                                                       erro=if_else(Resposta!=Gabarito,1,0))
    conferencia
  })
  ## Até aqui ok
  
  ## Função abaixo com problemas - Problema no Nome:Classificação, que foi alterado para Classificacao.
  ranking= reactive({
    req(conferencia())
    ranking = conferencia() %>% group_by(Nome,`Area`) %>% 
      summarise(Acertos=sum(acerto),Erros=sum(erro),
                Porcentagem=scales::percent(Acertos/n(),accuracy = 0.05,decimal.mark = ",")) %>%
      arrange(`Area`,desc(Porcentagem)) %>% ungroup() %>% group_split(`Area`) %>%
      map(~.x%>%mutate(cont=row_number(),
                       Classificacao=paste0(dense_rank(desc(Porcentagem)),"º/",max(cont)),
                       Minimo=min(Acertos),Media=formatC(mean(Acertos),decimal.mark = ','),
                       Maximo=max(Acertos), `Desvio Padrão`=formatC(round(sd(Acertos),2),
                                                                    decimal.mark = ',')))%>%
      map_df(~bind_rows(.x)) %>%
      select(Nome,`Area`,Acertos,Erros,Porcentagem,Classificacao,Minimo,Media,Maximo,`Desvio Padrão`)
    
    total = ranking %>% group_by(Nome) %>% summarise(Acertos=sum(Acertos),Erros=sum(Erros),
                                                     Porcentagem= scales::percent(Acertos/(Acertos+Erros),accuracy = 0.05,decimal.mark = ",")) %>%
      arrange(desc(Porcentagem)) %>% mutate(Classificacao=paste0(dense_rank(desc(Porcentagem)),"º/",max(row_number())),
                                            Minimo=min(Acertos),Media=formatC(mean(Acertos),decimal.mark = ','),Maximo=max(Acertos), `Desvio Padrão`=formatC(round(sd(Acertos),2),decimal.mark = ','),
                                            `Area`="Total") %>%
      select(Nome,`Area`,Acertos,Erros,Porcentagem,Classificacao,Minimo,Media,Maximo,`Desvio Padrão`) %>%ungroup()


    ranking_total=ranking %>% bind_rows(total)
    ranking_total %>% arrange(Nome)
  })

  pessoas_nome= reactive({
    req(ranking())
    unique(ranking()$Nome)
  })

  output$show_materia= renderTable({ranking()})

  output$nome_pessoas= renderUI({
    checkboxGroupInput("pessoas","Selecione pessoas para gerar o relatório:",
                       choices = pessoas_nome(),
                       selected = pessoas_nome()
    )
  })


  output$download_button= renderUI({
    downloadButton("baixar_relatorio","Baixar relatórios")
  })

  print_pessoas= eventReactive(input$confirmar_pessoas,{
    input$pessoas
  })

  output$print_nome_pessoas = renderPrint({
    print_pessoas()
  })

  output$baixar_relatorio <- downloadHandler(
    filename =    function() {
      paste("zip-", Sys.Date(), ".zip", sep="")
    }
    ,
    contentType = "application/zip",
    content = function(file) {


      # create list of dataframes and NULL value to store fileNames
      listDataFrames <- print_pessoas()
      allFileNames <- NULL

      # loop through each dataframe
      for(i in print_pessoas()) {

        params <- list(ranking = ranking()%>% filter(Nome==i),
                       gabarito = conferencia()%>% filter(Nome==i))
        render(input="modelo_relatorio_01.Rmd",
               output_file=paste0(rm_accent(i), ".docx"),
               params = params,
               envir = new.env(parent = globalenv()))
        fileName <- paste0(rm_accent(i), ".docx")
        allFileNames <- c(allFileNames,fileName)
      }
      zipr(zipfile=file, files=allFileNames)

    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
