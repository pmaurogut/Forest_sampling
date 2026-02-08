# initial values

source("utils.R")
L <- 100
N <- 5
K<-1000
init_pop <- make_population(N,L)
init_samp_points <- sampling_points(K,L)
all_trees <- get_all_trees(init_pop,init_samp_points)
est <- n_estimaciones(all_trees,L,rotate=FALSE)
par_int <- parametros_interes(init_pop,L,TRUE)
rm(all_trees)
rm(init_samp_points)
server <- function(input, output, session) {
  # thematic::thematic_shiny()
  ##### reactive values #####
  
  forest <- reactiveVal(init_pop)
  est<-reactiveVal(est)
  pos <- reactiveVal(c(1))

  
  table1 <- reactive({
    input$muestra
    positions <- pos()
    ests <- est()
    ests<-ests[ests$Type==input$plot_type1,]
    
    k <- length(positions)
    reps <- k/input$n
    print(input$n)
    print(k)
    print(reps)
    
    indexes <- match(positions,ests$Parc)
    tabla <- ests[indexes,]
    tabla<-tabla[seq(1,k,by=input$n),]
    tabla$Parc <- reps:1
    print(positions)
    print(tabla)
    tabla
  })
  par_int <- reactive({
    parametros_interes(forest(),input$lado,TRUE)
  })
  
  base_plot <- reactive({
    pop_plot(forest(),input$lado)
  })
  
  update_pop <- reactive({
    input$reset_pop
    pop <- make_population(input$N,input$lado)
    samp_points <- sampling_points(K,input$lado)

    trees <- get_all_trees(pop,samp_points)
    estimates <- n_estimaciones(trees,input$lado,rotate=FALSE)
    forest(pop)
    est(estimates)
    pos(1:input$n)
    
  })
  
  observeEvent(input$reset_pop,update_pop())
  observeEvent(input$N,update_pop())
  observeEvent(input$lado,update_pop())
  observeEvent(input$n,{
    pos(sample(1:K,input$n,replace = TRUE))
  })
  observeEvent(input$muestra,{
    pos(c(sample(1:K,input$n,replace = TRUE),pos()))
  })
  
  
  ##### Population #####
  
  output$poblacion <- renderTable({
    forest()[,c(1:4)]
  })
  
  output$plot_poblacion<-renderPlot({
    p <- base_plot()
    if(input$add_hd){
      p <- p +
        geom_label(aes(x=x,y=y-3,label=paste("d: ",diam)),size=4,fill="darkgreen",alpha=0.3)+
        geom_label(aes(x=x,y=y-8,label=paste("h: ",diam)),size=4,fill="blue",alpha=0.3)
    }
    p + ggtitle("Población")
  })
  
  output$tabla_interes1 <- renderTable({
    par_int()
  })
  
  ##### Seleccion #####
  output$plot_fijo <- renderPlot({
    trees <- get_trees(forest(),est()[pos()[1],],"r_fijo")
    plot_selection(base_plot(),trees,
                   all=input$all_trees,add_hd=input$add_hd)
  })

  output$plot_variable <- renderPlot({
    trees <- get_trees(forest(),est()[pos()[1],],"r_variable")
    plot_selection(base_plot(),trees,all=input$all_trees,add_hd=input$add_hd)
  })

  output$plot_relascopio <- renderPlot({
    trees <- get_trees(forest(),est()[pos()[1],],"r_relascopio")
    plot_selection(base_plot(),trees,all=input$all_trees,add_hd=input$add_hd)
  })


  output$plot_fijo2 <- renderPlot({
    trees <- get_trees(forest(),est()[pos()[1],],"r_fijo")
    plot_selection(base_plot(),trees,tree_center = FALSE,add_hd=input$add_hd)
  })

  output$plot_variable2 <- renderPlot({
    trees <- get_trees(forest(),est()[pos()[1],],"r_variable")
    plot_selection(base_plot(),trees,tree_center = FALSE,add_hd=input$add_hd)
  })

  output$plot_relascopio2 <- renderPlot({
    trees <- get_trees(forest(),est()[pos()[1],],"r_relascopio")
    plot_selection(base_plot(),trees,tree_center = FALSE,add_hd=input$add_hd)
  })

  # ##### One plot #####
  output$tabla_interes2<-renderTable({
    par_int()
  })
  output$muestra <- renderTable({
    samp<-get_trees(forest(),table1()[1,],input$plot_type1)
    samp
  })

  output$plot_selected1 <- renderPlot({
    trees <- get_trees(forest(),table1()[1,],input$plot_type1)
    plot_selection(base_plot(),trees,
                   tree_center = input$centered=="arbol",add_hd=input$add_hd)
  })

  output$estimacion1<-renderTable({
    tabla <- table1()
    tabla[1,]
  })

  output$tabla_acc <- renderTable({table1()})

  output$plot_res1 <- renderPlot({
    tabla <-table1()
    add_samples_plot(par_int(),tabla)
  })



  # ##### n plots #####

  output$tabla_interes3<-renderTable({
    par_int()
  })
  
  output$plot_selected2<-renderPlot({
      selected <- get_n_points(forest(),samp_points(),input$plot_type1)
      plot_n_selections(base_plot(),selected,samp_points(),
                        type=input$plot_type1,
                        tree_center = input$centered=="arbol",
                        all=input$all_trees)
  })
  
  output$n_estimaciones<-renderTable({
    selected <- est()
    selected[selected$Rep==max(selected$Rep),]
  })
  
  output$plot_res2<- renderPlot({
    add_samples_n_plots(par_int(),est())
  })
  
  # ##### samp dist #####
  
  output$tabla_interes4<-renderTable({
    par_int()
  })
  
  output$plot_selected3<-renderPlot({base_plot()})
  
  output$n_estimaciones<-renderTable({
    selected <- est()
    selected[selected$Rep==max(selected$Rep),]
  })
  
  output$plot_res2<- renderPlot({
    add_samples_n_plots(par_int(),est())
  })
  
  
}


