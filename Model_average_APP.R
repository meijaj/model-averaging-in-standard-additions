#######################################################
# Model averaging for method of standard additions 
# Author: Juris Meija and Enea Pagliano, NRC Canada
# Date: 2022-09-26
#######################################################

# Brief description
# This calculator performs ordinary least squares fitting of the measurement models 
# and calculates the model average based on Bayesian information criterion (BIC)

require(shiny)
require(shinyjs)
require(rhandsontable)
require(DT)
#require(LaplacesDemon) # Mixture Gaussian

truevalue = NULL
utruevalue = NULL
amount.x = NULL
signal.y = NULL
usignal.y = NULL

EX = list()
EX[[1]] = list()
EX[[2]] = list(
  truevalue = 50.5,
  utruevalue = 0.2/2,
  amount.x = c (0, 0, 0, 50.46, 50.46, 50.46, 100.20, 100.20,100.20, 145.27, 145.27, 145.27, 199.19, 199.19, 199.19),
  signal.y = c(1.313, 1.308, 1.319, 2.645, 2.659, 2.647, 3.872, 3.969, 3.911, 5.003, 5.013, 5.045, 6.352, 6.342, 6.394)
)
EX[[2]]$usignal.y = EX[[2]]$signal.y*0.004317251

EX[[3]] = list(
  truevalue = 1,
  utruevalue = 0,
  amount.x = c(0, 1, 2, 3, 4),
  signal.y = c(1, 2, 3, 4, 5)
)
EX[[3]]$usignal.y = EX[[3]]$signal.y/100
                       
EX[[4]] = list(
  truevalue = 1,
  utruevalue = 0,
  amount.x = c(0, 1, 2, 3, 4),
  signal.y = c(1, 2, 3, 4, 5)
)
EX[[4]]$usignal.y = rep(min(EX[[4]]$signal.y)/100,5)

EX[[5]] = list(
  truevalue = 150,
  utruevalue = 0,
  amount.x = c(0, 150, 300, 450, 600)
)
EX[[5]]$signal.y = 0.111*(EX[[5]]$amount.x+150) + 275*log10(2/(1 + exp(7.27E-4 * (EX[[5]]$amount.x+150))))
EX[[5]]$usignal.y = EX[[5]]$signal.y/100

EX[[6]] = list(
  truevalue = 150,
  utruevalue = 0,
  amount.x = c(0, 150, 300, 450, 600)
)
EX[[6]]$signal.y = 0.111*(EX[[6]]$amount.x+150) + 275*log10(2/(1 + exp(7.27E-4 * (EX[[6]]$amount.x+150))))
EX[[6]]$usignal.y = rep(min(EX[[6]]$signal.y)/100,5)
  
EX[[7]] = list(
  truevalue = 22.53,
  utruevalue = 0.43/2,
  amount.x = c(0, 22.792, 45.534, 67.536, 88.830),
  signal.y = c(0.2168, 0.4393, 0.6576, 0.8742, 1.1020),
  usignal.y = c(0.0030, 0.0038, 0.0006, 0.0070, 0.0050)
)

EX[[8]] = list(
  truevalue = 22.53,
  utruevalue = 0.43/2,
  amount.x = c(0, 22.898, 45.402, 67.013, 89.631),
  signal.y = c(0.2231, 0.4373, 0.6648, 0.8927, 1.1215),
  usignal.y = c(0.0033, 0.0005, 0.0113, 0.0041, 0.0027)
)

EX[[9]] = list(
  truevalue = 22.53,
  utruevalue = 0.43/2,
  amount.x = c(0, 23.055, 45.566, 67.646, 90.832),
  signal.y = c(0.2139, 0.4369, 0.6518, 0.8809, 1.1052),
  usignal.y = c(0.0008, 0.0039, 0.0038, 0.0066, 0.0034)
)

EX[[10]] = list(
  truevalue = 22.53,
  utruevalue = 0.43/2,
  amount.x = c(0, 23.691, 46.900, 69.989, 94.643),
  signal.y = c(0.2125, 0.4388, 0.6542, 0.8825, 1.1149),
  usignal.y = c(0.0017, 0.0007, 0.0033, 0.0094, 0.0098)
)

EX[[11]] = list(
  truevalue = 22.53,
  utruevalue = 0.43/2,
  amount.x = c(0, 22.902, 45.788, 68.532, 91.618),
  signal.y = c(0.2178, 0.4366, 0.6585, 0.8803, 1.1168),
  usignal.y = c(0.0005, 0.0015, 0.0023, 0.0017, 0.0088)
)

EX[[12]] = list(
  truevalue = 0.3657,
  utruevalue = 0.3657*0.2/100,
  amount.x = c(0, 0.120042270519229, 0.238102420689831, 0.347275027582308, 0.480867338841154),
  signal.y = c(0.392255, 0.509564, 0.625789, 0.729955, 0.859708)
)
EX[[12]]$usignal.y = EX[[12]]$signal.y*0.5/100

EX[[13]] = list(
  truevalue = 96.28, # ID4MS not the certified one
  utruevalue = 0.42,
  amount.x = c(0, 48.0800634170792, 95.5260001168512, 142.979896477749, 190.256546306178),
  signal.y = c(1.78975048923679, 2.65751289123396, 3.56056403143782, 4.43019575665221, 5.30638101819857)
)
EX[[13]]$usignal.y = EX[[13]]$signal.y*0.75/100

EX[[14]] = list(
  truevalue = 0.1940,
  utruevalue = 0.1940*0.2/100,
  amount.x = c(0, 9.87051562701267E-02, 0.194793208651105, 0.289294669216881, 0.376682079703856, 0.483616286682886, 0.582576032730027, 0.678247525321147, 0.748733788135177),
  signal.y = c(0.195536, 0.303489, 0.403876, 0.499759, 0.585619, 0.679964, 0.767436, 0.849295, 0.906815)
)
EX[[14]]$usignal.y = EX[[14]]$signal.y*0.5/100

### END EXAMPLES ###

### BEGIN TEST DATA ###
init.df = data.frame (amount=EX[[2]]$amount.x, 
                      signal=EX[[2]]$signal.y, 
                      u_signal=EX[[2]]$usignal.y)

# true value and its std. uncertainty
TV = c(EX[[2]]$truevalue, EX[[2]]$utruevalue) 

### END TEST DATA ###

### BEGIN DECLARATION OF GLOBAL VARIABLES ### Verified EP 2022-08-25

# Name of the models MUST reflect the order of models in models.mle
modelnames = c(
  'LINEAR, x = a + b*y', 
  'RATIONAL, x = (a + b*y)/(1 + c*y)',
  'QUADRATIC, x = a + b*y + c*y^2',
  'REDUCED CUBIC, x = a + b*y + c*y^3'
)

### END DECLARATION OF GLOBAL VARIABLES ### Verified EP 2022-08-25

### BEGIN SERVER FILE FUNCTION ###

server <- function(input, output, session) {
  
  ### BEGIN HOT DATA INPUT TABLE ###
  output$hot <- renderRHandsontable({
    if (is.null(input$hot)) { DF = init.df } else { DF = hot_to_r(input$hot) }
    rhandsontable(DF, readOnly = FALSE, stretchH = "all", selectCallback = TRUE) %>%
      hot_context_menu(allowColEdit = FALSE ) %>%
      hot_validate_numeric(cols = 1:3, min = 0) %>%
      hot_cols('float', format = '0.0000')
  })
  
  ### END HOT DATA INPUT TABLE ###
  
  observeEvent(input$ex, {
    if(input$ex!='1') {
      ex = EX[[as.double(input$ex)]]
      
      output$hot <- renderRHandsontable({
        DF = data.frame(cbind(ex$amount.x, ex$signal.y, ex$usignal.y))
        names(DF) = c('amount','signal','u_signal')
        rhandsontable(DF, readOnly = FALSE, stretchH = "all", selectCallback = TRUE) %>%
          hot_context_menu(allowColEdit = FALSE ) %>%
          hot_validate_numeric(cols = 1:3, min = 0) %>%
          hot_cols('float', format = '0.0000')
      })
      
      TV <- c(ex$truevalue, ex$utruevalue) 
      updateNumericInput(session, 'truevalue', value = TV[1])
      updateNumericInput(session, 'truevalueunc', value = TV[2])
    }
  })
  
  user.data  <- reactive({
    if (is.null(input$hot)) { z = init.df } else { z = hot_to_r(input$hot) }
    list(amount=z[,1], signal=z[,2], u_signal=z[,3])
  })
  
  ### BEGIN INSTRUCTION AFTER CLICKING BUTTON "Perform least squares fitting" ###  
  observeEvent(input$button, {
    
    # Number of iterations for the Monte Carlo
    it.MC = input$nMC
    
    ### BEGIN NO MODELS SELECTED ###
    if(length(input$models) < 1){
      output$text2A  <- renderText({ paste("<font color=\"#FF0000\">", "NO MEASUREMENT MODELS SELECTED", "</font>") })
    }
    ### END NO MODELS SELECTED ###
    
    ### BEGIN AT LEAST ONE MODEL SELECTED ###
    if(length(input$models) > 0){
      
      ### model.id contains the active models
      models.id = modelnames[as.double(input$models)]
      
      ### BEGIN initialization of LIST for Monte Carlo output values ### Verified EP 2022-08-25
      result_list = list()
      for (i in 1:length(models.id)) {
        t_list = list(list("result" = c(), 
                           "SD" = c(), 
                           "BIC" = c(),
                           "weight" = c(),
                           "result_BS" = c()
        ))
        names(t_list) = models.id[i]
        result_list = append(result_list, t_list)
      }
      
      t_list = list(list("result" = c(), 
                         "SD" = c(),
                         "result_BS" = c()
      ))
      
      names(t_list) <- "average"
      result_list = append(result_list, t_list)
      ### END initialization of LIST for Monte Carlo output values ### Verified EP 2022-08-25
      
      
      ### BEGIN Monte Carlo simulation of Model Average ###
      withProgress(message = 'Calculations in progress', value = 0, min=0, max=1, {
        for (k in 1:it.MC) {
          
        # Measurement models
        r.x = user.data()$amount #JURIS: check the user.data() syntax #OK
        r.y = rnorm(length(user.data()$signal), 
                    user.data()$signal, 
                    user.data()$u_signal)
        
        # Coordinate Swapping in Standard Addition Graphs for Analytical Chemistry: A Simplified Path for Uncertainty Calculation in Linear and Nonlinear Plots
        # Anal. Chem. 2014, 86, 17, 8563-8567
        
        # 1 # x = a + b*y (x = a when y = 0)
        m1.mle = lm(r.x ~ r.y)
        
        # 2 # x = (a + b*y)/(1 + c*y) (x = a when y = 0)
        m2.mle = lm(r.x ~ r.y + I(-r.x * r.y))
        
        # 3 # x = a + b*y + c*y^2 (x = a when y = 0)
        m3.mle = lm(r.x ~ r.y + I(r.y^2)) 
        
        # 4 # x = a + b*y + c*y^3 (x = a when y = 0)
        m4.mle = lm(r.x ~ r.y + I(r.y^3))
        
        models.mle = list(m1.mle, m2.mle,  m3.mle, m4.mle)
        models.selected = models.mle[as.double(input$models)]
        
        
        ### BEGIN MODEL AVERAGE CORE CALCULATION ###
        
        # model weights (BIC) 
        models.bic = sapply(models.selected, BIC) # Vector of length = all measurement models
        models.dbic = exp(-0.5*(models.bic-min(models.bic))) # Vector of length = all measurement models
        models.w = models.dbic/sum(models.dbic) # Vector of length = all measurement models
        
        # result and standard deviation of all models 
        mu.all = -sapply(sapply(models.selected, coef),'[[',1) # Vector of length = all measurement models
        sd.all = sqrt(sapply(sapply(models.selected, vcov),'[[',1)) # Vector of length = all measurement models
        
        # result and standard deviation of models average
        mu.average = sum(models.w * mu.all)
        sd.average = sqrt(sum(models.w * sd.all)^2) # Possolo & Meija (2022, 2nd ed) p. 144
        
        # result and standard deviation of models + models average
        models.a  = c(mu.all, mu.average)
        models.ua = c(sd.all, sd.average)
        
        # Parametric bootstrap on the result
        models.a.BS = rnorm(length(models.a), models.a, models.ua)
        
        ### END MODEL AVERAGE CORE CALCULATION ###
        
        ### BEGIN PRINTING THE MONTE CARLO RESULTS INTO THE LIST result_list ### Verify with JURIS
        
        for (i in 1:length(models.id)) {
          result_list[[i]]$result[k] = mu.all[i]
          result_list[[i]]$SD[k] = sd.all[i]
          result_list[[i]]$BIC[k] = models.bic[i]
          result_list[[i]]$weight[k] = models.w[i]
          result_list[[i]]$result_BS[k] = models.a.BS[i]
        }
        
        result_list[[length(models.id)+1]]$result[k] = mu.average
        result_list[[length(models.id)+1]]$SD[k] = sd.average
        result_list[[length(models.id)+1]]$result_BS[k] = models.a.BS[length(models.id)+1]
        
        # Increment the progress bar
        incProgress(1/it.MC, detail = paste(round(100*k/it.MC), "%"))
        
        ### END PRINTING THE MONTE CARLO RESULTS INTO THE LIST result_list ###
        
      }
      })
      ### END Monte Carlo simulation of Model Average ###
      
      ### BEGIN SAVING THE result_list
      # save the result_list as txt
      #cat(capture.output(print(result_list), file=paste("EX", EX, "List_MC_TMP.txt", sep = "_")))
      
      # save the result_list as R file
      #saveRDS(result_list, file = paste("EX", EX, "List_MC_TMP.rds", sep = "_"))
      
      ### END SAVING THE result_list ### Verified EP 2022-08-25
      
      ### BEGIN CALCULATE MONTE CARLO RESULTS AND SD ### Verified EP 2022-08-25
      models.bic.AW = c()
      models.weight.AW = c()
      models.bic.SD = c()
      models.weight.SD = c()
      
      for (i in 1:length(models.id)) {
        models.bic.AW[i] = mean(result_list[[i]]$BIC)
        models.weight.AW[i] = mean(result_list[[i]]$weight)
        models.bic.SD[i] = sd(result_list[[i]]$BIC)
        models.weight.SD[i] = sd(result_list[[i]]$weight)
      }
      
      models.a.MC = c()
      models.ua.MC = c()
      
      for (i in 1:(length(models.id)+1)) {
        models.a.MC[i] = mean(result_list[[i]]$result_BS)
        models.ua.MC[i] = sd(result_list[[i]]$result_BS) 
      }
      ### END CALCULATE MONTE CARLO RESULTS AND SD ### Verified EP 2022-08-25
      
      ### BEGIN TABLE RESULTS SHOW AND SAVE ### Verified EP 2022-08-25
      
      results.table = cbind('Nr'=c(1:length(mu.all),'-'),
                            'model'=c(models.id,'average'),
                            'BIC'=c(formatC(models.bic.AW, digits=2, format='f'),'-'),
                            'u_BIC'=c(formatC(models.bic.SD, digits=2, format='f'),'-'),
                            'weight'=c(formatC(models.weight.AW, digits=3, format='f'),'-'),
                            'u_weight'=c(formatC(models.weight.SD, digits=3, format='f'),'-'),
                            'result'=formatC(models.a.MC, digits=3, format='f'), 
                            'u_result'=formatC(models.ua.MC, digits=3, format='f'))
      
      output$res.table <- renderTable({ results.table }, sanitize.text.function = function(x) x)
      
      # CSV export of Model average output data
      #write.csv(results.table, paste("EX", EX, "Table_summary_TMP.csv", sep = "_"), row.names = TRUE)
      
      ### END TABLE RESULTS SHOW AND SAVE ### Verified EP 2022-08-25
      
      ### BEGIN SAVE LIST FOR OFFLINE GGPLOT ### Verified EP 2022-08-25
      Plot_MA_TMP = list("model_ID" = models.id,
                         "result" = models.a.MC, 
                         "SD" = models.ua.MC,
                         "TV" = input$truevalue,
                         "uTV" = input$truevalueunc)
      #saveRDS(Plot_MA_TMP, file = paste("EX", EX,"Plot_MA_TMP.rds", sep = "_"))
      ### END SAVE LIST FOR OFFLINE GGPLOT ### Verified EP 2022-08-25
      
      ### BEGIN Standard additions results plot (model comparison) ###
      
      ### BEGIN ### Verified EP 2022-08-25
      mu.all = head(models.a.MC, length(models.a.MC)-1)
      sd.all = head(models.ua.MC, length(models.ua.MC)-1)
      mu.average = models.a.MC[length(models.a.MC)]
      sd.average = models.ua.MC[length(models.ua.MC)]

      ### END ### Verified EP 2022-08-25
      
      output$plot_right <- renderPlot({ 
        
        # RESULTS PLOT
        par(las=1,mar=c(4.5, 4.1, 1.5, 3.1))
        plot(y=1:length(mu.all), x=mu.all, xlim=c(min(mu.all - 2*sd.all), max(mu.all + 2*sd.all)), 
             ylim=c(0, 0.5+length(mu.all)), pch=19, cex=1.3, yaxt='n', xlab='Result',ylab='',cex.axis=1.3, cex.lab=1.3)
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "gray95")
        mtext('95% confidence',side=3,line=0.2,adj=0.99,cex=0.9)
        
        if(is.numeric(input$truevalue) & input$truevalue!=0 & is.numeric(input$truevalueunc) & input$truevalueunc!=0) {
          rect(input$truevalue - 2*input$truevalueunc, -1, input$truevalue + 2*input$truevalueunc, length(modelnames)+1, col='gray65',border='gray65')
          abline(v=input$truevalue,lty=1, lwd=2, col='white') 
        }
        
        segments(mu.all - 2*sd.all, 1:length(mu.all), mu.all + 2*sd.all, 1:length(mu.all), lwd=3, col='steelblue3')
        points(y=1:length(mu.all), x=mu.all, pch=21, col='black', bg='white', cex=2)
        text(x=mu.all,y=1:length(mu.all), labels = models.id, col='black', pos=3, offset=1, font=1, cex=1.3)
        
        segments(mu.average - 2*sd.average, 0, mu.average + 2*sd.average, 0, lwd=3, col='tomato')
        points(y=0, x=mu.average, pch=21, col='black', bg='white',cex=2)
        text(x=mu.average,y=0, labels = 'average', col='black', pos=3, offset=1, font=2, cex=1.3)
        
        box()
        #for(i in 2:6) mtext(at=c(i,-5),adj=1,line=1.5,side=2,text=id[i])
        
      })
      
      ### END Standard additions results plot (model comparison) ###
      
      ### BEGIN Standard additions plot ###
      
      output$plot_left <- renderPlot({ 
        xy = user.data()
        max.x = +max(xy$signal)
        min.x = 0
        min.y = -max(mu.all)
        max.y = +max(xy$amount)
        par(mar=c(4.5, 4.1, 1.5, 3.1))
        plot(y=xy$signal, x=xy$amount, main="", ylim=c(min.x, max.x), xlim=c(min.y, max.y), 
             ylab="Analytical signal", xlab="Added amount", cex.axis=1.3, cex.lab=1.3)
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "gray95")
        abline(v=0)
        # average model and its 95 % confidence bound
        #polygon(c(x.p, rev(x.p)), c(y.p.lwr,rev(y.p.upr)), col='gray50', border='gray50')
        #lines(x=x.p, y=y.p.fit, lwd=3, col='tomato')
        abline(c(-coef(m1.mle)[1], 1)/coef(m1.mle)[2], lty=2, col='grey10')
        points(y=xy$signal, x=xy$amount, pch=21, cex=2.5, col='black', bg='white')
        mtext('95% confidence',side=3,line=0.2,adj=0.99,cex=0.9)
        box()
      })
      
      ### END Standard additions plot ###
      
    } 
    ### END AT LEAST ONE MODELS SELECTED ###
    
  }) 
  ### END INSTRUCTION AFTER CLICKING BUTTON "Perform least squares fitting" ###
  
  shinyjs::onclick("toggleextra", shinyjs::toggle(id = "filterextra", anim = TRUE))
  shinyjs::onclick("togglenotes", shinyjs::toggle(id = "filternotes", anim = TRUE))
  shinyjs::onclick("toggleextra.mol", shinyjs::toggle(id = "filterextra", anim = TRUE))
  shinyjs::onclick("togglenotes.mol", shinyjs::toggle(id = "filternotes", anim = TRUE))
  
}

### END SERVER FILE FUNCTION ###

ex_choices = c("none"=1,
  "1. Nitrate by IDMS (GCMS) in standard solution"=2,
  "2. in silico linear data (heteroscedastic)"=3,
  "3. in silico linear data (homoscedastic)"=4,
  "4. in silico nonlinear data (heteroscedastic)"=5,
  "5. in silico nonlinear data (homoscedastic)"=6,
  "6. Nitrate by ion chromatography/conductivity in spinach CRM (1)"=7,
  "7. Nitrate by ion chromatography/conductivity in spinach CRM (2)"=8,
  "8. Nitrate by ion chromatography/conductivity in spinach CRM (3)"=9,
  "9. Nitrate by ion chromatography/conductivity in spinach CRM (4)"=10,
  "10. Nitrate by ion chromatography/conductivity in spinach CRM (5)"=11,
  "11. Nitrite by photometry in seawater"=12,
  "12. Bromide by IDMS (GCMS) in groundwater CRM (BCR 611)"=13,
  "13. Phosphate by photometry in seawater"=14
  )

### BEGIN user interface

ui <- fluidPage(
  
  titlePanel( title="NRC Standard addition model averaging calculator" ),
  shinyjs::useShinyjs(),
  sidebarLayout(
    
    sidebarPanel(
      fluidRow(
        column(10,
               checkboxGroupInput("models", label = "Select measurement models", 
                                  choiceNames = modelnames,
                                  choiceValues = 1:length(modelnames), 
                                  selected = c(1:length(modelnames))),
        ), width='85%'),
      
      h5(tags$b("Enter (paste) the measurement results")),
      rHandsontableOutput("hot"),
      helpText("right-click to add or delete rows"),
      column(12, selectInput("ex", label = "(optional) Load example data", choices = ex_choices, selected = 1, multiple=FALSE)),
      h5("Additional settings", a(id = "toggleextra", "show/hide")),
      shinyjs::hidden(div(id = "filterextra",
                          fluidRow(
                            column(12, h5(tags$b("(optional) True value of the measurand"))),
                            column(6, numericInput("truevalue", label = "Mean", min=0, value=TV[1], width='85%') ),
                            column(6, numericInput("truevalueunc", label = "Std. uncertainty", min=0, value=TV[2], width='85%') ),
                            column(12, numericInput("nMC", label = "Monte Carlo simulations", min=1000, value=5000, width='85%') )
                          )
      )),
      br(),
      conditionalPanel(condition = "!$('html').hasClass('shiny-busy')",   
                       actionButton("button", label = "Perform least squares fitting", icon = icon('bar-chart-o'))),
      conditionalPanel(condition = "$('html').hasClass('shiny-busy')",   
                       actionButton("button", label = "busy...", icon = icon('hourglass-half')))      
    ),
    
    mainPanel(
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      fluidRow(column(helpText("This calculator performs model averaging for the method of standard additions."), width=11)),
      br(),
      conditionalPanel(condition = "!$('html').hasClass('shiny-busy')",   
                       fluidRow( column(8, tableOutput("res.table")) )                 
                       ),
      br(),
      conditionalPanel(condition = "!$('html').hasClass('shiny-busy')",   
                       fluidRow( column(6, plotOutput("plot_left")), 
                                 column(6, plotOutput("plot_right"))
                       )                 
      ),
      conditionalPanel(condition = "!$('html').hasClass('shiny-busy')",   
                       strong(htmlOutput("text1b"))
      ),
      br(),
      h5("Notes and explanations", a(id = "togglenotes", "show/hide")),
      shinyjs::hidden(div(id = "filternotes",
                          fluidRow(column(
                            p("This calculator performs ordinary least squares fitting of the measurement models and calculates the model average based on Bayesian information criterion (BIC). All uncertainties are expressed as standard uncertainties except when noted otherwise."),
                            p("Created using R and Shiny. Juris Meija (2022) NRC Canada"),width=11)
                          ))),
      br(),
      p("NRC Model averaging standard additions calculator (2022) v.1")
      
    )
  )
)
### END user interface

shinyApp(ui = ui, server = server)
