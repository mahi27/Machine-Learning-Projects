library(shiny)
library(xtable)
library(dplyr)
library(plotly)
ui <- fluidPage(
      pageWithSidebar(
      headerPanel(
       h1("EMI Calculator")),
    sidebarPanel(
      numericInput("Principal", "Loan Amount (in INR)", value="70000",min="0",max="1000000000",step="10000"),
      numericInput("Interest", "Rate of interest p.a(in %)", value="18.5",min="3",max="20",step="0.25"),
      numericInput("Tenure", "Time Period(in months)",min="1", max="360",value="36",step="1"),
      actionButton("go", "Calculate EMI")),
    mainPanel(
      tabsetPanel(
        tabPanel(
        "About EMI Calculator",
        h4("This application calculates EMI payable on the Principal borrowed"),
        p("There are three tabs in this application: About EMI Calculator,EMI Summary and EMI Schedule BreakUp"),
        p("The user needs to enter the desired numbers and click on Calculate EMI button to see the calculated EMI"),
        p("EMI Summary tab gives the details of EMI to be paid,Interest Payable and Total Amount payable"),
        p("EMI Schedule Break Up tab shows the detailed monthly breakup of oustanding principal and interest"),
        
        br(),
        
        h4("The formula to calculate EMI is:"),
        p("A = [P*r*(1+r)^n]/[(1+r)^n - 1]"),
        em(p("where A is the EMI payable, P is the principal borrowed, r is the rate of interest for the tenure, n is the tenure or duration in months")),
        
        em(p("For more information,refer the following wikipedia link about EMI calculation:")),
        h5(div(a("https://en.wikipedia.org/wiki/Equated_Monthly_Installment",href="https://en.wikipedia.org/wiki/Equated_Monthly_Installment"),style="color:blue")),
        
        br(),
        strong(h4("Following are the inputs to be given in sidepanel:")),
        strong("1. Loan Amount in INR:"),
        p("This is the amount borrowed in INR. This should be a numeric with value greater than zero."),
        
        br(),
        
        strong("2. Yearly Interest Rate as %:"),
        p("This is the annual rate of interest at which the loan is borrowed in %. It should be a positive numeric value."),
        
        br(),
        
        strong("3. Time period in months:"),
        p("This is the duration for which loan is borrowed in number of months. It should be a positivie numeric value."),
        
        br(),
        
        p("Thank you!"),
        
        br()
        
        ),
        tabPanel(
        "EMI Summary",
        fluidRow(
          column(6,
        (h4("Loan Amount in INR:",style = "color:steelblue")),
        strong(textOutput("Principal")),
        h4("Yearly Interest Rate as %:",style = "color:steelblue"),
        strong(textOutput("Interest")),
        h4("Time period in months :",style = "color:steelblue"),
        strong(textOutput("Tenure")),
        br(),
        h4("Equated Monthly Installment in INR:",style = "color:steelblue"),
        strong(h4(div(textOutput("EMI"))))),
          column(6,
        h4("Total Interest Payable in INR:",style = "color:steelblue"),
        strong(textOutput("Interest_payable")),
        h4("Total Amount Payable in INR:",style = "color:steelblue"),
        strong(textOutput("Amount_payable"))
        )),
        
        fluidRow(
        #pie chart
        plotOutput("pie"),
        #bar chart
        plotOutput("bar")
      )),
        tabPanel(
        "EMI Schedule Break Up",
        tableOutput("EMIbreakup")
      )
    )
  )
)
)


  

EMI <- function(Principal,Interest,Tenure) {
  temp1 = (1+Interest/1200)**Tenure
  EMI = Principal*Interest/1200*temp1/(temp1-1)
}

EMIbreakup <- function(Principal,Interest,Tenure,EMI){
  Balance = Principal
  Month = 0
  Interest_paid = 0
  Principal_paid = 0
  EMI_paid = 0
  Outstanding = 0
  
  Month = as.integer(Month)
  Interest_paid = as.numeric(Interest_paid)
  Principal_paid = as.numeric(Principal_paid)
  Outstanding = as.numeric(Outstanding)
  
  for(i in 1:Tenure){
    Month[i]=i
    Interest_paid[i] = Balance*Interest/1200
    Principal_paid[i] = EMI - Interest_paid[i]
    EMI_paid[i] = EMI
    Balance = Balance-Principal_paid[i] 
    Outstanding[i]=Balance
  }
  
  EMIbreakup = data.frame(Month,Principal_paid,Interest_paid,EMI_paid,Outstanding)
  colnames(EMIbreakup) = c("Month","Principal","Interest","EMI","Outstanding Balance")
  EMIbreakup = xtable(EMIbreakup)
}

server <- function(input, output){
    
    output$Principal <- renderText({input$go
                                    validate(
              need(input$Principal > 0, "Illogical values.Please enter a value greater than zero"))
                                    isolate(input$Principal)
                                   })
    
    output$Interest <- renderText({input$go
                                   validate(
        need(input$Interest > 0, "Illogical values.Please enter a value greater than zero"))
                                   isolate(input$Interest)
                                   })
    
    output$Tenure <- renderText({input$go
                                 validate(
        need(input$Tenure > 0, "Illogical values.Please enter a value greater than zero"))
                                 isolate(input$Tenure)
                                 })
    output$Interest_payable <- renderText({input$go
                              isolate(round(input$Tenure*EMI(input$Principal,input$Interest,input$Tenure)-input$Principal))})
    
    output$Amount_payable <- renderText({input$go
                            isolate(round(input$Tenure*EMI(input$Principal,input$Interest,input$Tenure))
                             )})
    
    output$EMI <- renderText({input$go
                            isolate(round(EMI(input$Principal,input$Interest,input$Tenure)))
                            })
    
    output$EMIbreakup <- renderTable({
      input$go
      EMICalc = as.numeric(EMI(input$Principal,input$Interest,input$Tenure))
      isolate(EMIbreakup(input$Principal,input$Interest,input$Tenure,EMICalc))
    },include.rownames=FALSE)
    
    vect <- reactive({c(input$Principal,(EMI(input$Principal,input$Interest,input$Tenure)*input$Tenure) - input$Principal)})
    lbls <- c("Principal Amount - ","Total Interest Payable - ")
    
    output$pie <- renderPlot({input$go
                  isolate(pie(vect(),labels = paste(lbls,round(vect()/sum(vect())*100),"%",sep=""),
                              main = "Total Amount Payable",col = c("orange","blue")))})
    lev <- reactive({t(as.matrix(EMIbreakup(input$Principal,input$Interest,
                                input$Tenure,as.numeric(EMI(input$Principal,input$Interest,input$Tenure)))))[2:3,]})
    
    output$bar <- renderPlot({input$go
                  isolate(barplot(lev(), main="EMI Breakup per month",
                                  col=c("darkblue","red"),
                                  legend = rownames(lev()),names.arg = 1:input$Tenure
                                  ))})
}
    
shinyApp(ui = ui, server = server)