
library(shiny)


odds <- function(odds,team1 = 'team 1',team2 = 'team 2'){
    prob = odds
    ret = rep('',8)
    
    ret[1] = (paste('3-0 ',team1,': ',round(dnbinom(0,3,prob),3) * 100, '%'))
    ret[2] = (paste('3-1 ',team1,': ',round(dnbinom(1,3,prob),3) * 100, '%'))
    ret[3] = (paste('3-2 ',team1,': ',round(dnbinom(2,3,prob),3) * 100, '%'))
    ret[4] = (paste('3-2 ',team2,': ',round(dnbinom(2,3,1-prob),3) * 100, '%'))
    ret[5] = (paste('3-1 ',team2,': ',round(dnbinom(1,3,1-prob),3) * 100, '%'))
    ret[6] = (paste('3-0 ',team2,': ',round(dnbinom(0,3,1-prob),3) * 100, '%'))
    ret[7] = (paste(team1,' wins: ',round(sum(dnbinom(0:2,3,prob)),3) * 100, '%'))
    ret[8] = (paste(team2,' wins: ',round(sum(dnbinom(0:2,3,1-prob)),3) * 100, '%'))
    
    ret1 = ''
    for(i in 1:8){
        ret1 = paste(ret1,ret[i],'\n')
    }
    return(ret1)
}

ui <- fluidPage(

    titlePanel("Odds of series win"),

    sidebarLayout(
        sidebarPanel(
            textInput("team1", "Team 1:", ""),
            textInput("team2", "Team 2:", ""),
            
            sliderInput("Odds",
                        "Odds of Team 1: winning:",
                        min = 0,
                        max = 1,
                        value = 0.05)
        ),

        mainPanel(
            verbatimTextOutput("odds")
        )
    )
)

server <- function(input, output) {

    output$odds <- reactive({odds(input$Odds,input$team1,input$team2)
    })
}

shinyApp(ui = ui, server = server)
