###################
## UI COMPONENTS ##
###################

venn_ui <- function(id) {
    ns <- NS(id)
    tabPanel("Data streams",
             fluidRow(column(width = 4),
                      column(
                          width = 8,
                          box(
                              width = 12,
                              solidHeader = TRUE,
                              title = "Venn-diagram for data streams",
                              addSpinner(
                                  plotOutput(ns("venn_plot"), height = 500),
                                  spin = "bounce",
                                  color = cols[1]
                              )
                          )
                      )))
}

#######################
## SERVER COMPONENTS ##
#######################

venn_server <-
    function(input,
             output,
             session,
             rawdata) {
        ns <- session$ns
        
        output$venn_plot <- renderPlot({
            venn_plot(rawdata)
        })

    }

venn_plot <- function(rawdata) {
    if (!rawdata$auth) {
        return(NULL)
    }
    chronpi = unique(rawdata$chronicle$processed$child_id)
    tudpi = unique(rawdata$tud$processed$child_id)
    maqpi = unique(rawdata$maq$processed$child_id)
    
    nC = length(chronpi)
    nT = length(tudpi)
    nM = length(maqpi)
    
    nCT = length(intersect(chronpi, tudpi))
    nCM = length(intersect(chronpi, maqpi))
    nTM = length(intersect(maqpi, tudpi))
    
    nCTM = length(intersect(intersect(chronpi, tudpi), maqpi))
    
    grid.newpage()
    draw.triple.venn(
        area1 = nC,
        area2 = nT,
        area3 = nM,
        n12 = nCT,
        n13 = nCM,
        n23 = nTM,
        n123 = nCTM,
        
        category = c("Chronicle", "TUD", "MAQ"),
        lty = 1,
        lwd = 2,
        fill = c(cols[1], cols[2], cols[4]),
        fontfamily = "sans",
        cex = 1.5,
        fontface = "bold",
        cat.fontfamily = "sans",
        cat.fontface = "bold",
        cat.col = c(cols[1], cols[2], cols[4]),
        scaled = TRUE,
        euler.D = TRUE,
        cat.dist = 0.03,
        cat.pos = c(-10, 5, 5),
        label.col = "white",
        alpha = 0.8
    )
    
}




