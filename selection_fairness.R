library(shiny)
library(dplyr)
library(ggplot2)
library(faux)
library(ggpubr)
library(magrittr)

set.seed(666)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # App title
  titlePanel("Regressions"),
  
  # Sidebar layout with input and output definitions 
  sidebarLayout(
  
  # Sidebar panel for inputs
      sidebarPanel(

            # Means of variables A, B and C in Group A
            h2("Group A"),
            # Predictor 1
            sliderInput("n.a",
                        "N",
                        min = 10,
                        max = 1000,
                        value = 500,
                        step = 10),
            
            sliderInput("mu.a.1",
                        "M(Pred1)",
                        min = -3,
                        max = 3,
                        value = 0,
                        step = .1),

            sliderInput("sd.a.1",
                        "SD(Pred1)",
                        min = 0,
                        max = 3,
                        value = 1,
                        step = .1),
            hr(),
            # Predictor 2
            sliderInput("mu.a.2",
                        "M(Pred2)",
                        min = -3,
                        max = 3,
                        value = 0,
                        step = .1),
             sliderInput("sd.a.2",
                        "SD(Pred2)",
                        min = 0,
                        max = 3,
                        value = 1,
                        step = .1),
            hr(),
            # Criterion
            sliderInput("mu.a.3",
                        "M(Crit)",
                        min = -3,
                        max = 3,
                        value = 0,
                        step = .1),
            sliderInput("sd.a.3",
                        "SD(Crit)",
                        min = 0,
                        max = 3,
                        value = 1,
                        step = .1),            
            hr(),
            # Correlations
            sliderInput("r.a.12",
                        "cor(Pred1, Pred2)",
                        min = -1,
                        max = 1,
                        value = 0,
                        step = .1),
            sliderInput("r.a.13",
                        "cor(Pred1, Crit)",
                        min = -1,
                        max = 1,
                        value = 0,
                        step = .1),
            sliderInput("r.a.23",
                        "cor(Pred2, Crit)",
                        min = -1,
                        max = 1,
                        value = 0,
                        step = .1),           
            br(),
            br(),
            h2("Group B"),
            # Predictor 1
            sliderInput("n.b",
                        "N",
                        min = 10,
                        max = 1000,
                        value = 500,
                        step = 10),
            
            sliderInput("mu.b.1",
                        "M(Pred1)",
                        min = -3,
                        max = 3,
                        value = 0,
                        step = .1),

            sliderInput("sd.b.1",
                        "SD(Pred1)",
                        min = 0,
                        max = 3,
                        value = 1,
                        step = .1),
            hr(),
            # Predictor 2
            sliderInput("mu.b.2",
                        "M(Pred2)",
                        min = -3,
                        max = 3,
                        value = 0,
                        step = .1),
             sliderInput("sd.b.2",
                        "SD(Pred2)",
                        min = 0,
                        max = 3,
                        value = 1,
                        step = .1),
            hr(),
            # Criterion
            sliderInput("mu.b.3",
                        "M(Crit)",
                        min = -3,
                        max = 3,
                        value = 0,
                        step = .1),
            sliderInput("sd.b.3",
                        "SD(Crit)",
                        min = 0,
                        max = 3,
                        value = 1,
                        step = .1),            
            hr(),
            # Correlations
            sliderInput("r.b.12",
                        "cor(Pred1, Pred2)",
                        min = -1,
                        max = 1,
                        value = 0,
                        step = .1),
            sliderInput("r.b.13",
                        "cor(Pred1, Crit)",
                        min = -1,
                        max = 1,
                        value = 0,
                        step = .1),
            sliderInput("r.b.23",
                        "cor(Pred2, Crit)",
                        min = -1,
                        max = 1,
                        value = 0,
                        step = .1),                         
            br(),
            br(),
            h3("Cutoff & Baserate"),
            # Cutoff and base rate
            sliderInput("cutoff",
                        "Cutoff",
                        min = -3,
                        max = 3,
                        value = 0,
                        step = .1),
            sliderInput("baserate",
                        "Base rate",
                        min = -3,
                        max = 3,
                        value = 0,
                        step = .1),
            br(),
            br(),
            h3("Download"),
            # Buttons to download plots
            downloadButton('dl_p_crit_p1', 'Download Plot Crit~P1'),
            downloadButton('dl_p_crit_p2', 'Download Plot Crit~P2'),
            downloadButton('dl_p_crit_p1_p2', 'Download Plot Crit~P1+P2'),
            br(),
            br(),
            HTML(paste0("luc.watrin[at]uni-ulm.de", "<br>",
                        "CC-By Attribution 4.0 International", "<br>",
                        "Code available here: https://github.com/luc-w/selection_fairness"))
            ),

  # Main panel to display outputs
  mainPanel(
      
      # Output: plot1, plot2, plot3  
      tabsetPanel(type = "tabs",
                  tabPanel("Crit~P1", 
                           plotOutput("p_crit_p1", width = 700, height = 700), 
                           div(tableOutput("diagnostics_1"), style = "font-size:160%")),
                  tabPanel("Crit~P2", 
                           plotOutput("p_crit_p2", width = 700, height = 700), 
                           div(tableOutput("diagnostics_2"), style = "font-size:160%")),
                  tabPanel("Crit~P1+P2", 
                           plotOutput("p_crit_p1_p2", width = 700, height = 700), 
                           div(tableOutput("diagnostics_12"), style = "font-size:160%")),
                  tabPanel("Debug", 
                           tableOutput("data"))
      )
  )
  )
)

                  
                  
                  
                  
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    data <- reactive({
      
        # Sample sizes
        n.a = input$n.a
        n.b = input$n.b
        
        # Means of variables A, B and C
        mu.a.1 <- input$mu.a.1
        mu.a.2 <- input$mu.a.2
        mu.a.3 <- input$mu.a.3
        mu.b.1 <- input$mu.b.1
        mu.b.2 <- input$mu.b.2
        mu.b.3 <- input$mu.b.3
        
        # SDs of variables A, B and C
        sd.a.1 <- input$sd.a.1
        sd.a.2 <- input$sd.a.2
        sd.a.3 <- input$sd.a.3
        sd.b.1 <- input$sd.b.1
        sd.b.2 <- input$sd.b.2
        sd.b.3 <- input$sd.b.3

        # Correlations of variables A, B and C
        r.a.12 = input$r.a.12
        r.a.13 = input$r.a.13
        r.a.23 = input$r.a.23
        r.b.12 = input$r.b.12
        r.b.13 = input$r.b.13
        r.b.23 = input$r.b.23

        # Cutoff and base rate
        cutoff = input$cutoff
        baserate = input$baserate
        
        
        # simulate data
        rbind(rnorm_multi(n = n.a,
                          mu = c(mu.a.1, mu.a.2,mu.a.3 ),
                          sd = c(sd.a.1, sd.a.2,sd.a.3 ),
                          r = c(r.a.12,r.a.13,r.a.23),
                          varnames = c("Pred1", "Pred2", "Criterion"),
                          empirical = FALSE),
              rnorm_multi(n = n.b,
                          mu = c(mu.b.1, mu.b.2,mu.b.3),
                          sd = c(sd.b.1, sd.b.2,sd.b.3),
                          r = c(r.b.12,r.b.13,r.b.23),
                          varnames = c("Pred1", "Pred2", "Criterion"),
                          empirical = FALSE)) %>%
              mutate(Group.num = as.numeric(c(rep("1", n.a), rep("0", n.b))),
                     Group = c(rep("A", n.a), rep("B", n.b)),
                     ID = c(1:(n.a + n.b)),
                     `Pred1 + Pred2` = scale(predict(lm(Criterion ~ Pred1 + Pred2))),
                     cutoff_cat1 = ifelse(Pred1 >= cutoff, "selected", "not-selected"),
                     cutoff_cat2 = ifelse(Pred2 >= cutoff, "selected", "not-selected"),
                     cutoff_cat12 = ifelse(`Pred1 + Pred2` >= cutoff, "selected", "not-selected"),
                     baserate_cat = ifelse(Criterion >= baserate, "suitable", "not-suitable")
                     )
    })
      
    # Cross-tables 
    # Selected x suited based on Pred1
    crosstab_A1 <- reactive({
      
      data() %>% 
        filter(Group == "A") %$% 
        table(factor(cutoff_cat1, levels = c("selected", "not-selected")), factor(baserate_cat, levels = c("suitable", "not-suitable")))
      
    })
    
    crosstab_B1 <- reactive({
      
    data() %>% 
        filter(Group == "B") %$% 
        table(factor(cutoff_cat1, levels = c("selected", "not-selected")), factor(baserate_cat, levels = c("suitable", "not-suitable")))
      
    })
    
    # Selected x suited based on Pred2  
    crosstab_A2 <- reactive({
      
    data() %>% 
        filter(Group == "A") %$%
        table(factor(cutoff_cat2, levels = c("selected", "not-selected")), factor(baserate_cat, levels = c("suitable", "not-suitable")))
      
    })
    
    crosstab_B2 <- reactive({
      
      data() %>% 
        filter(Group == "B") %$% 
        table(factor(cutoff_cat2, levels = c("selected", "not-selected")), factor(baserate_cat, levels = c("suitable", "not-suitable")))
      
    })
    
    # Selected x suited based on Pred1+Pred2
    crosstab_A12 <- reactive({
      
    data() %>% 
        filter(Group == "A") %$% 
        table(factor(cutoff_cat12, levels = c("selected", "not-selected")), factor(baserate_cat, levels = c("suitable", "not-suitable")))
      
    })
    
    crosstab_B12 <- reactive({
      
      data() %>% 
        filter(Group == "B") %$% 
        table(factor(cutoff_cat12, levels = c("selected", "not-selected")), factor(baserate_cat, levels = c("suitable", "not-suitable")))
      
    })
    
    diagnostics_1 <- reactive({
      
      data.frame(Value = c(# Odds Ratio
                           sum(crosstab_A1()["selected",])/sum(crosstab_B1()["selected",])),
                 row.names = "Odds Ratio", check.names = FALSE)
      })
    
    diagnostics_2 <- reactive({
      
      data.frame(Value = c(# Odds Ratio
                           sum(crosstab_A2()["selected",])/sum(crosstab_B2()["selected",])),
                 row.names = "Odds Ratio", check.names = FALSE)
      })
        
    diagnostics_12 <- reactive({
      
      data.frame(Value = c(# Odds Ratio
                           sum(crosstab_A12()["selected",])/sum(crosstab_B12()["selected",])),
                 row.names = "Odds Ratio", check.names = FALSE)
      })

    # produce plot
      p_crit_p1 <- reactive({
        p <- ggplot(data(), aes(x = Pred1, y = Criterion, color = Group)) +
                geom_point(alpha = .6, size = 3) +
                geom_smooth(method = "lm", size = 2) +
                #stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~")), label.y = c(3.6, 4), label.x = c(-2, -2), size = 6) +
                annotate("text", -3.7, 4, label = paste(round(crosstab_A1()["not-selected", "suitable"]/sum(crosstab_A1())*100, 1), "%"), color = "#e41a1c", size = 8) +
                annotate("text", -3.7, 3.7, label = paste(round(crosstab_B1()["not-selected", "suitable"]/sum(crosstab_B1())*100, 1), "%"), color = "#377eb8", size = 8) +
                annotate("text", -3.7, -3.7, label = paste(round(crosstab_A1()["not-selected", "not-suitable"]/sum(crosstab_A1())*100, 1), "%"), color = "#e41a1c", size = 8) +
                annotate("text", -3.7, -4, label = paste(round(crosstab_B1()["not-selected", "not-suitable"]/sum(crosstab_B1())*100, 1), "%"), color = "#377eb8", size = 8) +
                annotate("text", 3.7, 4, label = paste(round(crosstab_A1()["selected", "suitable"]/sum(crosstab_A1())*100, 1), "%"), color = "#e41a1c", size = 8) +
                annotate("text", 3.7, 3.7, label = paste(round(crosstab_B1()["selected", "suitable"]/sum(crosstab_B1())*100, 1), "%"), color = "#377eb8", size = 8) +
                annotate("text", 3.7, -3.7, label = paste(round(crosstab_A1()["selected", "not-suitable"]/sum(crosstab_A1())*100, 1), "%"), color = "#e41a1c", size = 8) +
                annotate("text", 3.7, -4, label = paste(round(crosstab_B1()["selected", "not-suitable"]/sum(crosstab_B1())*100, 1), "%"), color = "#377eb8", size = 8) +
                #annotate("text", 4, 4, label = paste(OR_AB1), color = "#e41a1c", size = 8) +
                scale_x_continuous(limits = c(-4, 4), breaks = seq(-4, 4, 1)) + 
                scale_y_continuous(limits = c(-4, 4), breaks = seq(-4, 4, 1)) +
                geom_hline(yintercept = input$baserate, linetype = "dashed") +
                geom_vline(xintercept = input$cutoff, linetype = "dashed") +
                coord_fixed() + 
                theme_classic() +
                theme(text = element_text(size = 22),
                      legend.position="bottom") +
                scale_color_manual(values=c("#e41a1c", "#377eb8"))
         })

      # produce plot
      p_crit_p2 <- reactive({
        p <- ggplot(data(), aes(x = Pred2, y = Criterion, color = Group)) +
                geom_point(alpha = .6, size = 3) +
                geom_smooth(method = "lm", size = 2) +
                #stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~")), label.y = c(3.6, 4), size = 6) +
                annotate("text", -3.7, 4, label = paste(round(crosstab_A2()["not-selected", "suitable"]/sum(crosstab_A2())*100, 1), "%"), color = "#e41a1c", size = 8) +
                annotate("text", -3.7, 3.7, label = paste(round(crosstab_B2()["not-selected", "suitable"]/sum(crosstab_B2())*100, 1), "%"), color = "#377eb8", size = 8) +
                annotate("text", -3.7, -3.7, label = paste(round(crosstab_A2()["not-selected", "not-suitable"]/sum(crosstab_A2())*100, 1), "%"), color = "#e41a1c", size = 8) +
                annotate("text", -3.7, -4, label = paste(round(crosstab_B2()["not-selected", "not-suitable"]/sum(crosstab_B2())*100, 1), "%"), color = "#377eb8", size = 8) +
                annotate("text", 3.7, 4, label = paste(round(crosstab_A2()["selected", "suitable"]/sum(crosstab_A2())*100, 1), "%"), color = "#e41a1c", size = 8) +
                annotate("text", 3.7, 3.7, label = paste(round(crosstab_B2()["selected", "suitable"]/sum(crosstab_B2())*100, 1), "%"), color = "#377eb8", size = 8) +
                annotate("text", 3.7, -3.7, label = paste(round(crosstab_A2()["selected", "not-suitable"]/sum(crosstab_A2())*100, 1), "%"), color = "#e41a1c", size = 8) +
                annotate("text", 3.7, -4, label = paste(round(crosstab_B2()["selected", "not-suitable"]/sum(crosstab_B2())*100, 1), "%"), color = "#377eb8", size = 8) +
                scale_x_continuous(limits = c(-4, 4), breaks = seq(-4, 4, 1)) + 
                scale_y_continuous(limits = c(-4, 4), breaks = seq(-4, 4, 1)) +
                geom_hline(yintercept = input$baserate, linetype = "dashed") +
                geom_vline(xintercept = input$cutoff, linetype = "dashed") +
                coord_fixed() + 
                theme_classic() +
                theme(text = element_text(size = 22),
                      legend.position="bottom") +
                scale_color_manual(values=c("#e41a1c", "#377eb8"))
         })
      
      # produce plot
      p_crit_p1_p2 <- reactive({
        p <- ggplot(data(), aes(x = `Pred1 + Pred2`, y = Criterion, color = Group)) +
                geom_point(alpha = .6, size = 3) +
                geom_smooth(method = "lm", size = 2) +
                #stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~")), label.y = c(3.6, 4), size = 6) +
                annotate("text", -3.7, 4, label = paste(round(crosstab_A12()["not-selected", "suitable"]/sum(crosstab_A12())*100, 1), "%"), color = "#e41a1c", size = 8) +
                annotate("text", -3.7, 3.7, label = paste(round(crosstab_B12()["not-selected", "suitable"]/sum(crosstab_B12())*100, 1), "%"), color = "#377eb8", size = 8) +
                annotate("text", -3.7, -3.7, label = paste(round(crosstab_A12()["not-selected", "not-suitable"]/sum(crosstab_A12())*100, 1), "%"), color = "#e41a1c", size = 8) +
                annotate("text", -3.7, -4, label = paste(round(crosstab_B12()["not-selected", "not-suitable"]/sum(crosstab_B12())*100, 1), "%"), color = "#377eb8", size = 8) +
                annotate("text", 3.7, 4, label = paste(round(crosstab_A12()["selected", "suitable"]/sum(crosstab_A12())*100, 1), "%"), color = "#e41a1c", size = 8) +
                annotate("text", 3.7, 3.7, label = paste(round(crosstab_B12()["selected", "suitable"]/sum(crosstab_B12())*100, 1), "%"), color = "#377eb8", size = 8) +
                annotate("text", 3.7, -3.7, label = paste(round(crosstab_A12()["selected", "not-suitable"]/sum(crosstab_A12())*100, 1), "%"), color = "#e41a1c", size = 8) +
                annotate("text", 3.7, -4, label = paste(round(crosstab_B12()["selected", "not-suitable"]/sum(crosstab_B12())*100, 1), "%"), color = "#377eb8", size = 8) +
                scale_x_continuous(limits = c(-4, 4), breaks = seq(-4, 4, 1)) + 
                scale_y_continuous(limits = c(-4, 4), breaks = seq(-4, 4, 1)) +
                geom_hline(yintercept = input$baserate, linetype = "dashed") +
                geom_vline(xintercept = input$cutoff, linetype = "dashed") +
                coord_fixed() + 
                theme_classic() +
                theme(text = element_text(size = 22),
                      legend.position="bottom") +
                scale_color_manual(values=c("#e41a1c", "#377eb8"))
         })
      
    
    # output plot crit~p1
    output$p_crit_p1 <- renderPlot({
        print(p_crit_p1())
    })
    
    # output plot cirt ~ p2
    output$p_crit_p2 <- renderPlot({
        print(p_crit_p2())
    })
    
    # output plot cirt ~ p1+p2
    output$p_crit_p1_p2 <- renderPlot({
        print(p_crit_p1_p2())
    })
    
    # produce data output (for debugging)
    output$data <- renderTable({
        data()      
    })
      
    # produce table output for diagnostic values (1)
    output$diagnostics_1 <- renderTable({
        diagnostics_1()
    },
      rownames = TRUE
    )
    
    # produce table output for diagnostic values (2)
    output$diagnostics_2 <- renderTable({
        diagnostics_2()
    },
      rownames = TRUE
    )
    
    # produce table output for diagnostic values (1+2)
    output$diagnostics_12 <- renderTable({
        diagnostics_12()
    },
      rownames = TRUE
    )
    
    # output for plot crit ~ p1 download
    output$dl_p_crit_p1 <- downloadHandler(
    filename = function() { paste(input$dataset, '.png', sep='') },
    content = function(file) {
        device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
        ggsave(file, plot = p_crit_p1(), device = device, type = "cairo")
    }
    )
    
    # output for plot crit ~ p2 download
    output$dl_p_crit_p2 <- downloadHandler(
    filename = function() { paste(input$dataset, '.png', sep='') },
    content = function(file) {
        device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
        ggsave(file, plot = p_crit_p2(), device = device, type = "cairo")
    }
    )
    
        # output for plot crit ~ p1 + p2 download
    output$dl_p_crit_p1_p2 <- downloadHandler(
    filename = function() { paste(input$dataset, '.png', sep='') },
    content = function(file) {
        device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
        ggsave(file, plot = p_crit_p1_p2(), device = device, type = "cairo")
    }
    )


    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
