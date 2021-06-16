#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

drive_auth(email = "financefifo@gmail.com")
drive_find()
get_data <- drive_download("Logbook_FA_viz.xlsx", path = NULL, type = NULL, overwrite = TRUE, verbose = TRUE)
data_set <- read_excel("Logbook_FA_viz.xlsx", 
                                         sheet = "Petty", skip = 1)

# Transformasi Data
data_set$No <- as.character(data_set$No)
data_set$Date <- as.Date(c(data_set$Date), format = "%Y-%m-%d")
data_set$`Company Code` <- as.factor(data_set$`Company Code`)


# Tarik data pada kolom No, Date, Company Code, Debit, Credit, Balance
cash_flow <- select(data_set, "No", "Company Code", "Debit":"Balance")

cash_flow_plot <- select(data_set, "Date","Company Code", "Debit":"Balance")

cash_flow_replace <- cash_flow_plot %>%
  replace_na(list(Debit = 0, Credit = 0))

flow_TCM <- cash_flow_replace[cash_flow_replace$`Company Code` == 'TCM',]
flow_BEK <- cash_flow_replace[cash_flow_replace$`Company Code` == 'BEK',]
flow_ITM <- cash_flow_replace[cash_flow_replace$`Company Code` == 'ITM',]
flow_GEM <- cash_flow_replace[cash_flow_replace$`Company Code` == 'GEM',]
flow_KTD <- cash_flow_replace[cash_flow_replace$`Company Code` == 'KTD',]
flow_IMM <- cash_flow_replace[cash_flow_replace$`Company Code` == 'IMM',]
flow_JBG <- cash_flow_replace[cash_flow_replace$`Company Code` == 'JBG',]
flow_TRUST <- cash_flow_replace[cash_flow_replace$`Company Code` == 'TRUST',]

#ploting for balance column
flow_balance <- cash_flow_replace %>%
  group_by(cash_flow_replace$`Company Code`) %>%
  plot_ly(x = ~Date, y = ~Balance) %>%
  add_lines(data = filter(cash_flow_replace,
                          `Company Code` %in% c("TCM", "BEK", "ITM", "GEM", "KTD", "IMM", "JBG", "TRUST")
                          ),
            hoverinfo = "Company Code",
            line = list(color = c("red", "blue", "yellow", "brown", "bisque", "black", "aqua", "coral")),
            color = ~`Company Code`
            )
flow_balance <- flow_balance %>% layout(title = 'Balance Graphic Flow')

#all site
all_site <- cash_flow_replace %>%
  group_by(
    data_bulanan = floor_date(Date, "30 days")
  ) %>%
  summarize(Debit = sum(Debit),
            Credit = sum(Credit)
            )

dat <- all_site %>%
  gather(Total, Value, -data_bulanan)

#menghitung minggu TCM
plot_TCM <- flow_TCM %>%
  group_by(
    data_mingguan = floor_date(Date, "30 days")
  ) %>%
  summarize(Debit = sum(Debit),
            Credit = sum(Credit)
            )

dat2 <- plot_TCM %>%
  gather(Total, Value, -data_mingguan)

#menghitung minggu BEK
plot_BEK <- flow_BEK %>%
  group_by(
    data_mingguan = floor_date(Date, "7 days")
  ) %>%
  summarize(Debit = sum(Debit),
            Credit = sum(Credit)
            )

dat3 <- plot_BEK %>%
  gather(Total, Value, -data_mingguan)

#menghitung minggu ITM
plot_ITM <- flow_ITM %>%
  group_by(
    data_mingguan = floor_date(Date, "7 days")
  ) %>%
  summarize(Debit = sum(Debit),
            Credit = sum(Credit)
            )

dat4 <- plot_ITM %>%
  gather(Total, Value, -data_mingguan)

#menghitung minggu GEM
plot_GEM <- flow_GEM %>%
  group_by(
    data_mingguan = floor_date(Date, "7 days")
  ) %>%
  summarize(Debit = sum(Debit),
            Credit = sum(Credit)
            )

dat5 <- plot_GEM %>%
  gather(Total, Value, -data_mingguan)

#menghitung minggu KTD
plot_KTD <- flow_KTD %>%
  group_by(
    data_mingguan = floor_date(Date, "7 days")
  ) %>%
  summarize(Debit = sum(Debit),
            Credit = sum(Credit)
            )

dat6 <- plot_KTD %>%
  gather(Total, Value, -data_mingguan)

#menghitung minggu IMM
plot_IMM <- flow_IMM %>%
  group_by(
    data_mingguan = floor_date(Date, "7 days")
  ) %>%
  summarize(Debit = sum(Debit),
            Credit = sum(Credit)
            )

dat7 <- plot_IMM %>%
  gather(Total, Value, -data_mingguan)

#menghitung minggu JBG
plot_JBG <- flow_JBG %>%
  group_by(
    data_mingguan = floor_date(Date, "7 days")
  ) %>%
  summarize(Debit = sum(Debit),
            Credit = sum(Credit)
            )

dat8 <- plot_JBG %>%
  gather(Total, Value, -data_mingguan)

#menghitung minggu TRUST
plot_TRUST <- flow_TRUST %>%
  group_by(
    data_mingguan = floor_date(Date, "7 days")
  ) %>%
  summarize(Debit = sum(Debit),
            Credit = sum(Credit)
            )

dat9 <- plot_TRUST %>%
  gather(Total, Value, -data_mingguan)

#all site flow
debit_flow_all <- ggplot(dat, aes(x = data_bulanan, y = Value, fill = Total)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = number_format(accuracy = 1.00)) +
  labs(
    x = "Data Bulanan",
    y = "Total",
    title = "Cash Flow All Site",
    subtitle = "FIFO Data Info",
    caption = "Sumber data : Team Finance Balikpapan"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21
  ) +
  theme(plot.title.position = "plot",
        legend.box = "horizontal",
        legend.position = "top"
  )

#plotting TCM
debit_flow_TCM <- ggplot(dat2, aes(x = data_mingguan, y = Value, fill = Total)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = number_format(accuracy = 1.00)) +
  labs(
    x = "Data Mingguan",
    y = "Total",
    title = "Cash Flow TCM",
    subtitle = "FIFO Data Info",
    caption = "Sumber data : Team Finance Balikpapan"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21
  ) +
  theme(plot.title.position = "plot",
        legend.box = "horizontal",
        legend.position = "top"
  )

#plotting BEK
debit_flow_BEK <- ggplot(dat3, aes(x = data_mingguan, y = Value, fill = Total)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = number_format(accuracy = 1.00)) +
  labs(
    x = "Data Mingguan",
    y = "Total",
    title = "Cash Flow BEK",
    subtitle = "FIFO Data Info",
    caption = "Sumber data : Team Finance Balikpapan"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21
  ) +
  theme(plot.title.position = "plot",
        legend.box = "horizontal",
        legend.position = "top"
  )

#plotting ITM
debit_flow_ITM <- ggplot(dat4, aes(x = data_mingguan, y = Value, fill = Total)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = number_format(accuracy = 1.00)) +
  labs(
    x = "Data Mingguan",
    y = "Total",
    title = "Cash Flow ITM",
    subtitle = "FIFO Data Info",
    caption = "Sumber data : Team Finance Balikpapan"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21
  ) +
  theme(plot.title.position = "plot",
        legend.box = "horizontal",
        legend.position = "top"
  )

#plotting GEM
debit_flow_GEM <- ggplot(dat5, aes(x = data_mingguan, y = Value, fill = Total)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = number_format(accuracy = 1.00)) +
  labs(
    x = "Data Mingguan",
    y = "Total",
    title = "Cash Flow GEM",
    subtitle = "FIFO Data Info",
    caption = "Sumber data : Team Finance Balikpapan"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21
  ) +
  theme(plot.title.position = "plot",
        legend.box = "horizontal",
        legend.position = "top"
  )

#plotting KTD
debit_flow_KTD <- ggplot(dat6, aes(x = data_mingguan, y = Value, fill = Total)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = number_format(accuracy = 1.00)) +
  labs(
    x = "Data Mingguan",
    y = "Total",
    title = "Cash Flow KTD",
    subtitle = "FIFO Data Info",
    caption = "Sumber data : Team Finance Balikpapan"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21
  ) +
  theme(plot.title.position = "plot",
        legend.box = "horizontal",
        legend.position = "top"
  )

#plotting IMM
debit_flow_IMM <- ggplot(dat7, aes(x = data_mingguan, y = Value, fill = Total)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = number_format(accuracy = 1.00)) +
  labs(
    x = "Data Mingguan",
    y = "Total",
    title = "Cash Flow IMM",
    subtitle = "FIFO Data Info",
    caption = "Sumber data : Team Finance Balikpapan"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21
  ) +
  theme(plot.title.position = "plot",
        legend.box = "horizontal",
        legend.position = "top"
  )

#plotting JBG
debit_flow_JBG <- ggplot(dat8, aes(x = data_mingguan, y = Value, fill = Total)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = number_format(accuracy = 1.00)) +
  labs(
    x = "Data Mingguan",
    y = "Total",
    title = "Cash Flow JBG",
    subtitle = "FIFO Data Info",
    caption = "Sumber data : Team Finance Balikpapan"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21
  ) +
  theme(plot.title.position = "plot",
        legend.box = "horizontal",
        legend.position = "top"
  )

#plotting TRUST
debit_flow_TRUST <- ggplot(dat9, aes(x = data_mingguan, y = Value, fill = Total)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = number_format(accuracy = 1.00)) +
  labs(
    x = "Data Mingguan",
    y = "Total",
    title = "Cash Flow TRUST",
    subtitle = "FIFO Data Info",
    caption = "Sumber data : Team Finance Balikpapan"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21
  ) +
  theme(plot.title.position = "plot",
        legend.box = "horizontal",
        legend.position = "top"
  )

data_TCM <- cash_flow[cash_flow$`Company Code` == 'TCM',]
data_BEK <- cash_flow[cash_flow$`Company Code` == 'BEK',]
data_ITM <- cash_flow[cash_flow$`Company Code` == 'ITM',]
data_GEM <- cash_flow[cash_flow$`Company Code` == 'GEM',]
data_KTD <- cash_flow[cash_flow$`Company Code` == 'KTD',]
data_IMM <- cash_flow[cash_flow$`Company Code` == 'IMM',]
data_JBG <- cash_flow[cash_flow$`Company Code` == 'JBG',]
data_TRUST <- cash_flow[cash_flow$`Company Code` == 'TRUST',]

# Define server logic required to draw a histogram
shinyServer(function(input, output){
  output$table_cash_flow <- renderTable({
    if(input$code == 'TCM'){data_TCM}
    else{
      if(input$code == 'BEK'){data_BEK}
      else{
        if(input$code == 'ITM'){data_ITM}
        else{
          if(input$code == 'GEM'){data_GEM}
          else{
            if(input$code == 'KTD'){data_KTD}
            else{
              if(input$code == 'IMM'){data_IMM}
              else{
                if(input$code == 'JBG'){data_JBG}
                else{
                  if(input$code == 'TRUST'){data_TRUST}
                  else{
                    if(input$code == 'ALL'){cash_flow}
                    else{
                      cash_flow
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
  })
  output$plot <- renderPlot({
    if(input$code == 'TCM'){debit_flow_TCM}
    else{
      if(input$code == 'BEK'){debit_flow_BEK}
      else{
        if(input$code == 'ITM'){debit_flow_ITM}
        else{
          if(input$code == 'GEM'){debit_flow_GEM}
          else{
            if(input$code == 'KTD'){debit_flow_KTD}
            else{
              if(input$code == 'IMM'){debit_flow_IMM}
              else{
                if(input$code == 'JBG'){debit_flow_JBG}
                else{
                  if(input$code == 'TRUST'){debit_flow_TRUST}
                  else{
                    if(input$code == 'ALL'){debit_flow_all}
                    else{
                      debit_flow_all
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  })
  output$plot2 <- renderPlotly({
    flow_balance
    })
})

# Run the application
#shinyApp(ui = ui, server = server)
