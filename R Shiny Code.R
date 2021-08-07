library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(tidylog)
library(tidylog)
library(r2d3)
library(shinydashboard)
library(ECharts2Shiny)

data <- readxl::read_xlsx("C:\\Users\\deepa\\Downloads\\mp_ac_152_split.xlsx")
form20 <- as.data.frame(readxl::read_xlsx("C:\\Users\\deepa\\Downloads\\152_Form20.xlsx"))
data2 <- readxl::read_xlsx("C:\\Users\\deepa\\Downloads\\Religion Last Names.xlsx", sheet = 1)
data3 <- readxl::read_xlsx("C:\\Users\\deepa\\Downloads\\Religion Last Names.xlsx", sheet = 2)

data <- data[!duplicated(data$VoterID),]

lol1 <- data[tolower(data$LastNameEng) %in% tolower(data2$Name),]
lol1["Religion"] <- "muslim"
dataf <- subset(data, !(tolower(data$LastNameEng) %in% tolower(data2$Name)))
lol2 <- dataf[tolower(dataf$FirstNameEng) %in% tolower(data3$Name),]
lol2["Religion"] <- "muslim"
dataf <- subset(dataf, !(tolower(dataf$FirstNameEng) %in% tolower(data3$Name)))
dataf["Religion"] <- "hindu"
dat <- rbind(lol1, lol2, dataf)

data <- as_data_frame(dat)
data <- data %>% drop_na(PollingStationAddressEn)
data$Age_Group <- ifelse(data$Age < 28, "18-27", ifelse(data$Age < 38, "28-37", ifelse(data$Age < 48, "38-47", ifelse(data$Age < 58, "48-57", ifelse(data$Age < 68, "58-67", "68+")))))
colnames(data)
d <- dim(data)

const <- as.list(c("Digvijay Singh", "Sadhvi Pragya Thakur"))

data$PartNo <- sprintf("%03d", as.numeric(data$PartNo))
form20$PartNo <- sprintf("%03d", as.numeric((form20$PartNo)))
head(form20$PartNo)
y <- as_tibble(cbind("000 Entire Constituency", d[1]))
colnames(y) <- c("Booth", "n")
y

data["Booth"] <- paste(data$PartNo, data$PollingStationAddressEn)
lol <- data %>% group_by(Booth) %>% count()
lol <- rbind(y , lol)
lol %>% drop_na(Booth)
lol <- cbind(lol, form20)
lol["Voting Percentage"] <- (as.numeric(lol$Total_Votes)/as.numeric(lol$n))
lol["Voting Difference"] <- abs(as.numeric(lol$Digvijay_Singh.INC.) - as.numeric(lol$Pragya_Thakur.BJP.))
lol["Non Votes"] <- as.numeric(lol$Total_Votes) - (as.numeric(lol$Digvijay_Singh.INC.) + as.numeric(lol$Pragya_Thakur.BJP.))
lol$Winner <- ifelse(lol$Digvijay_Singh.INC. < lol$Pragya_Thakur.BJP., "BJP", "INC")
head(lol)

hehe <- data %>%
  group_by(PartNo)%>%
  filter(Age <= 23) %>%
  tally() %>%
  pull() %>%
  as.data.frame()

head(hehe)

huhu <- data %>%
  filter(Age <= 23) %>%
  tally() %>%
  pull() %>%
  as.data.frame()

f <- rbind(huhu, hehe)

lol["Swing"] <- f

lol["Other"] <- lol$Total_Votes - lol$Digvijay_Singh.INC. - lol$Pragya_Thakur.BJP.

foo1 <- cbind(as.data.frame(lol$Booth), lol$Digvijay_Singh.INC., lol$Pragya_Thakur.BJP. + lol$Other)
colnames(foo1) <- c("Booth", "INC - Digvijay Singh", "BJP - Sadhvi Pragya Thakur")
foo2 <- cbind(as.data.frame(lol$Booth), lol$Digvijay_Singh.INC. + lol$Other, lol$Pragya_Thakur.BJP.)
colnames(foo2) <- c("Booth", "INC - Digvijay Singh", "BJP - Sadhvi Pragya Thakur")
foo3 <- cbind(as.data.frame(lol$Booth), lol$Digvijay_Singh.INC., lol$Pragya_Thakur.BJP., lol$Other)
colnames(foo3) <- c("Booth", "INC - Digvijay Singh", "BJP - Sadhvi Pragya Thakur", "Others")


ui <- dashboardPage(
  dashboardHeader(
    title = "Polling Booth Dashboard",
    titleWidth = 200
  ),
  dashboardSidebar(
    width = 300,
    selectInput(
      inputId = "party",
      label = "Select Candidate:",
      choices = const,
      selectize = FALSE
    ),
    sidebarMenu(
      selectInput(
        inputId = "booth",
        label = "Polling Booth:",
        choices = lol[1],
        selected = 1,
        selectize = TRUE
      ),
      actionLink("remove", "Remove detail tabs")
    )
  ),
  dashboardBody(
    tags$head(tags$script(loadEChartsLibrary())),
    tabsetPanel(
      id = "tabs",
      tabPanel(
        loadEChartsLibrary(),
        title = "Main Dashboard",
        value = "page1",
        fluidRow(
          valueBoxOutput("total_voters", width = 2),
          valueBoxOutput("total_votes", width = 2),
          valueBoxOutput("Vot_per", width = 2),
          valueBoxOutput("Vot_diff", width = 2),
          valueBoxOutput("Vot_non", width = 2),
          valueBoxOutput("total_swing", width = 2),
        ),
        fluidRow(
          valueBoxOutput("male_vote", width = 3),
          valueBoxOutput("female_vote", width = 3),
          valueBoxOutput("hindu_vote", width = 3),
          valueBoxOutput("muslim_vote", width = 3),
        ),
        fluidRow(),
        fluidRow(
          column(
            width = 4,
            d3Output("age_groups")
          ),
          column(
            width = 3,
            selectInput(inputId = "select", 
                        label = h3("Select Category"),
                        choices = list("Current Vote Share" = 3, "Vote Share with others included" = 2),
                        selected = 3),
            valueBoxOutput("text", width = 100),
          ),
          column(
            width = 5,
            tags$div(id="test_2", style="width:100%; height:300px;"),  # Specify the div for the chart. Can also be considered as a space holder
            deliverChart(div_id = "test_2"),
          )# Deliver the plotting
        )
      )
    )
  )
)

server <- function(input, output, session) {
  tab_list <- NULL
  
  base_voters <- reactive({
    res <- data
    if(input$booth != "000 Entire Constituency") res <- filter(res, Booth ==input$booth)
    res
  })
  base_votes <- reactive({
    res1 <- lol
    res1 <- filter(res1, Booth ==input$booth)
    res1
  })
  share_votes <- reactive({
    res2 <- foo1
    res3 <- foo2
    res4 <- foo3
    if(input$select==2 && input$party== "Sadhvi Pragya Thakur"){
      rest <- filter(res2, Booth==input$booth)
      res <- data.frame(
        name = c("INC - Digvijay Singh", "BJP - Sadhvi Pragya Thakur"),
        value = c(rest$`INC - Digvijay Singh`, rest$`BJP - Sadhvi Pragya Thakur`)
      )
    }
    if(input$select==2 && input$party == "Digvijay Singh"){
      rest <- filter(res3, Booth==input$booth)
      res <- data.frame(
        name = c("INC - Digvijay Singh", "BJP - Sadhvi Pragya Thakur"),
        value = c(rest$`INC - Digvijay Singh`, rest$`BJP - Sadhvi Pragya Thakur`)
      )
    }
    if(input$select==3){
      rest <- filter(res4, Booth==input$booth)
      res <- data.frame(
        name = c("INC - Digvijay Singh", "BJP - Sadhvi Pragya Thakur", "Others"),
        value = c(rest$`INC - Digvijay Singh`, rest$`BJP - Sadhvi Pragya Thakur`, rest$Others )
      )
    }
    res
  })
  output$total_voters <- renderValueBox({
    base_voters() %>%
      tally() %>%
      pull() %>%
      as.integer() %>%
      prettyNum(big.mark = ",") %>%
      valueBox(subtitle = "Number of Voters")
  })
  output$total_swing <- renderValueBox({
    base_voters() %>%
      filter(Age <= 23) %>%
      tally() %>%
      pull() %>%
      as.integer() %>%
      prettyNum(big.mark = ",") %>%
      valueBox(subtitle = "Number of Swing Voters")
  })
  output$total_votes <- renderValueBox({
    base_votes() %>%
      select("Total_Votes") %>%
      pull() %>%
      as.integer() %>%
      prettyNum(big.mark = ",") %>%
      valueBox(subtitle = "Number of Votes Given", col = "blue")
  })
  output$Vot_per <- renderValueBox({
    base_votes () %>%
      select(("Voting Percentage")) %>%
      as.numeric () %>%
      percent(accuracy = 0.01, decimal.mark = ".") %>%
      valueBox(subtitle = "Voting Percentage", col = "blue")
  })
  output$Vot_diff <- renderValueBox({
    haha <- base_votes()%>%select("Winner")
    if(haha == "BJP"){
      base_votes () %>%
        select(("Voting Difference")) %>%
        as.integer () %>%
        prettyNum(big.mark = ",") %>%
        valueBox(subtitle = "Winning Margin - Sadhvi Pragya", col = "orange") 
    }else if(haha == "INC"){
      base_votes () %>%
        select(("Voting Difference")) %>%
        as.integer () %>%
        prettyNum(big.mark = ",") %>%
        valueBox(subtitle = "Winning Margin - Digvijay Singh", col = "green") 
    }
  })
  output$Vot_non <- renderValueBox({
    base_votes () %>%
      select("Non Votes") %>%
      as.integer () %>%
      prettyNum(big.mark = ",") %>%
      valueBox(subtitle = "Non BJP-INC Votes", col = "blue")
  })
  output$age_groups <- renderD3({
    res <- base_voters() %>%
      group_by(Age_Group) %>%
      tally() %>%
      collect() %>%
      mutate(
        y = n,
        x = Age_Group
      ) %>%
      select(x, y)
    
    res <- res %>% mutate(label = x)
    r2d3(res, "col_plot.js")
  })
  
  get_details <- function(group = NULL, religion = NULL) {
    # Create a generic details function that can be called
    # by different dashboard events
    res <- base_voters()
    if (!is.null(group)) res <- filter(res, Age_Group == group)
    if (!is.null(religion)) res <- filter(res, Religion == religion)
    
    res %>%
      select(
        Booth, HouseNoEn, VoterNameEn, Sex, Age, VoterID, Religion
      ) %>%
      group_by(HouseNoEn)%>%
      collect() %>%
      arrange(VoterNameEn, .by_group = TRUE)
  }
  observeEvent(input$column_clicked != "", {
    
    age_g <- input$column_clicked
    tab_title <- paste(
      input$PartNo, "- Age Group", age_g
    )
    if (!(tab_title %in% tab_list)) {
      appendTab(
        inputId = "tabs",
        tabPanel(
          tab_title,
          DT::renderDataTable(
            get_details(group = age_g)
          )
        )
      )
      tab_list <<- c(tab_list, tab_title)
    }
    updateTabsetPanel(session, "tabs", selected = tab_title)
  },
  ignoreInit = TRUE
  )
  output$male_vote <- renderValueBox({
    base_voters() %>%
      filter(Sex == "M") %>%
      tally() %>%
      pull() %>%
      as.integer() %>%
      prettyNum(big.mark = ",") %>%
      valueBox(subtitle = "Number of Male Voters")
    
  })
  output$female_vote <- renderValueBox({
    base_voters() %>%
      filter(Sex == "F") %>%
      tally() %>%
      pull() %>%
      as.integer() %>%
      prettyNum(big.mark = ",") %>%
      valueBox(subtitle = "Number of Female Voters")
    
  })
  output$hindu_vote <- renderValueBox({
    base_voters() %>%
      filter(Religion == "hindu") %>%
      tally() %>%
      pull() %>%
      as.integer() %>%
      prettyNum(big.mark = ",") %>%
      valueBox(subtitle = "Number of Hindu Voters")
    
  })
  output$muslim_vote <- renderValueBox({
    base_voters() %>%
      filter(Religion == "muslim") %>%
      tally() %>%
      pull() %>%
      as.integer() %>%
      prettyNum(big.mark = ",") %>%
      valueBox(subtitle = "Number of Muslim Voters")
    
  })
  output$text <- renderValueBox({
    res <- share_votes()
    out <- ifelse(res[1,2]>res[2,2], "INC Wins", "BJP Wins")
    out <- out %>%
      as.character() %>%
      valueBox(subtitle = "Who wins?")
    
  })
  observeEvent(share_votes(),
               {
                 renderPieChart(div_id = "test_2",
                                data = share_votes(),
                                radius = "70%",center_x = "50%", center_y = "50%")
               })
  observeEvent(input$remove, {
    # Use purrr's walk command to cycle through each
    # panel tabs and remove them
    tab_list %>%
      walk(~ removeTab("tabs", .x))
    tab_list <<- NULL
  })
}

shinyApp(ui, server)
