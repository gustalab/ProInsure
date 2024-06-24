
library(shinyjs)
library(shiny)
library(shinyWidgets)

# UI ----

login_ui <- function(id, title) {
  
  ns <- NS(id)
  
  div(
    id = ns("login"),
    style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px; background-color: red;",
    div(
      class = "well",
      h2(class = "text-center", title),
      
      textInput(inputId = ns("user_name"),
                label = tagList(icon("user"), "Username"), 
                placeholder = "Enter user name"),
      
      
      passwordInput(inputId = ns("password"),
                    label = tagList(icon("fa-thin-fa-key-skeleton"), "Password"),
                    placeholder = "Enter password"),
      
      div(
        class = "text-center",
        actionButton(inputId = ns("login_button"), "Log in", class = "btn-primary", style = "color:white;")
      )
      
      
    )
  )
  
  
}

# SERVER ----

validate_pwd <- function(input, output, session, data, user_col, pwd_col) {
  
  
  user <- data %>% pull(!! enquo(user_col))
  pwd <- data %>% pull(!! enquo(pwd_col))
  
  eventReactive(input$login_button, {
    
    validate <- FALSE
    if(input$user_name == user && input$password == pwd) {
      
      validate <- TRUE
      
    }
    
    if (validate) shinyjs::hide(id = "login")
    
    validate
    
  })
  
}

 # MODULE ----

modFunction <- function(input, output, session, data, reset) {
  
  v <- reactiveValues(data = data)
  
  proxy = dataTableProxy("mod_table")
  
  observeEvent(input$mod_table_cell_edit, {
    print(names(v$data))
    info = input$mod_table_cell_edit
    str(info)
    i = info$row
    j = info$col
    k = info$value
    str(info)
    
    isolate(
      if (j %in% match("Gelir", names(v$data))) {
        print(match("Gelir", names(v$data)))
        v$data[i, j] <<- DT::coerceValue(k, v$data[i, j])
        print(v$data)
        
      } else {
        print("You are not supposed to change this column.") # check to stop the user from editing only few columns
      }
    )
    replaceData(proxy, v$data, resetPaging = FALSE)  # replaces data displayed by the updated table
  })
  
  ### Reset Table
  observeEvent(reset(), {
    v$data <- data # your default data
  })
  
  print(isolate(colnames(v$data)))
  output$mod_table <- DT::renderDataTable({
    DT::datatable(v$data, options = list(paging = F, searching = F), editable = TRUE)
    
  })
  
  return(v)
}

modFunctionUI <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("mod_table"))
  
}



