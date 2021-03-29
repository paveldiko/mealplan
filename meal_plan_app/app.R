#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(rhandsontable)
library(shiny)
library(stringr)

source('meal_plan_functions.R')

# Define UI for application that draws a histogram
ui <- fluidPage(

        
    # Handsontable
    mainPanel(
        tabsetPanel(
            tabPanel('edit', 
                     verticalLayout(
                         rHandsontableOutput('hot'),
                         flowLayout(
                             actionButton("proposeMenu", label = "Propose menu"),
                             actionButton("saveMenu", label = "Save menu")
                            )
                         )
                     ),
            tabPanel('view', verticalLayout(
                downloadButton('downloadMenus', 'Download menus'),
                DT::dataTableOutput('table')
                )),
            tabPanel('Settings', dateRangeInput('menuDates', 
                                                label = 'Menu date range:',
                                                start = today(),
                                                end = today() + ddays(6)))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    dishList = importDishList()
    breakfasts = dishList %>%
        filter(meal == 'breakfast')
    lunchSoups = dishList %>%
        filter(meal == 'lunch', course == 'soup')
    lunchMains = dishList %>%
        filter(meal == 'lunch', course == 'main')
    dinnerMains = dishList %>%
        filter(meal == 'dinner', course == 'main')
    
    initialHistory = importMenus()
    values = reactiveValues(menu = data.frame(),
                            history = initialHistory)

    observeEvent(input$menuDates, {

        startDate = input$menuDates[1]
        endDate = input$menuDates[2]
        if (endDate < startDate) {
            endDate = startDate
            updateDateRangeInput(session, 'menuDates', start = startDate, end = endDate)
        }
        
        menuDates = seq(startDate, endDate, by = 'day')
        proposedMenu = proposeMenu(menuDates, dishList)
        values[['menu']] = proposedMenu %>%
            transmute(date,
                      dayType,
                      meal_course = paste(meal, course, sep = '_'),
                      dish,
                      keep = FALSE)
    })
    
        
    output$hot = renderRHandsontable({
        # print('in render hot:')
        df = values[['menu']]
        currentMenu = dcast(df, 
                   date ~ meal_course, 
                   value.var = 'dish') 
        
        currentFlags = dcast(df,
                             date ~ meal_course,
                             value.var = 'keep') %>%
            rename(breakfast_main_keep = breakfast_main,
                   lunch_soup_keep = lunch_soup,
                   lunch_main_keep = lunch_main,
                   dinner_main_keep = dinner_main)
        
        df = inner_join(currentMenu, currentFlags, 
                        by = 'date') %>%
            mutate(wkday = weekdays(date, abbreviate = TRUE)) %>%
            relocate(date, 
                     wkday, 
                     breakfast_main, 
                     breakfast_main_keep,
                     lunch_soup, 
                     lunch_soup_keep,
                     lunch_main, 
                     lunch_main_keep,
                     dinner_main,
                     dinner_main_keep)
        rhandsontable(df, 
                      colHeaders =, c('Date',
                                      'Wday',
                                      'Breakfast',
                                      'Fix',
                                      'Lunch soup',
                                      'Fix',
                                      'Lunch main',
                                      'Fix',
                                      'Dinner main',
                                      'Fix'),
                      width = 1000) %>%
            hot_col(col = 'Breakfast', 
                    type = 'dropdown',
                    source = unique(breakfasts$dishName)) %>%
            hot_col(col = 'Lunch soup', 
                    type = 'dropdown',
                    source = unique(lunchSoups$dishName)) %>%
            hot_col(col = 'Lunch main', 
                    type = 'dropdown',
                    source = unique(lunchMains$dishName)) %>%
            hot_col(col = 'Dinner main', 
                    type = 'dropdown',
                    source = unique(dinnerMains$dishName))
        
        
        
    })
    
    output$table = DT::renderDataTable({
        df = values[['history']]
        df = df %>%
            dcast(date ~ meal + course) %>%
            mutate(wkday = weekdays(date, abbreviate = TRUE)) %>%
            relocate(date,
                     wkday,
                     breakfast_main,
                     lunch_soup,
                     lunch_main,
                     dinner_main)
        
        DT::datatable(df, 
                      options = list(pageLength = 7, 
                                     lengthMenu = c(7, 14, 28),
                                     autoWidth = TRUE,
                                     columnDefs = list(list(width = '10ex', targets = list(1))),
                                     scrollX = TRUE
                                     ),
                      width = 1500,
                      colnames = c(
            'Date',
            'WkDay',
            'Breakfast',
            'Lunch soup',
            'Lunch main',
            'Dinner main'
        ))
    })
    
    output$downloadMenus = downloadHandler(
        filename = 'menus.csv',
        content = function(fileId) {
            write.csv(values[['history']], fileId, row.names = FALSE)
        }
    )
    
    observeEvent(input$saveMenu, {
        # print('in save menu:')
        finalMenu = isolate(hot_to_r(input$hot)) 
        finalMenu = finalMenu %>%
            select(date,
                   breakfast_main,
                   lunch_soup,
                   lunch_main,
                   dinner_main) %>%
            melt(id.vars = 'date', 
                 variable.name = 'meal_course',
                 value.name = 'dish')
        
        aux = str_split_fixed(finalMenu$meal_course, '_', 2)
        finalMenu = transmute(finalMenu, 
                  date,
                  meal = aux[, 1],
                  course = aux[, 2],
                  dish
                  )
        
        # update menu history
        menuHistory = isolate(values[['history']])
        
        if (is.null(menuHistory)) {
            menuHistory = finalMenu
        } else {
            menuHistory = menuHistory %>%
                full_join(finalMenu, by = c('date', 'meal', 'course')) %>%
                transmute(date,
                          meal,
                          course,
                          dish = ifelse(is.na(dish.y), dish.x, dish.y))    
        }
                
        exportMenus(menuHistory)
        
        values[['history']] = menuHistory
        
        # values[['menu']] = finalMenu
    })
    
    # Propose new dishes that are not flagged to be kept
    observeEvent(input$proposeMenu, {
        flaggedMenu = isolate(hot_to_r(input$hot))
        currentMenu = flaggedMenu %>%
            select(date,
                   breakfast_main,
                   lunch_soup,
                   lunch_main,
                   dinner_main) %>%
            melt(id.vars = 'date',
                 variable.name = 'meal_course',
                 value.name = 'dish')
        flags = flaggedMenu %>%
            select(date,
                   breakfast_main_keep,
                   lunch_soup_keep,
                   lunch_main_keep,
                   dinner_main_keep) %>%
            rename(breakfast_main = breakfast_main_keep,
                   lunch_soup = lunch_soup_keep,
                   lunch_main = lunch_main_keep,
                   dinner_main = dinner_main_keep
                   ) %>%
            melt(id.vars = 'date',
                 variable.name = 'meal_course',
                 value.name = 'keep')
        flaggedMenu = inner_join(currentMenu, flags, 
                                 by = c('date', 'meal_course'))
        
        startDate = input$menuDates[1]
        endDate = input$menuDates[2]

        menuDates = seq(startDate, endDate, by = 'day')
        proposedMenu = proposeMenu(menuDates, dishList) %>%
            transmute(date,
                      meal_course = paste(meal, course, sep = '_'),
                      dish)
        
        currentMenu = inner_join(flaggedMenu, proposedMenu, 
                                 by = c('date', 'meal_course')) %>%
            transmute(date,
                      meal_course,
                      dish = ifelse(keep, dish.x, dish.y),
                      keep)
        values[['menu']] = currentMenu
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
