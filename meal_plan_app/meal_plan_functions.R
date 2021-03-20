# Meal plan
# Meal planner application functions library
library(dplyr)
library(lubridate)
library(reshape2)

workingDir = '.'
dishListFileId = 'https://onedrive.live.com/download?resid=EA257F6CE45ABBE0!2126&authkey=!AM_X9ceY2J7iFIQ&e=Xxtl3t'
menusFileId = file.path(workingDir, 'menus.csv')

dayTypes = c('workday', 'saturday', 'sunday')
meals = c('breakfast', 'lunch', 'dinner')
courses = c('soup', 'main')

importDishList = function(source = dishListFileId) {
  # dishCode, dishName, 
  # workday, saturday, sunday, 
  # breakfast, lunch, dinner, 
  # soup, main
  tmp = tempfile(fileext = '.csv')
  download.file(source, 
                destfile = tmp)
  dishData = read.csv(tmp)
  dishData = read.csv(dishListFileId)
  melt(dishData, id.vars = c('dishCode',
                             'dishName', 
                             'workday',
                             'saturday',
                             'sunday',
                             'breakfast',
                             'lunch',
                             'dinner')) %>%
    filter(value == TRUE) %>%
    select(-c('value')) %>%
    rename(course = variable) %>%
    melt(id.vars = c('dishCode',
                     'dishName',
                     'workday',
                     'saturday',
                     'sunday',
                     'course')) %>%
    filter(value == TRUE) %>%
    select(-c('value')) %>%
    rename(meal = variable) %>%
    melt(id.vars = c('dishCode',
                     'dishName',
                     'meal',
                     'course')) %>%
    filter(value == TRUE) %>%
    select(-c('value')) %>%
    rename(dayType = variable)
}

# importDishList()

importMenus = function(fileId = menusFile) {
  menus = NULL
  
  if(file.exists(fileId)) {
    menus = read.csv(fileId) %>%
      mutate(date = ymd(date))
  }
  
  menus
}

exportMenus = function(menus, fileId = menusFileId) {
  write.csv(menus, fileId)
}

getCourseDishes = function(aDayType, aMeal, aCourse, dishList) {
  dishList %>% 
    filter(dayType == aDayType,
           meal == aMeal,
           course == aCourse) %>%
    select(dishCode, dishName)
}

# dishList = importDishList()
# getCourseDishes('workday', 'breakfast', 'main', dishList)
# getCourseDishes('workday', 'lunch', 'soup', dishList)

proposeDish = function(courseDishes, pastMenus = NULL) {
  sample(courseDishes$dishName, 1)
}

# dishList = importDishList()
# dishes = getCourseDishes('workday', 'lunch', 'soup', dishList)
# proposeDish(dishes)



buildCourses = function(dates) {
  courseTemplate = data.frame(meal = c('breakfast',
                                       'lunch',
                                       'lunch',
                                       'dinner'),
                              course = c('main',
                                         'soup',
                                         'main',
                                         'main'))
  
  dayTypeTemplate = c(dayTypes[1],
                      dayTypes[1],
                      dayTypes[1],
                      dayTypes[1],
                      dayTypes[1],
                      dayTypes[2],
                      dayTypes[3])
  
  data.frame(date = dates) %>%
    mutate(dayType = dayTypeTemplate[wday(date, week_start = 1)]) %>%
    full_join(courseTemplate, by = character())
}

# buildCourses(ymd(20210131))
# buildCourses(seq(ymd(20210201), ymd(20210207), by = 'day'))

proposeMenu = function(dates, dishList, pastMenus = NULL) {
  menu = buildCourses(dates)
  menu$dish = with(menu,
    mapply(
      function(dayType, meal, course) 
        proposeDish(
          getCourseDishes(dayType, meal, course, dishList)),
      dayType, meal, course)
  )
  menu
}

# dishList = importDishList()
# proposeMenu(today(), dishList)
# proposeMenu(seq(ymd(20210201), ymd(20210207), by = 'day'), dishList)

runMealPlan = function() {
  
}

