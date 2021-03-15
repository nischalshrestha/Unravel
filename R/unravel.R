
#' Unravels dplyr code for exploration.
#'
#' @param code A character
#'
#' @return A shiny app
#' @export
#'
#' @examples
#' unravel("mtcars %>% select(cyl, mpg)")
unravel <- function(code) {

  # TODO store these as interesting examples to try out in shiny app
  # example 1
  # Lesson: the toggle off can help us understand data semantic mistakes, like not grouping before summarizing
  # Lesson: reordering `arrange` before `summarise` reveals how `summarise` no longer respects order according to `arrange`
  # Lesson: the toggle off on certain verbs can also not really affect verbs down the pipeline such as `select` here (redundancy)
  # Lesson: code and data callouts help identify the new variables you should pay attention to (e.g. `n` and `price` via `summarise()`)
  # group_by and summarize go hand in hand together, don't forget to group_by if summarising on certain variables
  # note also how `select` in this example only gives us problems if placed after a line that removes one of the variables
  # diamonds %>%
  #   select(carat, cut, color, clarity, price) %>%
  #   group_by(color) %>%
  #   summarise(n = n(), price = mean(price)) %>%
  #   arrange(desc(color))

  # example 2
  # Lesson: summary box + data prompt is useful to show how `pivot_wider` reshapes the data (dimension change + explanation)
  # Lesson: toggling off a verb shows column dependencies btw verbs: for e.g., the `pivot_wider` creates the M/F columns
  # required to compute new variables (the box is handy in showing the drastic change)
  # babynames %>%
  #    group_by(year, sex) %>%
  #    summarise(total = sum(n)) %>%
  #    pivot_wider(names_from = sex, values_from = total) %>%
  #    mutate(percent_male = M / (M + F) * 100, ratio = M / F)

  # example 3:
  # Lesson: the toggle off can highlight the importance of dropping NAs for columns which you operate on for
  # other verbs like group_by/summarise
  # starwars %>%
  #   drop_na(hair_color, mass) %>% # toggle this off to see effect of forgetting to drop NAs
  #   group_by(hair_color) %>%
  #   summarise(mass_mean = mean(mass))

  # example 4:
  # Lesson: the tool's summary box color is also useful in internal changes like rowwise(), and with the
  # data prompt summary pointing out we're still grouped by rows + toggle off on `rowwise`, it can point
  # out how rowwise affects subsequent verbs, until you do an explicit `ungroup`
  # mtcars %>%
  #   rowwise() %>%
  #   mutate(mymean=mean(c(cyl,mpg))) %>%
  #   ungroup() %>%
  #   select(cyl, mpg, mymean)

  # example 5
  # Lesson: the summary box color + output reveals certain api behavior that the dimension numbers or code callouts might not help.
  # For e.g. `group_by` overrides the previous grouped variables
  # but this is only apparent when we see that for each case, the data is changed internally, and the output shows a different
  # column being used as the grouped variable
  # mtcars %>%
  #   group_by(cyl) %>%
  #   group_by(disp)

  ui <- fluidPage(
    datawatsUI("datawat", code)
  )

  server <- function(input, output, session) {
    datawatsServer("datawat")
  }

  shinyApp(ui, server)
}
