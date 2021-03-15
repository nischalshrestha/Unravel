
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
  # Lesson: the reorder of lines can reveal how sometimes verbs depends on order, e.g. `filter` has to follow `groupby` here:
  # starwars %>%
  #   group_by(species) %>%
  #   filter(n() > 1) %>%
  #   summarise(across(c(sex, gender, homeworld), ~ length(unique(.x))))

  # example 3
  # Lesson: the data prompt and summary box color can point out whether or not we still have a grouping variable in the chain
  # (real question which our tool could help with: https://community.rstudio.com/t/understanding-group-by-order-matters/22685)
  # mtcars %>%
  #   group_by(cyl, gear) %>%
  #   summarise(mean_mpg = mean(mpg))

  # example 4:
  # Lesson: the toggle off can highlight the importance of dropping NAs for columns which you operate on for
  # other verbs like group_by/summarise
  # starwars %>%
  #   drop_na(hair_color, mass) %>% # toggle this off and notice how summarise has empty results
  #   group_by(hair_color) %>%
  #   summarise(mass_mean = mean(mass))

  # example 5:
  # Lesson: the tool's summary box color is also useful in internal changes like rowwise(), and with the
  # data prompt summary pointing out we're still grouped by rows + toggle off on `rowwise`, it can point
  # out how rowwise affects subsequent verbs, until you do an explicit `ungroup`
  # mtcars %>%
  #   rowwise() %>%
  #   mutate(mymean = mean(c(cyl, mpg))) %>%
  #   ungroup() %>% # toggle this off and notice how `select` keeps rows grouped
  #   select(cyl, mpg, mymean)

  # example 6
  # Lesson: the summary box color + output reveals certain api behavior that the dimension numbers or code callouts might not help.
  # For e.g. `group_by` overrides the previous grouped variables
  # but this is only apparent when we see that for each case, the data is changed internally, and the output shows a different
  # column being used as the grouped variable
  # mtcars %>%
  #   group_by(cyl) %>%
  #   group_by(disp)

  # example 4
  # Lesson: summary box + data prompt is useful to show how `pivot_wider` reshapes the data (dimension change + explanation)
  # Lesson: toggling off a verb shows column dependencies btw verbs: for e.g., the `pivot_wider` creates the M/F columns
  # required to compute new variables (the box is handy in showing the drastic change)
  # babynames %>%
  #    group_by(year, sex) %>%
  #    summarise(total = sum(n)) %>%
  #    pivot_wider(names_from = sex, values_from = total) %>% # note the dimension change
  #    mutate(percent_male = M / (M + F) * 100, ratio = M / F) # toggle off above line to see column dependency on M/F

  ui <- fluidPage(
    datawatsUI("datawat", code)
  )

  server <- function(input, output, session) {
    datawatsServer("datawat")
  }

  shinyApp(ui, server)
}
