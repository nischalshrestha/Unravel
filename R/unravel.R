
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
  # Unravel: the toggle off can help us understand data semantic mistakes, like not grouping before summarizing
  # Unravel: reordering `arrange` before `summarise` reveals how `summarise` no longer respects order according to `arrange`
  # Unravel: the toggle off on certain verbs can also not really affect verbs down the pipeline such as `select` here (redundancy)
  # Unravel: code and data callouts help identify the new variables you should pay attention to (e.g. `n` and `price` via `summarise()`)
  # group_by and summarize go hand in hand together, don't forget to group_by if summarising on certain variables
  # note also how `select` in this example only gives us problems if placed after a line that removes one of the variables
  # diamonds %>%
  #   select(carat, cut, color, clarity, price) %>%
  #   group_by(color) %>%
  #   summarise(n = n(), price = mean(price)) %>%
  #   arrange(desc(color))

  # example 2 (order matters)
  # Unravel: the reorder of lines can reveal how sometimes verbs depends on order, e.g. `filter` has to follow `groupby` here.
  # when `filter` line is placed before `group_by`, the summary box is grey indicating no change since we are now operating on
  # the whole dataframe; we always have more than 1 row in the dataframe.
  # starwars %>%
  #   group_by(species) %>%
  #   filter(n() > 1) %>%
  #   summarise(across(c(sex, gender, homeworld), ~ length(unique(.x))))

  # example 3 (subtle function behavior -- grouping row)
  # Unravel: by calling this out via blue summary box and blue row column on output, rowwise becomes clearer.
  # toggling the `rowwise` line on/off can help highlight `rowwise` effect on behavior in this example
  # where it will properly calculate mean scores of test1, test2 across the row of each student instead of col-wise.
  # student_grades %>%
  #   rowwise() %>%
  #   mutate(avg_scores = mean(c(test1, test2)))
  #> # A tibble: 3 x 4
  #> # Rowwise:
  #>   student_id test1 test2 avg_scores
  #>        <int> <int> <int>      <dbl>
  #> 1          1     1     4        2.5
  #> 2          2     2     5        3.5
  #> 3          3     3     6        4.5
  # student_grades %>%
  #   rowwise() %>%
  #   mutate(avg_scores = mean(c(test1, test2)))

  # example 4 (subtle function behavior -- group dropping)
  # example question (https://community.rstudio.com/t/understanding-group-by-order-matters/22685)
  # Unravel: the data prompt + summary box color + toggle off on line 2 can point out how `summarise` drops the last grouping variable (`gear`)
  # but keeps the rest (`cyl`) for anything downstream
  # mtcars %>%
  #   group_by(cyl, gear) %>%
  #   summarise(mean_mpg = mean(mpg))

  # example 5 (NAs will bite you)
  # Unravel: the toggle off on `drop_na` can highlight the importance of dropping NAs for columns which you operate on for
  # other verbs like group_by/summarise (`mean` by default will not handle NAs)
  # starwars %>%
  #   drop_na(hair_color, mass) %>% #
  #   group_by(hair_color) %>%
  #   summarise(mass_mean = mean(mass))

  # example 6 (important but easy to forget steps --- ungroup)
  # real example: https://twitter.com/aosmith16/status/1369689345335070732
  # Unravel: the tool's summary box color is also useful in internal changes like rowwise(), and with the
  # data prompt summary pointing out we're still grouped by rows + toggle off on `rowwise`, it can point
  # out how rowwise affects subsequent verbs, until you do an explicit `ungroup`
  # mtcars %>%
  #   rowwise() %>%
  #   mutate(mymean = mean(c(cyl, mpg))) %>%
  #   ungroup() %>% # toggle this off and notice how `select` keeps rows grouped
  #   select(cyl, mpg, mymean)

  # example 7 (general function behavior discovery --- group_by overrides previous groups )
  # Unravel: the blue summary box + blue column on output reveals certain api behavior that the dimension numbers or code callouts
  # alone might not help.
  # For e.g. `group_by` overrides the previous grouped variables but this is only apparent when we see that for each case, the
  # data is changed internally, and the output shows a different blue column being used as the grouped variable.
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
