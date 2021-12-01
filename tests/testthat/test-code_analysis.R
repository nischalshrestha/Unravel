require(tidyverse)
require(DataTutor)

test_that("One liner functions", {
  # only dataframe
  expect_equal(
    get_output_intermediates(quote(diamonds)),
    list(
      list(
        line = 1,
        code = "diamonds",
        change = "none",
        output = diamonds,
        row = 53940,
        col = 10,
        summary = "<strong>Summary:</strong> tibble with <span class='number'>53,940</span> rows and <span class='number'>10</span> columns"
      )
    )
  )

  # invalid isolated function
  expect_equal(
    get_output_intermediates(quote(mean(1:2))),
    list(
      list(
        line = 1,
        code = "mean(1:2)",
        change = "error",
        summary = "<strong>Summary:</strong> Your code does not use functions that take in a dataframe."
      )
    )
  )

  # single verb
  expect_equal(
    get_output_intermediates(quote(select(diamonds, carat, cut, color, clarity, price))),
    list(
      list(
        line = 1,
        code = "select(diamonds, carat, cut, color, clarity, price)",
        change = "visible",
        output = select(diamonds, carat, cut, color, clarity, price),
        row = 53940,
        col = 5,
        callouts = list(),
        summary = "<strong>Summary:</strong> tibble with <span class='number'>53,940</span> rows and <span class='number'>5</span> columns"
      )
    )
  )
})


test_that("Multiple functions", {
  pipeline <- quote(
    diamonds %>%
      select(carat, cut, color, clarity, price) %>%
      group_by(color) %>%
      summarise(n = n(), price = mean(price)) %>%
      arrange(desc(color))
  )
  expect_equal(
    get_output_intermediates(pipeline),
    list(
      list(
        line = 1,
        code = "diamonds %>%",
        change = "none",
        output = diamonds,
        row = 53940,
        col = 10,
        callouts = NULL,
        summary = "<strong>Summary:</strong> tibble with <span class='number'>53,940</span> rows and <span class='number'>10</span> columns"
      ),
      list(
        line = 2,
        code = "\tselect(carat, cut, color, clarity, price) %>%",
        change = "visible",
        output = diamonds %>% select(carat, cut, color, clarity, price),
        row = 53940,
        col = 5,
        callouts = list(),
        summary = "<strong>Summary:</strong> <code class='code'>select</code> changed the dataframe shape from <span class = 'number'>[53940 x 10]</span> to <span class = 'visible-change number'>[53940 x 5]</span>. <code class='code'>select</code> dropped <span class='number'>5</span> variables (<code class='code'>depth</code>, <code class='code'>table</code>, <code class='code'>x</code>, <code class='code'>y</code>, <code class='code'>z</code>)."
      ),
      list(
        line = 3,
        code = "\tgroup_by(color) %>%",
        change = "internal",
        output =
          diamonds %>%
            select(carat, cut, color, clarity, price) %>%
            group_by(color),
        row = 53940,
        col = 5,
        callouts = list(list(word = "color", change = "internal-change")),
        summary = "<strong>Summary:</strong> <code class='code'>group_by</code> has no visible effect on the data. <code class='code'>group_by</code> has internally grouped one variable (<code class='code internal-change'>color</code>) <hr> <div> <i class='far fa-lightbulb'></i> <code class='code'>group_by()</code> doesn't really do anything itself; it just changes how the other verbs work. It now allows us to execute functions like <code class='code'>summarise</code> to perform statistics by groups (<code class='code internal-change'>color</code>). </div>"
      ),
      list(
        line = 4,
        code = "\tsummarise(n = n(), price = mean(price)) %>%",
        change = "visible",
        output =
          diamonds %>%
            select(carat, cut, color, clarity, price) %>%
            group_by(color) %>%
            summarise(n = n(), price = mean(price)),
        row = 7,
        col = 3,
        callouts = list(list(word = "n", change = "visible-change"), list(word = "price", change = "visible-change")),
        summary = "<strong>Summary:</strong> <code class='code'>summarise</code> changed the dataframe shape from <span class = 'number'>[53940 x 5]</span> to <span class = 'visible-change number'>[7 x 3]</span>. <code class='code'>summarise</code> (working on group variables: <code class='code'>color</code>) created <span class='number'>2</span> variables (<code class='code visible-change'>n</code> via <code class='code'>n()</code>, <code class='code visible-change'>price</code> via <code class='code'>mean(price)</code>). The data is now ungrouped."
      ),
      list(
        line = 5,
        code = "\tarrange(desc(color))",
        change = "visible",
        output =
          diamonds %>%
            select(carat, cut, color, clarity, price) %>%
            group_by(color) %>%
            summarise(n = n(), price = mean(price)) %>%
            arrange(desc(color)),
        row = 7,
        col = 3,
        callouts = list(list(word = "color", change = "visible-change")),
        summary = "<strong>Summary:</strong> <code class='code'>arrange</code> does not change the data shape. <code class='code'>arrange</code> sorted the data by one variable (<code class='code visible-change'>color</code> by descending order)."
      )
    )
  )
})

test_that("Evaluation is deterministic", {
  pipeline <- quote(
    data.frame(replicate(5, sample(0:1, 5, rep = TRUE))) %>%
      select(everything()) %>%
      group_by(X1)
  )
  outputs <- get_output_intermediates(pipeline)
  first_output <- outputs[[1]]$output
  expect_equal(
    outputs,
    list(
      list(
        line = 1,
        code = "data.frame(replicate(5, sample(0:1, 5, rep = TRUE))) %>%",
        change = "none",
        output = first_output,
        row = 5,
        col = 5,
        callouts = NULL,
        summary = "<strong>Summary:</strong> data.frame with <span class='number'>5</span> rows and <span class='number'>5</span> columns"
      ),
      list(
        line = 2,
        code = "\tselect(everything()) %>%",
        change = "none",
        output =
          first_output %>%
            select(everything()),
        row = 5,
        col = 5,
        callouts = list(),
        summary = "<strong>Summary:</strong>  <code class='code'>select</code> resulted in no changes."
      ),
      list(
        line = 3,
        code = "\tgroup_by(X1)",
        change = "internal",
        output =
          first_output %>%
            select(everything()) %>%
            group_by(X1),
        row = 5,
        col = 5,
        callouts = list(list(word = "X1", change = "internal-change")),
        summary = "<strong>Summary:</strong> <code class='code'>group_by</code> has no visible effect on the data. <code class='code'>group_by</code> has internally grouped one variable (<code class='code internal-change'>X1</code>) <hr> <div> <i class='far fa-lightbulb'></i> <code class='code'>group_by()</code> doesn't really do anything itself; it just changes how the other verbs work. It now allows us to execute functions like <code class='code'>summarise</code> to perform statistics by groups (<code class='code internal-change'>X1</code>). </div>"
      )
    )
  )
})

test_that("Properly analyzes problematic code", {
  pipeline <- quote(
    diamonds %>%
      mean() %>%
      select(carat, cut, color, clarity, price)
  )
  # suppress the warning for the testthat output but expect the warning is produced
  suppressWarnings({
    expect_warning(get_output_intermediates(pipeline), "argument is not numeric or logical: returning NA")
    outputs <- get_output_intermediates(pipeline)
  })
  # succesful line with dataframe
  expect_equal(
    outputs[[1]],
    list(
      line = 1,
      code = "diamonds %>%",
      change = "none",
      output = diamonds,
      row = 53940,
      col = 10,
      callouts = NULL,
      summary = "<strong>Summary:</strong> tibble with <span class='number'>53,940</span> rows and <span class='number'>10</span> columns"
    )
  )
  # culprit line with error
  expect_equal(
    Filter(function(x) !is.null(x), outputs[[2]][c("line", "code", "change", "callout", "summary")]),
    list(
      line = 2,
      code = "\tmean() %>%",
      change = "error",
      summary = "<strong>Summary:</strong> This step produced an `NA`."
    )
  )
  expect_true(is.na(outputs[[2]]$output))
  # subsequent line that can't run because of the previous line with error
  expect_equal(
    outputs[[3]],
    list(
      line = 3,
      code = "\tselect(carat, cut, color, clarity, price)",
      change = "error",
      summary = "<strong>Summary:</strong> Previous lines have problems!"
    )
  )
})

test_that("Data pronouns can be accessed", {
  pipeline <- quote(
    mtcars %>%
      split(.$cyl)
  )
  expect_equal(
    get_output_intermediates(pipeline),
    list(
      list(
        line = 1,
        code = "mtcars %>%",
        change = "none",
        output = mtcars,
        row = 32,
        col = 11,
        callouts = NULL,
        summary = "<strong>Summary:</strong> data.frame with <span class='number'>32</span> rows and <span class='number'>11</span> columns"
      ),
      list(
        line = 2,
        code = "\tsplit(.$cyl)",
        change = "visible",
        output = mtcars %>% split(.$cyl),
        callouts = NULL,
        summary = ""
      )
    )
  )

  # one with .data pronoun should work as well
  pipeline <- quote(
    mtcars %>%
      names() %>%
      map(~ count(mtcars, .data[[.x]]))
  )
  # NOTE: `test_that` uses an `rlang::enquo()` underneath the hood which seems
  # to fail to capture expressions that reference params of lambda like `.x`
  # so, we are capture the output of that to compare the last line here
  last_output <- mtcars %>%
    names() %>%
    map(~ count(mtcars, .data[[.x]]))
  expect_equal(
    get_output_intermediates(pipeline),
    list(
      list(
        line = 1,
        code = "mtcars %>%",
        change = "none",
        output = mtcars,
        row = 32,
        col = 11,
        callouts = NULL,
        summary = "<strong>Summary:</strong> data.frame with <span class='number'>32</span> rows and <span class='number'>11</span> columns"
      ),
      list(
        line = 2,
        code = "\tnames() %>%",
        change = "visible",
        output = mtcars %>% names(),
        callouts = NULL,
        summary = ""
      ),
      list(
        line = 3,
        code = "\tmap(~count(mtcars, .data[[.x]]))",
        change = "visible",
        output = last_output,
        callouts = list(),
        summary = "<strong>Summary:</strong> <code class='code'>count</code> changed the dataframe shape from <span class = 'number'>[32 x 11]</span> to <span class = 'visible-change number'>[6 x 2]</span>. The data is now ungrouped."
      )
    )
  )
})

test_that("Nested expressions can be unraveled", {
    across_expr <- quote(
      iris %>%
        as_tibble() %>%
        mutate(across(c(Sepal.Length, Sepal.Width), round))
    )

    expected_outputs <- list(
      iris,
      iris %>% as_tibble(),
      iris %>% as_tibble() %>% mutate(across(c(Sepal.Length, Sepal.Width), round))
    )

    outputs <- get_output_intermediates(across_expr)

    expect_equal(
      outputs,
      list(
        list(
          line = 1,
          code = "iris %>%",
          change = "none",
          output = expected_outputs[[1]],
          row = 150,
          col = 5,
          callouts = NULL,
          summary = "<strong>Summary:</strong> data.frame with <span class='number'>150</span> rows and <span class='number'>5</span> columns"
        ),
        list(
          line = 2,
          code = "\tas_tibble() %>%",
          change = "none",
          output = expected_outputs[[2]],
          row = 150,
          col = 5,
          callouts = NULL,
          summary = ""
        ),
        list(
          line = 3,
          code = "\tmutate(across(c(Sepal.Length, Sepal.Width), round))",
          change = "visible",
          output = expected_outputs[[3]],
          row = 150,
          col = 5,
          callouts = list(
            list(word = "Sepal.Length", change = "visible-change"),
            list(word = "Sepal.Width", change = "visible-change")
          ),
          summary = "<strong>Summary:</strong>  <code class='code'>mutate</code> changed <span class='number'>133</span> values (<span class='number'>89%</span>) of '<code class='code visible-change'>Sepal.Length</code>' (previously <span class='number'>0</span> <span class='number'>NA</span>s, now <span class='number'>0</span> new <span class='number'>NA</span>s); changed <span class='number'>122</span> values (<span class='number'>81%</span>) of '<code class='code visible-change'>Sepal.Width</code>' (previously <span class='number'>0</span> <span class='number'>NA</span>s, now <span class='number'>0</span> new <span class='number'>NA</span>s)."
        )
      )
    )
  })

