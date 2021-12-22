
test_that("Styling tidyverse code works", {
  symbol_args <- quote(
    starwars %>%
      select(name, height, mass, hair_color, skin_color, eye_color, birth_year,
             sex, gender, homeworld, species, films, vehicles
    )
  )
  expect_equal(
    style_dplyr_code(symbol_args),
    "starwars %>%\n\tselect(\n\t\tname, height, mass, hair_color, skin_color, eye_color,\n\t\tbirth_year, sex, gender, homeworld, species, films, vehicles\n\t)"
  )

  expr_args <- quote(
    starwars %>%
      drop_na(birth_year) %>%
      group_by(species) %>%
      summarise(
        across(c(sex, gender, homeworld), ~ length(unique(.x))),
        across(birth_year, ~ mean(.x, na.rm = TRUE)),
        across(birth_year, ~ mean(.x, na.rm = TRUE))
      )
  )
  expect_equal(
    style_dplyr_code(expr_args),
    "starwars %>%\n\tdrop_na(birth_year) %>%\n\tgroup_by(species) %>%\n\tsummarise(\n\t\tacross(c(sex, gender, homeworld), ~length(unique(.x))),\n\t\tacross(birth_year, ~mean(.x, na.rm = TRUE)),\n\t\tacross(birth_year, ~mean(.x, na.rm = TRUE))\n\t)"
  )

  mixed_expr_args <- quote(
    starwars %>%
      drop_na(birth_year) %>%
      group_by(species) %>%
      summarise(across(c(sex, gender, homeworld), ~ length(unique(.x))), birth_year_avg = mean(birth_year, na.rm = TRUE))
  )
  expect_equal(
    style_dplyr_code(mixed_expr_args),
    "starwars %>%\n\tdrop_na(birth_year) %>%\n\tgroup_by(species) %>%\n\tsummarise(\n\t\tacross(c(sex, gender, homeworld), ~length(unique(.x))),\n\t\tbirth_year_avg = mean(birth_year, na.rm = TRUE)\n\t)"
  )

})
