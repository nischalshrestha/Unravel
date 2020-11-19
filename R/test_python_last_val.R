library(DataTutor)

# code mixed with expressions and statements
"
import pandas as pd
import numpy as np

df_basic = pd.DataFrame(np.random.randn(8, 4), columns=['a','b','c','d'])
df_basic # this should be returned

x = 'hello' # statement should be ignored
" -> code

# introduce to py environment
reticulate::py_run_string(code)
# get last expression string
py_last_expr <- py$get_last_value(code)
py_last_expr
# [1] "df_basic"

# evaluate it
py_last_value <- reticulate::py_eval(py_last_expr, convert = FALSE)
py_last_value
# a         b         c         d
# 0 -1.169177  0.523183  1.372377  0.177704
# 1 -1.739976  0.272666 -0.060806 -0.879833
# 2  0.056298  0.277061 -0.671076  0.145632
# 3 -2.004460  1.616150  0.540406  0.602045
# 4  0.272720 -0.665923  0.071962  1.010777
# 5 -0.765531 -0.260634  0.876470  1.359991
# 6 -0.565503  0.249852 -0.462454  2.783382
# 7  0.584744 -0.595427  0.612433  0.045735

class(py_last_value)
# [1] "pandas.core.frame.DataFrame"        "pandas.core.generic.NDFrame"        "pandas.core.base.PandasObject"
# [4] "pandas.core.accessor.DirNamesMixin" "pandas.core.base.SelectionMixin"    "pandas.core.indexing.IndexingMixin"
# [7] "python.builtin.object"

py$get_last_value("x = 2") # empty
# [1] ""

