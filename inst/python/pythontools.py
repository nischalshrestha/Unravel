"""
Small module to get the last expression value of Python code from R.
"""

def get_last_value(code):
  try:
    import ast
    import astunparse
    # get ast 
    tree = ast.parse(code)
    # get last Expr object
    expressions = [n for n in ast.walk(tree) if isinstance(n, ast.Expr)]
    if len(expressions) == 0:
      return ""
    else:
      last_expr = expressions[-1]
    # first exec everything to introduce objects to global python environment
    exec(code)
    # then try eval on the last expression
    if last_expr is not None:
      out = eval(astunparse.unparse(last_expr).strip())
      return out
  except Exception as e:
    return None

# code = \
# """
# import pandas as pd
# import numpy as np
# arrays = [np.array(['bar', 'bar', 'baz', 'baz', 'foo', 'foo', 'qux', 'qux']),
#         np.array(['one', 'two', 'one', 'two', 'one', 'two', 'one', 'two'])]
# tuples = list(zip(*arrays))
# index = pd.MultiIndex.from_tuples(tuples, names=['first', 'second'])
# df_basic = pd.DataFrame(np.random.randn(8, 4), columns=['a','b','c','d'])
# 
# df_basic
# x = 'hello' # statement should be ignored
# """

# testing, we should get df_basic back
# get_last_value(code)



