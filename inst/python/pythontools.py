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
    # then try eval on the last expression
    if last_expr is not None:
      return astunparse.unparse(last_expr).strip()
  except Exception as e:
    return None
