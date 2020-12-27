import macros
import recipeFiles

macro importAll(): untyped =
  result = newStmtList()
  for r in RecipeFiles:
    result.add quote do:
      import `r`

importAll()
