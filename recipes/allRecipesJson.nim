import macros
import recipeFiles

macro importAllJson(): untyped =
  result = newStmtList()
  for r in RecipeFiles:
    let rJson = r & "_json"
    result.add quote do:
      import `rJson`

importAllJson()
