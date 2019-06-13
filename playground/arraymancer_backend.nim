import arraymancer
import ggplotnim

#[
In principle we could support an arraymancer backend. By changing the `DataFrame`
type as:
]#
type
  DataFrameAr* = object
    len*: int
    columns*: Table[string, int] # table mapping the column key to an integer
    data*: Tensor[Value]
    case kind: DataFrameKind
    of dfGrouped:
      # a grouped data frame stores the keys of the groups and maps them to
      # a set of the categories
      groupMap: OrderedTable[string, HashSet[Value]]
    else: discard
#[
And providing overloads for the access of the data frame this should work fine
]#

proc `[]`(df: DataFrameAr, key: string): Tensor[Value] =
  ## returns a column of the data frame
  ## Or maybe this should just return a single column data frame
  result = df.data[df.columns[key], _]

#[
The `DataFrame` tensor should probably be `column` major, so that accessing a column
is quick. For most operations looking at the whole column is required. Row access is
not needed so much?
]#
