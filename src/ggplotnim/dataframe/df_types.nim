import tables, sets
import column, value

type
  DataFrameKind* = enum
    dfNormal, dfGrouped

  # where value is as usual
  # then
  DataFrame* = ref object
    len*: int
    data*: OrderedTable[string, Column]
    case kind*: DataFrameKind
    of dfGrouped:
      # a grouped data frame stores the keys of the groups and maps them to
      # a set of the categories
      groupMap*: OrderedTable[string, HashSet[Value]]
    else: discard
