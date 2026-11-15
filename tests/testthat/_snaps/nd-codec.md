# nD integer and double token grammars reject malformed values

    Code
      .nd_io_parse_numeric("01", "integer", "point_index")
    Condition
      Error:
      ! Invalid integer token in field `point_index` at row 1.
