include Either

let value_map ~left ~right =
  function
  | Left v -> Left (left v)
  | Right v -> Right(right v)