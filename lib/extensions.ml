module Option = struct
  let ( let+ ) = Option.bind
end

module Result = struct
  let ( let+ ) = Result.bind
end
