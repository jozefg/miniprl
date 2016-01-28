structure Guid :> GUID =
struct
  val counter = ref 0
  type t = {id : int, name : string}

  fun compare ({id = id1, name = name1}, {id = id2, name = name2}) =
      Int.compare (id1, id2)
  fun toString {id, name} = name

  fun new () =
    let
      val i = !counter
      val () = counter := !counter + 1
    in
      {id = i, name = ""}
    end

  fun named s = {id = #id (new ()), name = s}
end
