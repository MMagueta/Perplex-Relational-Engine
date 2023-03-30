signature DICTIONARY =
sig
    type (''key, 'value) dict

    val empty: (''key, 'value) dict
    val member: ''key -> (''key, 'value) dict -> bool
    val insert: (''key * 'value) -> (''key, 'value) dict -> (''key, 'value) dict
    val fromList: (''key * 'value) list -> (''key, 'value) dict
end

structure Dictionary :> DICTIONARY =
struct
    type (''key, 'value) dict = (''key * 'value) list

    val empty = []

    fun member key [] = false
    | member key ((key2, _)::dict) =
        key = key2 orelse member key dict

    fun insert (key, value) [] = [(key, value)]
    | insert (key, value) ((key2, value2)::dict) =
        if key = key2
        then (key, value) :: dict
        else (key2, value2) :: insert (key, value) dict

    fun fromList pairs = foldl (fn (pair, dict) => insert pair dict) empty pairs
end