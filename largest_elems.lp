fun aux_max with list max_val =
    if isnil list then
        max_val
    else
        let head = !list
        if head > max_val then
            aux_max #list head
        else
            aux_max #list max_val
in

fun max with list = 
    if isnil list then 
        Nil
    else 
        aux_max #list !list
in

fun largest_elems with l =
    if isnil l then
        Nil
    else
        (max !l) @ largest_elems #l
in
largest_elems