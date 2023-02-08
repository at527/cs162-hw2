fun aux_max with l, max_val =
    if (isnil l) then
        max_val
    else
        if (!l > max_val) then
            aux_max (#l) (!l)
        else
            aux_max (#l) (max_val)
in
fun max_val with l = 
    if (isnil l) then 
        (0 @ 0 @ Nil)
    else 
        (1 @ (aux_max (#l) (!l)) @ Nil)
in
fun largest_elems with l =
    if (isnil l) then
        Nil
    else
        let m = (max_val !l) in
        if (!m = 0) then
            largest_elems (#l)
        else 
            !(#m) @ (largest_elems (#l))
in
largest_elems