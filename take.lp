fun take with n, l =
    if isnil l
        then Nil
    else (!l @ take n-1 #l)
in
take