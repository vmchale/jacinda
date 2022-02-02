# https://stackoverflow.com/a/4673336

/(^[A-Za-z][A-Za-z0-9\-]*\-[0-9]+(\.[0-9]+)*)\-[0-9a-zA-Z]{22}$/ { 
    !res[$0]++ 
} 
END { 
    for (str in res) {
        print str
    }
}
