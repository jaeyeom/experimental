package p

import erralias "errors"

// errAliased keeps an allowlisted call imported under another name.
var errAliased = erralias.New("y")
