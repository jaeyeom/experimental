package a

// Good comment ends with a period.
func Good() {}

// Bad comment has no period // want "Comment should end in a period"
func Bad() {}
