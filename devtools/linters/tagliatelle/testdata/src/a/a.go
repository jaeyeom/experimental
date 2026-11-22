package a

type Good struct {
	FirstName string `json:"firstName" yaml:"first_name"`
}

type Bad struct {
	FirstName string `json:"first_name"` // want "json\\(camel\\): got 'first_name' want 'firstName'"
	LastName  string `yaml:"lastName"`   // want "yaml\\(snake\\): got 'lastName' want 'last_name'"
}
