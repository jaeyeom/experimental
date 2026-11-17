package decide

// Human is the author and answeredBy of what the user wrote.
const Human = "human"

// FromHuman reports whether the user wrote m: its author is human and no
// agent sent it from the command line. Via is not checked.
func (m Message) FromHuman() bool { return m.Author == Human && m.By == "" }

// HumanUI reports whether the user wrote m in the inbox TUI or Emacs.
// Use it, with Answer.HumanUI, before acting on an approval: an agent
// can write as the user from the command line.
func (m Message) HumanUI() bool { return m.FromHuman() && m.Via == ViaUI }

// HumanUI reports whether a is an answer the user gave in the inbox TUI
// or Emacs. A nil answer is not.
func (a *Answer) HumanUI() bool { return a != nil && a.AnsweredBy == Human && a.Via == ViaUI }
