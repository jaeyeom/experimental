// clock implements classic bell clock that rings the bell every hour and half hour.
package clock

import (
	"log"
	"time"
)

// Clock just stores a BellHandler.
type Clock struct {
	BellHandler func(times int) error
}

// onTick is called every minute.
func (c Clock) onTick() error {
	hour := time.Now().Hour()
	minute := time.Now().Minute()
	if hour < 7 || hour >= 22 {
		// Quiet at night
		return nil
	}
	if minute == 0 {
		times := (hour + 11) % 12 + 1
		return c.BellHandler(times)
	} else if minute == 30 {
		return c.BellHandler(1)
	}
	return nil
}

// Serve searves a classic bell clock.
func (c Clock) Serve() error {
	nextTick := time.Now()
	log.Println("Current time: ", nextTick)
	nextTick = nextTick.Add(
		time.Minute - time.Duration(
			int64(nextTick.Second())*int64(1000000000)+
				int64(nextTick.Nanosecond())))
	log.Println("Next tick is at: ", nextTick)
	sleepDuration := nextTick.Sub(time.Now())
	log.Println("Sleeping for:", sleepDuration)
	time.Sleep(sleepDuration)
	ticker := time.NewTicker(time.Minute)
	log.Println("Minute ticker is created")
	if err := c.onTick(); err != nil {
		return err
	}
	for _ = range ticker.C {
		if err := c.onTick(); err != nil {
			return err
		}
	}
	return nil
}

