package conc

import "github.com/sourcegraph/conc"

type WaitGroup[T any] struct {
	conc.WaitGroup
}

func (h *WaitGroup[T]) Use(f func(*WaitGroup[T]) T) T {
	defer h.Wait()
	return f(h)
}
