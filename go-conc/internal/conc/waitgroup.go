package conc

import "github.com/sourcegraph/conc"

type WaitGroup interface {
	Go(func())
}

func RunWithWaitGroup[T any](f func(wg WaitGroup) T) T {
	var wg conc.WaitGroup
	defer wg.Wait()
	return f(&wg)
}
