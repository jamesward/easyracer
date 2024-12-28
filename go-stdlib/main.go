package main

import (
	"context"
	"errors"
	"fmt"
	"github.com/google/uuid"
	"github.com/struCoder/pidusage"
	"io"
	"net/http"
	"os"
	"sync"
	"syscall"
	"time"
)

func httpText(url string, ctx context.Context) (string, error) {
	req, newReqErr := http.NewRequestWithContext(ctx, "GET", url, nil)
	if newReqErr != nil {
		return "", newReqErr
	}

	resp, reqErr := http.DefaultClient.Do(req)
	if reqErr != nil {
		return "", reqErr
	}

	if resp.StatusCode < 200 || resp.StatusCode >= 300 {
		return "", errors.New("non-2xx HTTP response")
	}

	defer resp.Body.Close()
	body, readErr := io.ReadAll(resp.Body)
	if readErr != nil {
		return "", readErr
	}

	return string(body), nil
}

// Note that code is intentionally NOT shared across different scenarios

func scenario1(scenarioURL func(int) string) string {
	url := scenarioURL(1)
	result := make(chan string)
	var wg sync.WaitGroup
	defer wg.Wait()
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	httpTextToChannel := func() {
		defer wg.Done()
		text, err := httpText(url, ctx)
		if err == nil {
			result <- text
		}
	}

	wg.Add(2)
	go httpTextToChannel()
	go httpTextToChannel()

	return <-result
}

func scenario2(scenarioURL func(int) string) string {
	url := scenarioURL(2)
	result := make(chan string)
	var wg sync.WaitGroup
	defer wg.Wait()
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	httpTextToChannel := func() {
		defer wg.Done()
		text, err := httpText(url, ctx)
		if err == nil {
			result <- text
		}
	}

	wg.Add(2)
	go httpTextToChannel()
	go httpTextToChannel()

	return <-result
}

func scenario3(scenarioURL func(int) string) string {
	url := scenarioURL(3)
	result := make(chan string)
	var wg sync.WaitGroup
	defer wg.Wait()
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	for i := 1; i <= 10_000; i++ {
		wg.Add(1)
		// On certain (macOS?) machines, creating 100+ concurrent connections at a time
		// results in connections being dropped due to "Connection reset by peer" error.
		//
		// If you are running on such a machine, uncomment the following line:
		//time.Sleep(500 * time.Microsecond)
		go func() {
			defer wg.Done()
			text, err := httpText(url, ctx)
			if err != nil {
				// Connection reset by peer, occurs when connections are created too quickly
				syscallErr := &os.SyscallError{}
				isSyscallErr := errors.As(err, &syscallErr)
				if isSyscallErr && syscallErr.Syscall == "read" && syscallErr.Err == syscall.Errno(0x36) {
					panic(err)
				}

				return
			}

			result <- text
		}()
	}

	return <-result
}

func scenario4(scenarioURL func(int) string) string {
	url := scenarioURL(4)
	result := make(chan string)
	var wg sync.WaitGroup
	defer wg.Wait()
	timeoutCtx, timeoutCancel := context.WithTimeout(context.Background(), 1*time.Second)
	defer timeoutCancel()
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	httpTextToChannel := func(ctx context.Context) {
		defer wg.Done()
		text, err := httpText(url, ctx)
		if err == nil {
			result <- text
		}
	}

	wg.Add(2)
	go httpTextToChannel(timeoutCtx)
	go httpTextToChannel(ctx)

	return <-result
}

func scenario5(scenarioURL func(int) string) string {
	url := scenarioURL(5)
	result := make(chan string)
	var wg sync.WaitGroup
	defer wg.Wait()
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	httpTextToChannel := func() {
		defer wg.Done()
		text, err := httpText(url, ctx)
		if err == nil {
			result <- text
		}
	}

	wg.Add(2)
	go httpTextToChannel()
	go httpTextToChannel()

	return <-result
}

func scenario6(scenarioURL func(int) string) string {
	url := scenarioURL(6)
	result := make(chan string)
	var wg sync.WaitGroup
	defer wg.Wait()
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	httpTextToChannel := func() {
		defer wg.Done()
		text, err := httpText(url, ctx)
		if err == nil {
			result <- text
		}
	}

	wg.Add(3)
	go httpTextToChannel()
	go httpTextToChannel()
	go httpTextToChannel()

	return <-result
}

func scenario7(scenarioURL func(int) string) string {
	url := scenarioURL(7)
	result := make(chan string)
	var wg sync.WaitGroup
	defer wg.Wait()
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	httpTextToChannel := func() {
		defer wg.Done()
		text, err := httpText(url, ctx)
		if err == nil {
			result <- text
		}
	}

	wg.Add(2)
	go httpTextToChannel()
	time.Sleep(3 * time.Second)
	go httpTextToChannel()

	return <-result
}

func scenario8(scenarioURL func(int) string) string {
	url := scenarioURL(8)
	result := make(chan string)
	var wg sync.WaitGroup
	defer wg.Wait()
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	openUseAndCloseToChannel := func() {
		defer wg.Done()
		openURL := fmt.Sprintf("%s?open", url)
		resourceID, openErr := httpText(openURL, ctx)
		if openErr != nil {
			return
		}

		defer func() {
			closeURL := fmt.Sprintf("%s?close=%s", url, resourceID)
			_, _ = httpText(closeURL, ctx)
		}()

		useURL := fmt.Sprintf("%s?use=%s", url, resourceID)
		text, useErr := httpText(useURL, ctx)
		if useErr != nil {
			return
		}

		result <- text
	}
	wg.Add(2)
	go openUseAndCloseToChannel()
	go openUseAndCloseToChannel()

	return <-result
}

func scenario9(scenarioURL func(int) string) string {
	url := scenarioURL(9)
	result := make(chan string)
	var wg sync.WaitGroup
	defer wg.Wait()
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	httpTextToChannel := func() {
		defer wg.Done()
		text, err := httpText(url, ctx)
		if err == nil {
			result <- text
		}
	}

	for i := 1; i <= 10; i++ {
		wg.Add(1)
		go httpTextToChannel()
	}

	text := ""
	for i := 1; i <= 5; i++ {
		text += <-result
	}
	return text
}

func scenario10(scenarioURL func(int) string) string {
	url := scenarioURL(10)
	id := uuid.New().String()

	var wg sync.WaitGroup
	defer wg.Wait()
	ctx, cancel := context.WithCancel(context.Background())
	wg.Add(1)
	go func(ctx context.Context) {
		defer wg.Done()
		if ctx != nil {
			for ctx.Err() == nil {
			}
		}
	}(ctx)
	wg.Add(1)
	go func() {
		defer wg.Done()
		blockerURL := fmt.Sprintf("%s?%s", url, id)
		_, err := httpText(blockerURL, ctx)
		if err == nil {
			cancel()
		}
	}()

	var reporter func() (string, error)
	reporter = func() (string, error) {
		sysInfo, getStatErr := pidusage.GetStat(os.Getpid())
		if getStatErr != nil {
			return "", getStatErr
		}

		url := fmt.Sprintf("%s?%s=%.3f", url, id, sysInfo.CPU/100.0)

		req, newReqErr := http.NewRequest("GET", url, nil)
		if newReqErr != nil {
			return "", newReqErr
		}

		resp, reqErr := http.DefaultClient.Do(req)
		if reqErr != nil {
			return "", reqErr
		}

		if resp.StatusCode < 200 || resp.StatusCode >= 400 {
			return "", errors.New("non-2xx/3xx HTTP response")
		}

		if resp.StatusCode >= 300 {
			time.Sleep(1 * time.Second)
			return reporter()
		}

		defer resp.Body.Close()
		body, readErr := io.ReadAll(resp.Body)
		if readErr != nil {
			return "", readErr
		}

		return string(body), nil
	}

	result, err := reporter()

	if err != nil {
		return fmt.Sprintf("error in reporter: %v", err)
	}

	return result
}

func scenario11(scenarioURL func(int) string) string {
	url := scenarioURL(11)
	result := make(chan string)
	var wg sync.WaitGroup
	defer wg.Wait()
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	httpTextToChannel := func(wg *sync.WaitGroup, ctx context.Context) {
		defer wg.Done()
		text, err := httpText(url, ctx)
		if err == nil {
			result <- text
		}
	}
	innerGroup := func(wg *sync.WaitGroup) {
		defer wg.Done()
		innerCtx, innerCancel := context.WithCancel(context.Background())
		defer innerCancel()
		var innerWg sync.WaitGroup
		defer innerWg.Wait()

		innerWg.Add(2)
		go httpTextToChannel(&innerWg, innerCtx)
		go httpTextToChannel(&innerWg, innerCtx)
	}

	wg.Add(2)
	go innerGroup(&wg)
	go httpTextToChannel(&wg, ctx)

	return <-result
}

var scenarios = []func(func(int) string) string{
	scenario1, scenario2, scenario3, scenario4, scenario5, scenario6, scenario7, scenario8, scenario9, scenario10, scenario11,
}

func main() {
	scenarioURL := func(scenario int) string {
		return fmt.Sprintf("http://localhost:8080/%d", scenario)
	}
	for _, scenario := range scenarios {
		fmt.Println(scenario(scenarioURL))
	}
}
