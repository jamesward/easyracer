package main

import (
	"context"
	"errors"
	"fmt"
	"io"
	"net/http"
	"os"
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
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	httpTextToChannel := func() {
		text, err := httpText(url, ctx)
		if err == nil {
			result <- text
		}
	}

	go httpTextToChannel()
	go httpTextToChannel()

	return <-result
}

func scenario2(scenarioURL func(int) string) string {
	url := scenarioURL(2)
	result := make(chan string)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	httpTextToChannel := func() {
		text, err := httpText(url, ctx)
		if err == nil {
			result <- text
		}
	}

	go httpTextToChannel()
	go httpTextToChannel()

	return <-result
}

func scenario3(scenarioURL func(int) string) string {
	url := scenarioURL(3)
	result := make(chan string)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	for i := 1; i <= 10_000; i++ {
		// On certain (macOS?) machines, creating 100+ concurrent connections at a time
		// results in connections being dropped due to "Connection reset by peer" error.
		//
		// If you are running on such a machine, uncomment the following line:
		//time.Sleep(500 * time.Microsecond)
		go func() {
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
	timeoutCtx, timeoutCancel := context.WithTimeout(context.Background(), 1*time.Second)
	defer timeoutCancel()
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	httpTextToChannel := func(ctx context.Context) {
		text, err := httpText(url, ctx)
		if err == nil {
			result <- text
		}
	}

	go httpTextToChannel(timeoutCtx)
	go httpTextToChannel(ctx)

	return <-result
}

func scenario5(scenarioURL func(int) string) string {
	url := scenarioURL(5)
	result := make(chan string)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	httpTextToChannel := func() {
		text, err := httpText(url, ctx)
		if err == nil {
			result <- text
		}
	}

	go httpTextToChannel()
	go httpTextToChannel()

	return <-result
}

func scenario6(scenarioURL func(int) string) string {
	url := scenarioURL(6)
	result := make(chan string)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	httpTextToChannel := func() {
		text, err := httpText(url, ctx)
		if err == nil {
			result <- text
		}
	}

	go httpTextToChannel()
	go httpTextToChannel()
	go httpTextToChannel()

	return <-result
}

func scenario7(scenarioURL func(int) string) string {
	url := scenarioURL(7)
	result := make(chan string)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	httpTextToChannel := func() {
		text, err := httpText(url, ctx)
		if err == nil {
			result <- text
		}
	}

	go httpTextToChannel()
	time.Sleep(3 * time.Second)
	go httpTextToChannel()

	return <-result
}

func scenario8(scenarioURL func(int) string) string {
	url := scenarioURL(8)
	result := make(chan string)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	openUseAndCloseToChannel := func() {
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
	go openUseAndCloseToChannel()
	go openUseAndCloseToChannel()

	return <-result
}

func scenario9(scenarioURL func(int) string) string {
	url := scenarioURL(9)
	result := make(chan string)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	httpTextToChannel := func() {
		text, err := httpText(url, ctx)
		if err == nil {
			result <- text
		}
	}

	for i := 1; i <= 10; i++ {
		go httpTextToChannel()
	}

	text := ""
	for i := 1; i <= 5; i++ {
		text += <-result
	}
	return text
}

func scenarios(scenarioURL func(int) string) []string {
	return []string{
		scenario1(scenarioURL),
		scenario2(scenarioURL),
		scenario3(scenarioURL),
		scenario4(scenarioURL),
		scenario5(scenarioURL),
		scenario6(scenarioURL),
		scenario7(scenarioURL),
		scenario8(scenarioURL),
		scenario9(scenarioURL),
	}
}

func main() {
	scenarioURL := func(scenario int) string {
		return fmt.Sprintf("http://localhost:8080/%d", scenario)
	}
	for _, result := range scenarios(scenarioURL) {
		fmt.Println(result)
	}
}
