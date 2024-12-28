package main

import (
	"context"
	"errors"
	"fmt"
	"github.com/google/uuid"
	"github.com/jamesward/easyracer/go-conc/internal/conc"
	"github.com/struCoder/pidusage"
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

	return conc.RunWithWaitGroup(func(wg conc.WaitGroup) string {
		result := make(chan string)
		ctx, cancel := context.WithCancel(context.Background())
		defer cancel()
		httpTextToChannel := func() {
			text, err := httpText(url, ctx)
			if err == nil {
				result <- text
			}
		}

		wg.Go(httpTextToChannel)
		wg.Go(httpTextToChannel)

		return <-result
	})
}

func scenario2(scenarioURL func(int) string) string {
	url := scenarioURL(2)

	return conc.RunWithWaitGroup(func(wg conc.WaitGroup) string {
		result := make(chan string)
		ctx, cancel := context.WithCancel(context.Background())
		defer cancel()
		httpTextToChannel := func() {
			text, err := httpText(url, ctx)
			if err == nil {
				result <- text
			}
		}

		wg.Go(httpTextToChannel)
		wg.Go(httpTextToChannel)

		return <-result
	})
}

func scenario3(scenarioURL func(int) string) string {
	url := scenarioURL(3)

	return conc.RunWithWaitGroup(func(wg conc.WaitGroup) string {
		result := make(chan string)
		ctx, cancel := context.WithCancel(context.Background())
		defer cancel()

		for i := 1; i <= 10_000; i++ {
			// On certain (macOS?) machines, creating 100+ concurrent connections at a time
			// chs in connections being dropped due to "Connection reset by peer" error.
			//
			// If you are running on suresult a machine, uncomment the following line:
			//time.Sleep(500 * time.Microsecond)
			wg.Go(func() {
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
			})
		}

		return <-result
	})
}

func scenario4(scenarioURL func(int) string) string {
	url := scenarioURL(4)

	return conc.RunWithWaitGroup(func(wg conc.WaitGroup) string {
		result := make(chan string)
		timeoutCtx, timeoutCancel := context.WithTimeout(context.Background(), 1*time.Second)
		defer timeoutCancel()
		ctx, cancel := context.WithCancel(context.Background())
		defer cancel()
		httpTextToChannel := func(ctx context.Context) func() {
			return func() {
				text, err := httpText(url, ctx)
				if err == nil {
					result <- text
				}
			}
		}

		wg.Go(httpTextToChannel(timeoutCtx))
		wg.Go(httpTextToChannel(ctx))

		return <-result
	})
}

func scenario5(scenarioURL func(int) string) string {
	url := scenarioURL(5)

	return conc.RunWithWaitGroup(func(wg conc.WaitGroup) string {
		result := make(chan string)
		ctx, cancel := context.WithCancel(context.Background())
		defer cancel()
		httpTextToChannel := func() {
			text, err := httpText(url, ctx)
			if err == nil {
				result <- text
			}
		}

		wg.Go(httpTextToChannel)
		wg.Go(httpTextToChannel)

		return <-result
	})
}

func scenario6(scenarioURL func(int) string) string {
	url := scenarioURL(6)

	return conc.RunWithWaitGroup(func(wg conc.WaitGroup) string {
		result := make(chan string)
		ctx, cancel := context.WithCancel(context.Background())
		defer cancel()
		httpTextToChannel := func() {
			text, err := httpText(url, ctx)
			if err == nil {
				result <- text
			}
		}

		wg.Go(httpTextToChannel)
		wg.Go(httpTextToChannel)
		wg.Go(httpTextToChannel)

		return <-result
	})
}

func scenario7(scenarioURL func(int) string) string {
	url := scenarioURL(7)

	return conc.RunWithWaitGroup(func(wg conc.WaitGroup) string {
		result := make(chan string)
		ctx, cancel := context.WithCancel(context.Background())
		defer cancel()
		httpTextToChannel := func() {
			text, err := httpText(url, ctx)
			if err == nil {
				result <- text
			}
		}

		wg.Go(httpTextToChannel)
		wg.Go(func() {
			time.Sleep(3 * time.Second)
			httpTextToChannel()
		})

		return <-result
	})
}

func scenario8(scenarioURL func(int) string) string {
	url := scenarioURL(8)

	return conc.RunWithWaitGroup(func(wg conc.WaitGroup) string {
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

		wg.Go(openUseAndCloseToChannel)
		wg.Go(openUseAndCloseToChannel)

		return <-result
	})
}

func scenario9(scenarioURL func(int) string) string {
	url := scenarioURL(9)

	return conc.RunWithWaitGroup(func(wg conc.WaitGroup) string {
		result := make(chan string)
		ctx, cancel := context.WithCancel(context.Background())
		defer cancel()
		httpTextToChannel := func() {
			text, err := httpText(url, ctx)
			if err == nil {
				result <- text
			} else {
				result <- ""
			}
		}

		for i := 1; i <= 10; i++ {
			wg.Go(httpTextToChannel)
		}

		text := ""
		for i := 1; i <= 10; i++ {
			text += <-result
		}

		return text
	})
}

func scenario10(scenarioURL func(int) string) string {
	url := scenarioURL(10)
	id := uuid.New().String()

	return conc.RunWithWaitGroup(func(wg conc.WaitGroup) string {
		ctx, cancel := context.WithCancel(context.Background())

		// Blocking
		wg.Go(func() {
			if ctx != nil {
				for ctx.Err() == nil {
				}
			}
		})
		// Blocker
		wg.Go(func() {
			blockerURL := fmt.Sprintf("%s?%s", url, id)
			_, err := httpText(blockerURL, ctx)
			if err == nil {
				cancel()
			}
		})

		// Reporter
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
	})
}

func scenario11(scenarioURL func(int) string) string {
	url := scenarioURL(11)

	return conc.RunWithWaitGroup(func(wg conc.WaitGroup) string {
		result := make(chan string)
		ctx, cancel := context.WithCancel(context.Background())
		defer cancel()
		httpTextToChannel := func() {
			text, err := httpText(url, ctx)
			if err == nil {
				result <- text
			}
		}
		innerGroup := func() {
			conc.RunWithWaitGroup(func(wg conc.WaitGroup) any {
				wg.Go(httpTextToChannel)
				wg.Go(httpTextToChannel)

				return nil
			})
		}

		wg.Go(innerGroup)
		wg.Go(httpTextToChannel)

		return <-result
	})
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
