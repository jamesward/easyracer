package main

import (
	"context"
	"fmt"
	"github.com/testcontainers/testcontainers-go"
	"github.com/testcontainers/testcontainers-go/wait"
	"testing"
)

func TestScenarios(t *testing.T) {
	ctx := context.Background()
	req := testcontainers.ContainerRequest{
		Image:        "ghcr.io/jamesward/easyracer",
		ExposedPorts: []string{"8080/tcp"},
		WaitingFor:   wait.ForHTTP("/").WithPort("8080"),
	}

	scenarioServer, containerErr := testcontainers.GenericContainer(
		ctx,
		testcontainers.GenericContainerRequest{
			ContainerRequest: req,
			Started:          true,
		},
	)
	if containerErr != nil {
		t.Error(containerErr)
	}
	defer func() {
		if err := scenarioServer.Terminate(ctx); err != nil {
			t.Fatalf("failed to terminate container: %s", err.Error())
		}
	}()

	baseURL, endpointErr := scenarioServer.Endpoint(ctx, "http")
	if endpointErr != nil {
		t.Error(endpointErr)
	}

	scenarioURL := func(scenario int) string {
		return fmt.Sprintf("%s/%d", baseURL, scenario)
	}
	scenarios := []func(func(int) string) string{
		scenario1,
		scenario2,
		scenario3,
		scenario4,
		scenario5,
		scenario6,
		scenario7,
		scenario8,
		scenario9,
	}

	for idx, scenario := range scenarios {
		name := fmt.Sprintf("scenario%d", idx+1)
		t.Run(name, func(t *testing.T) {
			got := scenario(scenarioURL)
			if got != "right" {
				t.Errorf(`%s = "%s"; want "right"`, name, got)
			}
		})
	}
}
