using DotNet.Testcontainers.Builders;
using DotNet.Testcontainers.Containers;

namespace EasyRacer.Tests;

public class TestcontainersFixture : IDisposable
{
    private const string ContainerImage = "ghcr.io/jamesward/easyracer";
    private const int PortBinding = 8080;

    public HttpClient HttpClient { get; private set; }

    public int Port { get; private set; }
    
    private readonly IContainer container;

    public TestcontainersFixture()
    {
        container = new ContainerBuilder()
            .WithImage(ContainerImage)
            // Bind port 8080 of the container to a random port on the host.
            .WithPortBinding(PortBinding, true)
            // Wait until the HTTP endpoint of the container is available.
            .WithWaitStrategy(Wait.ForUnixContainer()
                .UntilHttpRequestIsSucceeded(r => r.ForPort(PortBinding)))
            // Build the container configuration.
            .Build();

        // Start the container.
        container.StartAsync().Wait();

        // Get the port of the container.
        Port = container.GetMappedPublicPort(PortBinding);

        // Create a new instance of HttpClient to send HTTP requests.
        HttpClient = new HttpClient();
    }    

    public void Dispose()
    {
        container.StopAsync().Wait();
    }        
}