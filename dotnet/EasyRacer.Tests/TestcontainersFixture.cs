using DotNet.Testcontainers.Builders;
using DotNet.Testcontainers.Containers;
using Xunit;

namespace EasyRacer.Tests;

public sealed class TestcontainersFixture : IAsyncLifetime, IDisposable
{
    private const ushort HttpPort = 8080;

    private readonly IContainer container = new ContainerBuilder()
        .WithImage("ghcr.io/jamesward/easyracer")
        .WithPortBinding(HttpPort, true)
        .WithWaitStrategy(Wait.ForUnixContainer().UntilHttpRequestIsSucceeded(r => r.ForPort(HttpPort)))
        .Build();

    public HttpClient HttpClient { get; } = new HttpClient();

    public ushort Port => container.GetMappedPublicPort(HttpPort);

    public Task InitializeAsync()
    {
        return container.StartAsync();
    }

    public Task DisposeAsync()
    {
        return container.StopAsync();
    }

    public void Dispose()
    {
        HttpClient.Dispose();
    }
}