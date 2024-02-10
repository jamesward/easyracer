namespace EasyRacer.Tests;

public class LibraryTests
{
    const string ContainerImage = "ghcr.io/jamesward/easyracer";
    const int Port = 8080;

    [Fact]
    public async void TestScenario1()
    {
        // Create a new instance of a container.
        var container = new ContainerBuilder()
            .WithImage(ContainerImage)
            // Bind port 8080 of the container to a random port on the host.
            .WithPortBinding(Port, true)
            // Wait until the HTTP endpoint of the container is available.
            .WithWaitStrategy(Wait.ForUnixContainer().UntilHttpRequestIsSucceeded(r => r.ForPort(Port)))
            // Build the container configuration.
            .Build();

        // Start the container.
        await container.StartAsync().ConfigureAwait(false);

        // Get the port of the container.
        var port = container.GetMappedPublicPort(Port);


        // Create a new instance of HttpClient to send HTTP requests.
        var httpClient = new HttpClient();

        var lib = new Library(httpClient); 

        Assert.Equal("right", await lib.Scenario1(port));
        Assert.Equal("right", await lib.Scenario2(port));
        Assert.Equal("right", await lib.Scenario3(port));
        Assert.Equal("right", await lib.Scenario4(port));
        Assert.Equal("right", await lib.Scenario5(port));
        Assert.Equal("right", await lib.Scenario6(port));
        Assert.Equal("right", await lib.Scenario7(port));
        Assert.Equal("right", await lib.Scenario8(port));
        Assert.Equal("right", await lib.Scenario9(port));
        Assert.Equal("right", await lib.Scenario10(port));
    }
}
