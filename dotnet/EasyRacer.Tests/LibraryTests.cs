namespace EasyRacer.Tests;

public class LibraryTests : IDisposable
{
    const string ContainerImage = "ghcr.io/jamesward/easyracer";
    const int PortBinding = 8080;

    private readonly HttpClient httpClient;
    private readonly IContainer container;
    private readonly int port;

    public LibraryTests()
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
        port = container.GetMappedPublicPort(PortBinding);

        // Create a new instance of HttpClient to send HTTP requests.
        httpClient = new HttpClient();
    }

    [Fact]
    public async void TestScenario1()
    {
        var lib = new Library(httpClient);  
        Assert.Equal("right", await lib.Scenario1(port));
    }
    
    [Fact]
    public async void TestScenario2()
    {
        var lib = new Library(httpClient);  
        Assert.Equal("right", await lib.Scenario2(port));
    }

    [Fact]
    public async void TestScenario3()
    {
        var lib = new Library(httpClient);  
        Assert.Equal("right", await lib.Scenario3(port));
    }

    [Fact]
    public async void TestScenario4()
    {
        var lib = new Library(httpClient);  
        Assert.Equal("right", await lib.Scenario4(port));
    }

    [Fact]
    public async void TestScenario5()
    {
        var lib = new Library(httpClient);  
        Assert.Equal("right", await lib.Scenario5(port));
    }

    [Fact]
    public async void TestScenario6()
    {
        var lib = new Library(httpClient);  
        Assert.Equal("right", await lib.Scenario6(port));
    }

    [Fact]
    public async void TestScenario7()
    {
        var lib = new Library(httpClient);  
        Assert.Equal("right", await lib.Scenario7(port));
    }

    [Fact]
    public async void TestScenario8()
    {
        var lib = new Library(httpClient);  
        Assert.Equal("right", await lib.Scenario8(port));
    }

    [Fact]
    public async void TestScenario9()
    {
        var lib = new Library(httpClient);  
        Assert.Equal("right", await lib.Scenario9(port));
    }

    [Fact]
    public async void TestScenario10()
    {
        var lib = new Library(httpClient);  
        Assert.Equal("right", await lib.Scenario10(port));
    }

    public void Dispose()
    {
        container.StopAsync().Wait();
    }    
}
