using Xunit;

namespace EasyRacer.Tests;

public class LibraryTests : IClassFixture<TestcontainersFixture>
{
    TestcontainersFixture fixture;

    public LibraryTests(TestcontainersFixture fixture)
    {
        this.fixture = fixture;
    }

    [Fact]
    public async Task TestScenario1()
    {
        var lib = new Library(fixture.HttpClient);
        Assert.Equal("right", await lib.Scenario1(fixture.Port));
    }

    [Fact]
    public async Task TestScenario2()
    {
        var lib = new Library(fixture.HttpClient);
        Assert.Equal("right", await lib.Scenario2(fixture.Port));
    }

    [Fact]
    public async Task TestScenario3()
    {
        var lib = new Library(fixture.HttpClient);
        Assert.Equal("right", await lib.Scenario3(fixture.Port));
    }

    [Fact]
    public async Task TestScenario4()
    {
        var lib = new Library(fixture.HttpClient);
        Assert.Equal("right", await lib.Scenario4(fixture.Port));
    }

    [Fact]
    public async Task TestScenario5()
    {
        var lib = new Library(fixture.HttpClient);
        Assert.Equal("right", await lib.Scenario5(fixture.Port));
    }

    [Fact]
    public async Task TestScenario6()
    {
        var lib = new Library(fixture.HttpClient);
        Assert.Equal("right", await lib.Scenario6(fixture.Port));
    }

    [Fact]
    public async Task TestScenario7()
    {
        var lib = new Library(fixture.HttpClient);
        Assert.Equal("right", await lib.Scenario7(fixture.Port));
    }

    [Fact]
    public async Task TestScenario8()
    {
        var lib = new Library(fixture.HttpClient);
        Assert.Equal("right", await lib.Scenario8(fixture.Port));
    }

    [Fact]
    public async Task TestScenario9()
    {
        var lib = new Library(fixture.HttpClient);
        Assert.Equal("right", await lib.Scenario9(fixture.Port));
    }

    [Fact]
    public async Task TestScenario10()
    {
        var lib = new Library(fixture.HttpClient);
        Assert.Equal("right", await lib.Scenario10(fixture.Port));
    }
}
