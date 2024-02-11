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
    public async void TestScenario1()
    {
        var lib = new Library(fixture.HttpClient);  
        Assert.Equal("right", await lib.Scenario1(fixture.Port));
    }
    
    [Fact]
    public async void TestScenario2()
    {
        var lib = new Library(fixture.HttpClient);  
        Assert.Equal("right", await lib.Scenario2(fixture.Port));
    }

    [Fact]
    public async void TestScenario3()
    {
        var lib = new Library(fixture.HttpClient);  
        Assert.Equal("right", await lib.Scenario3(fixture.Port));
    }

    [Fact]
    public async void TestScenario4()
    {
        var lib = new Library(fixture.HttpClient);  
        Assert.Equal("right", await lib.Scenario4(fixture.Port));
    }

    [Fact]
    public async void TestScenario5()
    {
        var lib = new Library(fixture.HttpClient);  
        Assert.Equal("right", await lib.Scenario5(fixture.Port));
    }

    [Fact]
    public async void TestScenario6()
    {
        var lib = new Library(fixture.HttpClient);  
        Assert.Equal("right", await lib.Scenario6(fixture.Port));
    }

    [Fact]
    public async void TestScenario7()
    {
        var lib = new Library(fixture.HttpClient);  
        Assert.Equal("right", await lib.Scenario7(fixture.Port));
    }

    [Fact]
    public async void TestScenario8()
    {
        var lib = new Library(fixture.HttpClient);  
        Assert.Equal("right", await lib.Scenario8(fixture.Port));
    }

    [Fact]
    public async void TestScenario9()
    {
        var lib = new Library(fixture.HttpClient);  
        Assert.Equal("right", await lib.Scenario9(fixture.Port));
    }

    [Fact]
    public async void TestScenario10()
    {
        var lib = new Library(fixture.HttpClient);  
        Assert.Equal("right", await lib.Scenario10(fixture.Port));
    }
}
