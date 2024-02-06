
namespace EasyRacer.Tests
{
    public class LibraryTests
    {
        [Fact]
        public async void TestScenario1()
        {
            var lib = new Library(); 
            await lib.Scenario1(8080);
        }
    }
}