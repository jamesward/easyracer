using EasyRacer;

const int Port = 8080;

var http = new HttpClient();
var lib = new Library(http);

// Console.WriteLine(await lib.Scenario1(Port));
Console.WriteLine(await lib.Scenario2(Port));
// Console.WriteLine(await lib.Scenario3(Port));
Console.WriteLine(await lib.Scenario4(Port));
// await lib.Scenario5(Port);
// await lib.Scenario6(Port);
