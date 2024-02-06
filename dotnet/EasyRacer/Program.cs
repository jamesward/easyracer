using EasyRacer;

const int Port = 8080;

var http = new HttpClient();
var lib = new Library(http);

// await lib.Scenario1(Port);
// await lib.Scenario2(Port);
// await lib.Scenario3(Port);
// await lib.Scenario4(Port);
// await lib.Scenario5(Port);
await lib.Scenario6(Port);
