using EasyRacer;

const string Host = "localhost";
const ushort Port = 8080;

var http = new HttpClient();
var lib = new Library(http);

Console.WriteLine(await lib.Scenario1(Host, Port));
Console.WriteLine(await lib.Scenario2(Host, Port));
Console.WriteLine(await lib.Scenario3(Host, Port));
Console.WriteLine(await lib.Scenario4(Host, Port));
Console.WriteLine(await lib.Scenario5(Host, Port));
Console.WriteLine(await lib.Scenario6(Host, Port));
Console.WriteLine(await lib.Scenario7(Host, Port));
Console.WriteLine(await lib.Scenario8(Host, Port));
Console.WriteLine(await lib.Scenario9(Host, Port));
Console.WriteLine(await lib.Scenario10(Host, Port));
