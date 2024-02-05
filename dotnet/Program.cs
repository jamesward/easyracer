const int Port = 8080;

var http = new HttpClient();

string GetUrl(int port, int scenario) => $"http://localhost:{port}/{scenario}";

/// <summary>
/// Race 2 concurrent requests, print the result of the first one to return 
/// and cancels the other one.
/// </summary>
async Task Scenario1()
{
    Console.WriteLine("Running scenario 1...");

    var cancel = new CancellationTokenSource();

    var req1 = http.GetStringAsync(GetUrl(Port, 1), cancel.Token);
    var req2 = http.GetStringAsync(GetUrl(Port, 1), cancel.Token);

    await Task.WhenAny(req1, req2).ContinueWith(async result => 
    {
        var response = await result.Result;
        Console.WriteLine($"\tresponse: {response}");
        
        cancel.Cancel();
    });
}

/// <summary>
/// Race 2 concurrent requests, where one produces a connection error.
/// </summary>
async Task Scenario2()
{
    Console.WriteLine("Running scenario 2...");

    var tasks = new Task<string>[]
    {
        http.GetStringAsync(GetUrl(Port, 2)),
        http.GetStringAsync(GetUrl(Port, 2))
    };

    try
    {
        await Task.WhenAll(tasks);
    }
    catch (Exception error)
    {
        Console.Error.WriteLine($"\tError: {error.Message}");
    }

    var task = tasks.FirstOrDefault(t => !t.IsFaulted);

    if (task != null)
    {
        var response = await task;
        Console.WriteLine($"\tresponse: {response}");
    }
}

await Scenario1();
await Scenario2();