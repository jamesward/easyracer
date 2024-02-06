namespace EasyRacer;

public class Library
{
    private HttpClient http;

    public Library(HttpClient http)
    {
        this.http = http;
    }

    /// <summary>
    /// Race 2 concurrent requests, print the result of the first one to return 
    /// and cancels the other one.
    /// </summary>
    public async Task Scenario1(int port)
    {
        Console.WriteLine("Running scenario 1...");

        var cancel = new CancellationTokenSource();

        var req1 = http.GetStringAsync(GetUrl(port, 1), cancel.Token);
        var req2 = http.GetStringAsync(GetUrl(port, 1), cancel.Token);

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
    public async Task Scenario2(int port)
    {
        Console.WriteLine("Running scenario 2...");

        var tasks = new Task<string>[]
        {
            http.GetStringAsync(GetUrl(port, 2)),
            http.GetStringAsync(GetUrl(port, 2))
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
    
    /// <summary>
    /// Race 10000 concurrent requests, accept the first that succeeds.
    /// </summary>
    public async Task Scenario3(int port)
    {
        const int RequestCount = 10000;

        Console.WriteLine($"Running scenario 3 with {RequestCount} requests...");

        using var cancel = new CancellationTokenSource();

        var tasks = new List<Task<string>>(RequestCount);

        for (int i = 0; i < RequestCount; i++)
        {
            tasks.Add(http.GetStringAsync(GetUrl(port, 3), cancel.Token));
        }

        await Task.WhenAny(tasks).ContinueWith(async task => 
        {
            var response = await task.Result;
            cancel.Cancel();
            Console.WriteLine($"\tresponse: {response}");
        });
    }

    /// <summary>
    /// Race 2 requests, 1 with a 1 second timeout.
    /// </summary>
    public async Task Scenario4(int port)
    {
        Console.WriteLine("Running scenario 4...");

        var timeout = TimeSpan.FromSeconds(1);
        using var cancel = new CancellationTokenSource(timeout);

        var tasks = new Task<string>[]
        {
            http.GetStringAsync(GetUrl(port, 4)),
            http.GetStringAsync(GetUrl(port, 4), cancel.Token)
        };

        await Task.WhenAny(tasks);

        var task = tasks.FirstOrDefault(t => t.IsCompletedSuccessfully);

        if (task != null)
        {
            var response = task.Result;
            Console.WriteLine($"\tresponse: {response}");
        }
    }

    private string GetUrl(int port, int scenario) => $"http://localhost:{port}/{scenario}";    
}