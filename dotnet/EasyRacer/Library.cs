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
    public async Task<string> Scenario1(int port)
    {
        var cancel = new CancellationTokenSource();

        var req1 = http.GetStringAsync(GetUrl(port, 1), cancel.Token);
        var req2 = http.GetStringAsync(GetUrl(port, 1), cancel.Token);

        var result = await Task.WhenAny(req1, req2).ContinueWith(result => 
        {
            cancel.Cancel();

            return result.Result;
        });

        return await result;
    }

    /// <summary>
    /// Race 2 concurrent requests, where one produces a connection error.
    /// </summary>
    public async Task<string> Scenario2(int port)
    {
        var tasks = new Task<string>[]
        {
            http.GetStringAsync(GetUrl(port, 2)),
            http.GetStringAsync(GetUrl(port, 2))
        };

        var response = await Task.WhenAll(tasks).ContinueWith(_ => 
        {
            var task = tasks.FirstOrDefault(t => !t.IsFaulted);
            return task?.Result ?? "";
        });

        return response;
    }
    
    /// <summary>
    /// Race 10000 concurrent requests, accept the first that succeeds.
    /// </summary>
    public async Task<string> Scenario3(int port)
    {
        const int RequestCount = 10000;

        using var cancel = new CancellationTokenSource();

        var tasks = new List<Task<string>>(RequestCount);

        for (int i = 0; i < RequestCount; i++)
        {
            tasks.Add(http.GetStringAsync(GetUrl(port, 3), cancel.Token));
        }

        var response = await Task.WhenAny(tasks).ContinueWith(task => 
        {
            cancel.Cancel();
            return task.Result;
        });

        return await response;
    }

    /// <summary>
    /// Race 2 requests, 1 with a 1 second timeout.
    /// </summary>
    public async Task<string> Scenario4(int port)
    {
        var timeout = TimeSpan.FromSeconds(1);
        using var cancel = new CancellationTokenSource(timeout);

        var tasks = new Task<string>[]
        {
            http.GetStringAsync(GetUrl(port, 4)),
            http.GetStringAsync(GetUrl(port, 4), cancel.Token)
        };

        var response = await Task.WhenAll(tasks).ContinueWith(_ => 
        {
            var task = tasks.FirstOrDefault(t => !t.IsCanceled && 
                t.IsCompletedSuccessfully);

            return task?.Result ?? "";
        });

        return response;
    }

    /// <summary>
    /// Race 2 concurrent requests where a non-200 response is a loser
    /// </summary>
    public async Task Scenario5(int port)
    {
        Console.WriteLine("Running scenario 5...");

        using var cancel = new CancellationTokenSource();

        var tasks = new Task<HttpResponseMessage>[]
        {
            http.GetAsync(GetUrl(port, 5), cancel.Token),
            http.GetAsync(GetUrl(port, 5), cancel.Token)
        };

        await Task.WhenAll(tasks);

        var task = tasks.FirstOrDefault(t => t.IsCompletedSuccessfully && 
            t.Result.IsSuccessStatusCode);

        if (task != null)
        {
            var response = await task.Result.Content.ReadAsStringAsync();
            Console.WriteLine($"\tresponse: {response}");
        }
    }

    /// <summary>
    /// Race 3 concurrent requests where a non-200 response is a loser
    /// </summary>
    public async Task Scenario6(int port)
    {
        Console.WriteLine("Running scenario 6...");

        // This is not in the instrucions, but I needed to timeout because one
        // of the requests never completes with a non-200.
        using var cancel = new CancellationTokenSource(TimeSpan.FromSeconds(3));

        var tasks = new Task<HttpResponseMessage>[]
        {
            http.GetAsync(GetUrl(port, 6), cancel.Token),
            http.GetAsync(GetUrl(port, 6), cancel.Token),
            http.GetAsync(GetUrl(port, 6), cancel.Token)
        };

        try
        {
            await Task.WhenAll(tasks);
        }
        catch (Exception error)
        {
            Console.Error.WriteLine($"\tError: {error.Message}");
        }

        var task = tasks.FirstOrDefault(t => t.IsCompletedSuccessfully && 
            t.Result.IsSuccessStatusCode);

        if (task != null)
        {
            var response = await task.Result.Content.ReadAsStringAsync();
            Console.WriteLine($"\tresponse: {response}");
        }
    }

    /// <summary>
    /// Start a request, wait at least 3 seconds then start a second request (hedging)
    /// </summary>
    public async Task Scenario7(int port)
    {
        Console.WriteLine("Running scenario 6...");
    }

    private string GetUrl(int port, int scenario) => $"http://localhost:{port}/{scenario}";    
}