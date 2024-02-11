namespace EasyRacer;

/// <summary>
/// Implements 10 scenarios demonstrating structred concurrency in .NET.
/// </summary>
public class Library(HttpClient http)
{
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
        var tasks = new List<Task<HttpResponseMessage>>
        {
            http.GetAsync(GetUrl(port, 2)),
            http.GetAsync(GetUrl(port, 2))
        };

        return await RaceStringResultAsync(tasks);
    }
    
    /// <summary>
    /// Race 10000 concurrent requests, accept the first that succeeds.
    /// </summary>
    public async Task<string> Scenario3(int port)
    {
        const int RequestCount = 10000;
        var url = GetUrl(port, 3);

        using var cancel = new CancellationTokenSource();

        var tasks = new List<Task<HttpResponseMessage>>(RequestCount);

        for (int i = 0; i < RequestCount; i++)
        {
            tasks.Add(http.GetAsync(url, cancel.Token));
        }

        return await RaceStringResultAsync(tasks, cancel);
    }

    /// <summary>
    /// Race 2 requests, 1 with a 1 second timeout.
    /// </summary>
    public async Task<string> Scenario4(int port)
    {
        var timeout = TimeSpan.FromSeconds(1);
        using var cancel = new CancellationTokenSource(timeout);

        var tasks = new List<Task<HttpResponseMessage>>
        {
            http.GetAsync(GetUrl(port, 4), cancel.Token),
            http.GetAsync(GetUrl(port, 4))
        };

        return await RaceStringResultAsync(tasks);
    }


    /// <summary>
    /// Race 2 concurrent requests where a non-200 response is a loser
    /// </summary>
    public async Task<string> Scenario5(int port)
    {
        using var cancel = new CancellationTokenSource();

        var tasks = new List<Task<HttpResponseMessage>>
        {
            http.GetAsync(GetUrl(port, 5), cancel.Token),
            http.GetAsync(GetUrl(port, 5), cancel.Token)
        };

        return await RaceStringResultAsync(tasks, cancel);
    }

    /// <summary>
    /// Race 3 concurrent requests where a non-200 response is a loser
    /// </summary>
    public async Task<string> Scenario6(int port)
    {
        // Set a long timeout, just in case 1 or more never complete.
        using var cancel = new CancellationTokenSource(TimeSpan.FromSeconds(10));

        var tasks = new List<Task<HttpResponseMessage>>
        {
            http.GetAsync(GetUrl(port, 6), cancel.Token),
            http.GetAsync(GetUrl(port, 6), cancel.Token),
            http.GetAsync(GetUrl(port, 6), cancel.Token)
        };

        return await RaceStringResultAsync(tasks, cancel);
    }

    /// <summary>
    /// Start a request, wait at least 3 seconds then start a second request (hedging)
    /// </summary>
    public async Task<string> Scenario7(int port)
    {
        var cancel = new CancellationTokenSource();

        var tasks = new List<Task<HttpResponseMessage>>
        {
            http.GetAsync(GetUrl(port, 7), cancel.Token),
            await Task.Delay(TimeSpan.FromSeconds(3))
                .ContinueWith(_ => http.GetAsync(GetUrl(port, 7), cancel.Token)),
        };

        return await RaceStringResultAsync(tasks, cancel);
    }

    /// <summary>
    /// Race 2 concurrent requests that "use" a resource which is obtained and 
    /// released through other requests. The "use" request can return a non-20x 
    /// request, in which case it is not a winner.
    /// </summary>
    public async Task<string> Scenario8(int port)
    {
        var cancel = new CancellationTokenSource();

        var tasks = new List<Task<HttpResponseMessage>> 
        {
            CreateScenario8Request(port, cancel.Token),
            CreateScenario8Request(port, cancel.Token)
        };

        return await RaceStringResultAsync(tasks, cancel);
    }

    /// <summary>
    /// Wraps Scenario 8's 3 phases; Open, Use, Close.
    /// </summary>
    private async Task<HttpResponseMessage> CreateScenario8Request(int port, 
        CancellationToken cancel)
    {
        var baseUrl = GetUrl(port, 8);
        var openUrl = baseUrl + "?open";
        string UseUrl(string id) => baseUrl + $"?use={id}";
        string CloseUrl(string id) => baseUrl + $"?close={id}";

        var id = await http.GetStringAsync(openUrl);

        if (string.IsNullOrWhiteSpace(id))
            throw new ApplicationException("No id returned");

        try
        {
            return await http.GetAsync(UseUrl(id), cancel);
        }
        finally
        {
            await http.GetAsync(CloseUrl(id));
        }
    }    

    /// <summary>
    /// Make 10 concurrent requests where 5 return a 200 response with a letter
    /// </summary>
    public async Task<string> Scenario9(int port)
    {
        var cancel = new CancellationTokenSource();

        var tasks = new List<Task>();

        string answer = string.Empty;

        for (int i = 0; i < 10; i++)
        {
            tasks.Add(
                http.GetStringAsync(GetUrl(port, 9), cancel.Token)
                    .ContinueWith(task => 
                    {
                        answer += task.Result;

                        if (answer.Length == 5)
                            cancel.Cancel();
                    })
            );
        }

        try { await Task.WhenAll(tasks); }
        catch ( Exception ) { /*Ignore*/ }

        return answer;
    }

    /// <summary>
    /// This scenario validates that a computationally heavy task can be run in
    /// parallel to another task, and then cancelled.
    /// </summary>
    public async Task<string> Scenario10(int port)
    {
        var scenario = new Scenario10(http);
        return await scenario.Run(GetUrl(port, 10));
    }

    /// <summary>
    /// Helper method to return the first successful http request result.
    /// Waits for the first successful task and cancels all others. 
    /// </summary>
    private async Task<string> RaceStringResultAsync(
        List<Task<HttpResponseMessage>> tasks, 
        CancellationTokenSource? cancel = null)
    {
        // If no cancel token is provided, create one with a reasonable timeout.
        if (cancel == null)
            cancel = new CancellationTokenSource(TimeSpan.FromSeconds(100));

        while (!cancel.IsCancellationRequested && tasks.Count > 0)
        {
            var task = await Task.WhenAny(tasks);

            if (task.IsCompletedSuccessfully && task.Result.IsSuccessStatusCode)
            {
                cancel.Cancel();
                return await task.Result.Content.ReadAsStringAsync();
            }

            // Task wasn't a succes, so don't wait on this task any more.
            tasks.Remove(task);
        }

        return string.Empty;
    }    

    /// <summary>
    /// Helper method to get the url for a scenario.
    /// </summary>
    private string GetUrl(int port, int scenario) => $"http://localhost:{port}/{scenario}";    
}
