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

        // Using ContinueWith() to supress cancellation exception
        var response = await Task.WhenAll(tasks).ContinueWith(_ => 
        {
            return GetStringResultAsync(tasks);
        });

        return response;
    }
    
    /// <summary>
    /// Race 10000 concurrent requests, accept the first that succeeds.
    /// </summary>
    public async Task<string> Scenario3(int port)
    {
        const int RequestCount = 10000;

        var url = GetUrl(port, 3);

        using var cancel = new CancellationTokenSource();

        var tasks = new List<Task<string>>(RequestCount);

        for (int i = 0; i < RequestCount; i++)
        {
            tasks.Add(http.GetStringAsync(url, cancel.Token));
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

        // Using ContinueWith() to supress cancellation exception
        var response = await Task.WhenAll(tasks).ContinueWith(_ => 
        {
            return GetStringResultAsync(tasks);
        });

        return response;
    }

    /// <summary>
    /// Race 2 concurrent requests where a non-200 response is a loser
    /// </summary>
    public async Task<string> Scenario5(int port)
    {
        using var cancel = new CancellationTokenSource();

        var tasks = new Task<HttpResponseMessage>[]
        {
            http.GetAsync(GetUrl(port, 5), cancel.Token),
            http.GetAsync(GetUrl(port, 5), cancel.Token)
        };

        await Task.WhenAll(tasks);

        return await GetStringResultAsync(tasks);
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

        return await GetStringResultAsync(tasks, cancel);
    }

    /// <summary>
    /// Start a request, wait at least 3 seconds then start a second request (hedging)
    /// </summary>
    public async Task<string> Scenario7(int port)
    {
        // Not in the instructions, but needed to timeout here.
        var cancel = new CancellationTokenSource(TimeSpan.FromSeconds(5));

        var tasks = new List<Task<HttpResponseMessage>>
        {
            http.GetAsync(GetUrl(port, 7), cancel.Token),
            await Task.Delay(TimeSpan.FromSeconds(3))
                .ContinueWith(_ => http.GetAsync(GetUrl(port, 7), cancel.Token)),
        };

        return await GetStringResultAsync(tasks, cancel);
    }

    /// <summary>
    /// Race 2 concurrent requests that "use" a resource which is obtained and 
    /// released through other requests. The "use" request can return a non-20x 
    /// request, in which case it is not a winner.
    /// </summary>
    public async Task<string> Scenario8(int port)
    {
        var tasks = new Task<string>[] 
        {
            CreateScenario8Request(port),
            CreateScenario8Request(port)
        };

        var response = await Task.WhenAll(tasks).ContinueWith(_ => 
        {
            return GetStringResultAsync(tasks);
        });

        return response;
    }

    private async Task<string> CreateScenario8Request(int port)
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
            var response = await http.GetAsync(UseUrl(id));

            if (!response.IsSuccessStatusCode)
            {
                throw new Exception("Invalid status code");
            }

            return await response.Content.ReadAsStringAsync();
        }
        finally
        {
            await http.GetStringAsync(CloseUrl(id));
        }
    }    

    /// <summary>
    /// Make 10 concurrent requests where 5 return a 200 response with a letter
    /// </summary>
    public async Task<string> Scenario9(int port)
    {
        string answer = "";
        
        var tasks = new List<Task>();

        for (int i = 0; i < 10; i++)
        {
            tasks.Add(
                http.GetStringAsync(GetUrl(port, 9))
                    .ContinueWith(task => answer += task.Result)
            );
        }

        try { await Task.WhenAll(tasks); }
        catch (Exception) { /*Ignore*/ }

        return answer;
    }

    /// <summary>
    /// This scenario validates that a computationally heavy task can be run in parallel to another task, and then cancelled.
    /// </summary>
    public async Task<string> Scenario10(int port)
    {
        var scenario = new Scenario10(http);
        return await scenario.Run(GetUrl(port, 10));
    }

    /// <summary>
    /// Helper method to return the first non-canceled task string result.
    /// </summary>
    private string GetStringResultAsync(IEnumerable<Task<string>> tasks)
    {
        var task = tasks.FirstOrDefault(t => t.IsCompletedSuccessfully);

        return task?.Result ?? "";
    }

    /// <summary>
    /// Helper method to return the first non-canceled task result.
    /// </summary>
    private async Task<string> GetStringResultAsync(IEnumerable<Task<HttpResponseMessage>> tasks)
    {
        var task = tasks.FirstOrDefault(t => t.IsCompletedSuccessfully && 
            t.Result.IsSuccessStatusCode);

        if (task == null)
            throw new ApplicationException("No successful request");

        var response = await task;

        return await response.Content.ReadAsStringAsync();
    }

    private async Task<string> GetStringResultAsync(List<Task<HttpResponseMessage>> tasks, CancellationTokenSource cancel)
    {
        while (!cancel.IsCancellationRequested)
        {
            var task = await Task.WhenAny(tasks);

            if (task.IsCompletedSuccessfully && task.Result.IsSuccessStatusCode)
            {
                cancel.Cancel();
                return await task.Result.Content.ReadAsStringAsync();
            }

            // Don't wait on this task any more.
            tasks.Remove(task);
        }

        return string.Empty;
    }    

    /// <summary>
    /// Helper method to get the url for a scenario.
    /// </summary>
    private string GetUrl(int port, int scenario) => $"http://localhost:{port}/{scenario}";    
}