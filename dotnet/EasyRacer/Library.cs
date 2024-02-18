using System.Text;

namespace EasyRacer;

/// <summary>
/// Implements 10 scenarios demonstrating structured concurrency in .NET.
/// </summary>
public class Library(HttpClient http)
{
    /// <summary>
    /// Race 2 concurrent requests, print the result of the first one to return
    /// and cancels the other one.
    /// </summary>
    public async Task<string> Scenario1(string host, ushort port)
    {
        using var cancel = new CancellationTokenSource();

        var url = GetUrl(host, port, 1);

        var req1 = http.GetStringAsync(url, cancel.Token);
        var req2 = http.GetStringAsync(url, cancel.Token);

        var task = await Task.WhenAny(req1, req2)
            .ConfigureAwait(false);

        await cancel.CancelAsync()
            .ConfigureAwait(false);

        return task.Result;
    }

    /// <summary>
    /// Race 2 concurrent requests, where one produces a connection error.
    /// </summary>
    public async Task<string> Scenario2(string host, ushort port)
    {
        using var cancel = new CancellationTokenSource();

        var url = GetUrl(host, port, 2);

        var tasks = new List<Task<HttpResponseMessage>>
        {
            http.GetAsync(url, cancel.Token),
            http.GetAsync(url, cancel.Token)
        };

        return await RaceStringResultAsync(tasks, cancel)
            .ConfigureAwait(false);
    }

    /// <summary>
    /// Race 10000 concurrent requests, accept the first that succeeds.
    /// </summary>
    public async Task<string> Scenario3(string host, ushort port)
    {
        using var cancel = new CancellationTokenSource();

        var url = GetUrl(host, port, 3);

        var tasks = Enumerable.Range(0, 10000)
            .Select(_ => http.GetAsync(url, cancel.Token))
            .ToList();

        return await RaceStringResultAsync(tasks, cancel)
            .ConfigureAwait(false);
    }

    /// <summary>
    /// Race 2 requests, 1 with a 1 second timeout.
    /// </summary>
    public async Task<string> Scenario4(string host, ushort port)
    {
        using var cancel = new CancellationTokenSource(TimeSpan.FromSeconds(10));

        using var timeout = new CancellationTokenSource(TimeSpan.FromSeconds(1));

        var url = GetUrl(host, port, 4);

        var tasks = new List<Task<HttpResponseMessage>>
        {
            http.GetAsync(url, timeout.Token),
            http.GetAsync(url, default(CancellationToken))
        };

        return await RaceStringResultAsync(tasks, cancel)
            .ConfigureAwait(false);
    }

    /// <summary>
    /// Race 2 concurrent requests where a non-200 response is a loser
    /// </summary>
    public async Task<string> Scenario5(string host, ushort port)
    {
        using var cancel = new CancellationTokenSource();

        var url = GetUrl(host, port, 5);

        var tasks = new List<Task<HttpResponseMessage>>
        {
            http.GetAsync(url, cancel.Token),
            http.GetAsync(url, cancel.Token)
        };

        return await RaceStringResultAsync(tasks, cancel)
            .ConfigureAwait(false);
    }

    /// <summary>
    /// Race 3 concurrent requests where a non-200 response is a loser
    /// </summary>
    public async Task<string> Scenario6(string host, ushort port)
    {
        // Set a long timeout, just in case 1 or more never complete.
        using var cancel = new CancellationTokenSource(TimeSpan.FromSeconds(10));

        var url = GetUrl(host, port, 6);

        var tasks = new List<Task<HttpResponseMessage>>
        {
            http.GetAsync(url, cancel.Token),
            http.GetAsync(url, cancel.Token),
            http.GetAsync(url, cancel.Token)
        };

        return await RaceStringResultAsync(tasks, cancel)
            .ConfigureAwait(false);
    }

    /// <summary>
    /// Start a request, wait at least 3 seconds then start a second request (hedging)
    /// </summary>
    public async Task<string> Scenario7(string host, ushort port)
    {
        using var cancel = new CancellationTokenSource();

        var url = GetUrl(host, port, 7);

        var tasks = new List<Task<HttpResponseMessage>>();
        tasks.Add(http.GetAsync(url, cancel.Token));

        await Task.Delay(TimeSpan.FromSeconds(3), default(CancellationToken))
            .ConfigureAwait(false);

        tasks.Add(http.GetAsync(url, cancel.Token));

        return await RaceStringResultAsync(tasks, cancel)
            .ConfigureAwait(false);
    }

    /// <summary>
    /// Race 2 concurrent requests that "use" a resource which is obtained and
    /// released through other requests. The "use" request can return a non-20x
    /// request, in which case it is not a winner.
    /// </summary>
    public async Task<string> Scenario8(string host, ushort port)
    {
        using var cancel = new CancellationTokenSource();

        var tasks = new List<Task<HttpResponseMessage>>
        {
            CreateScenario8Request(host, port, cancel.Token),
            CreateScenario8Request(host, port, cancel.Token)
        };

        return await RaceStringResultAsync(tasks, cancel)
            .ConfigureAwait(false);
    }

    /// <summary>
    /// Wraps Scenario 8's 3 phases; Open, Use, Close.
    /// </summary>
    private async Task<HttpResponseMessage> CreateScenario8Request(
        string host,
        ushort port,
        CancellationToken cancel)
    {
        var baseUrl = GetUrl(host, port, 8);
        var openUrl = baseUrl + "?open";
        string UseUrl(string id) => baseUrl + $"?use={id}";
        string CloseUrl(string id) => baseUrl + $"?close={id}";

        var id = await http.GetStringAsync(openUrl, cancel)
            .ConfigureAwait(false);

        if (string.IsNullOrWhiteSpace(id))
        {
            throw new ApplicationException("No id returned");
        }

        try
        {
            return await http.GetAsync(UseUrl(id), cancel)
                .ConfigureAwait(false);
        }
        finally
        {
            await http.GetAsync(CloseUrl(id), cancel)
                .ConfigureAwait(false);
        }
    }

    /// <summary>
    /// Make 10 concurrent requests where 5 return a 200 response with a letter
    /// </summary>
    public async Task<string> Scenario9(string host, ushort port)
    {
        using var cancel = new CancellationTokenSource();

        var url = GetUrl(host, port, 9);

        var answer = new StringBuilder();

        var tasks = Enumerable.Range(0, 10)
            .Select(_ => http.GetStringAsync(url, cancel.Token))
            .Select(response => response.ContinueWith(task =>
            {
                answer.Append(task.Result);

                if (5.Equals(answer.Length))
                {
                    cancel.Cancel();
                }
            }, cancel.Token))
            .ToList();

        try
        {
            await Task.WhenAll(tasks)
                .ConfigureAwait(false);
        }
        catch
        {
            /* Ignore */
        }

        return answer.ToString();
    }

    /// <summary>
    /// This scenario validates that a computationally heavy task can be run in
    /// parallel to another task, and then cancelled.
    /// </summary>
    public async Task<string> Scenario10(string host, ushort port)
    {
        var scenario = new Scenario10(http);
        return await scenario.Run(GetUrl(host, port, 10))
            .ConfigureAwait(false);
    }

    /// <summary>
    /// Helper method to return the first successful http request result.
    /// Waits for the first successful task and cancels all others.
    /// </summary>
    private static async Task<string> RaceStringResultAsync(
        List<Task<HttpResponseMessage>> tasks,
        CancellationTokenSource cancel)
    {
        while (!cancel.IsCancellationRequested && tasks.Count > 0)
        {
            var task = await Task.WhenAny(tasks)
                .ConfigureAwait(false);

            if (task.IsCompletedSuccessfully && task.Result.IsSuccessStatusCode)
            {
                await cancel.CancelAsync()
                    .ConfigureAwait(false);

                using var response = task.Result;

                return await response.Content.ReadAsStringAsync()
                    .ConfigureAwait(false);
            }

            // Task wasn't a success, so don't wait on this task any more.
            tasks.Remove(task);
        }

        return string.Empty;
    }

    /// <summary>
    /// Helper method to get the url for a scenario.
    /// </summary>
    private static string GetUrl(string host, ushort port, int scenario) => $"http://{host}:{port}/{scenario}";
}
