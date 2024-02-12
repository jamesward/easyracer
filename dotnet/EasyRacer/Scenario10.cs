namespace EasyRacer;

/// <summary>
/// Wraps the context for a computationally heavy task run in parallel to 
/// another task, and then cancelled.
/// </summary>
public class Scenario10(HttpClient http)
{
    // Unique identifier for this scenario instance.
    public readonly string Id = Guid.NewGuid().ToString();

    // Tracks simulated cpu usage 0-1.
    private double cpuUsage = 0.0;

    /// <summary>
    /// Run the scenario.
    /// </summary>
    public async Task<string> Run(string url)
    {
        using var cancel = new CancellationTokenSource();
        
        var openUrl = $"{url}?{Id}";

        // Launch the open task by calling ContinueWith(), and cancel any other
        // tasks when it completes.
        var openTask = http.GetStringAsync(openUrl).ContinueWith(task => 
        {
            // When this http request completes, cancel the cpu task.
            cancel.Cancel();
        });

        var loadTask = Task.Run(() => SimulateCpu(cancel.Token));

        string answer = string.Empty;
        
        try 
        { 
            answer = await Monitor(url); 
        }
        catch (TaskCanceledException) 
        {}
        
        return answer;
    }

    private async Task<string> Monitor(string url)
    {
        while (true)
        {
            await Task.Delay(TimeSpan.FromSeconds(1));

            var loadUrl = $"{url}?{Id}={cpuUsage}"; 
            var response = await http.GetAsync(loadUrl);

            var answer = await response.Content.ReadAsStringAsync();

            if (response.IsSuccessStatusCode)
            {
                return answer;
            }
            else if (!string.IsNullOrWhiteSpace(answer))
            {
                Console.WriteLine($"Error: {answer}");
            }
        }
    }

    private void SimulateCpu(CancellationToken token)
    {
        while (!token.IsCancellationRequested)
        {
            cpuUsage = 0.95;
        }
        cpuUsage = 0;
    }
}
