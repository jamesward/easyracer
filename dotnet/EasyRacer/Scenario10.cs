namespace EasyRacer;

public class Scenario10(HttpClient http)
{
    public readonly string Id = Guid.NewGuid().ToString();

    private double cpuUsage = 0;

    public async Task<string> Run(string url)
    {
        using var cancel = new CancellationTokenSource();
        
        var openUrl = $"{url}?{Id}";

        var openTask = http.GetStringAsync(openUrl).ContinueWith(task => 
        {
            cancel.Cancel();
            return task.Result;
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
