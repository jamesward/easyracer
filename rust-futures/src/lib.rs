use futures::{stream::FuturesUnordered, StreamExt};
use reqwest::Client;

fn url(port: u16, path: &str) -> String {
    format!("http://localhost:{}/{}", port, path)
}

pub async fn scenario_1(port: u16) -> String {

    async fn req(port: u16) -> Result<String, reqwest::Error> {
        reqwest::get(url(port, "1")).await?.text().await
    }

    let mut futures = FuturesUnordered::new();
    futures.push(req(port));
    futures.push(req(port));

    while let Some(result) = futures.next().await {
        if let Ok(body) = result {
            return body;
        }
    }

    panic!("all futures failed");
}

pub async fn scenario_2(port: u16) -> String {

    async fn req(port: u16) -> Result<String, reqwest::Error> {
        reqwest::get(url(port, "2")).await?.text().await
    }

    let mut futures = FuturesUnordered::new();
    futures.push(req(port));
    futures.push(req(port));

    while let Some(result) = futures.next().await {
        if let Ok(body) = result {
            return body;
        }
    }

    panic!("all futures failed");
}

// requires `ulimit -n 12000`
// dropping `reqs` on return cancels the losing requests
pub async fn scenario_3(port: u16) -> String {
    let (client, url) = (&Client::new(), &url(port, "3"));

    let mut reqs: FuturesUnordered<_> = (0..10_000)
        .map(|_| async move { client.get(url).send().await?.text().await })
        .collect();

    while let Some(result) = reqs.next().await {
        if let Ok(body) = result {
            return body;
        }
    }

    panic!("all futures failed");
}
