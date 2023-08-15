use futures::{StreamExt, TryFutureExt};
use reqwest::Client;

async fn scenario_1() -> String {

    async fn req() -> Result<String, reqwest::Error> {
        reqwest::get("http://localhost:8080/1").await?.text().await
    }

    let mut futures = futures::stream::FuturesUnordered::new();
    futures.push(req());
    futures.push(req());

    while let Some(Ok(result)) = futures.next().await {
        return result;
    }
    panic!("all futures failed");
}

async fn scenario_2() -> String {

    async fn req() -> Result<String, reqwest::Error> {
        reqwest::get("http://localhost:8080/2").await?.text().await
    }

    let mut futures = futures::stream::FuturesUnordered::new();
    futures.push(req());
    futures.push(req());

    // todo: not sure why we can't do `while let Some(Ok(result))`
    while let Some(result) = futures.next().await {
        if result.is_ok() {
            return result.unwrap();
        }
    }

    panic!("all futures failed");
}


// requires `ulimit -n 16000`
async fn scenario_3() -> String {
    let client = Client::new();

    async fn req(client: Client) -> Result<String, reqwest::Error> {
        client.get("http://localhost:8080/3").send().await?.text().await
    }

    let mut reqs = futures::stream::FuturesUnordered::new();
    for _ in 0..10_000 {
        let cloned_client = client.clone();
        reqs.push(req(cloned_client));
    }

    while let Some(Ok(result)) = reqs.next().await {
        return result;
    }

    panic!("all futures failed");
}

#[tokio::main]
async fn main() {
    let result1 = scenario_1().await;
    println!("{}", result1);

    let result2 = scenario_2().await;
    println!("{}", result2);

    let result3 = scenario_3().await;
    println!("{}", result3);
}
