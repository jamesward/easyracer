use futures::StreamExt;

async fn scenario_1() -> Result<String, reqwest::Error> {

    async fn req() -> Result<String, reqwest::Error> {
        reqwest::get("http://localhost:8080/1").await?.text().await
    }

    let reqs = vec![req(), req()];

    let mut futures = futures::stream::FuturesUnordered::from_iter(reqs);
    while let Some(v) = futures.next().await {
        if v.is_ok() {
            return v;
        }
    }
    panic!("all futures failed");
}

async fn scenario_2() -> Result<String, reqwest::Error> {

    async fn req() -> Result<String, reqwest::Error> {
        reqwest::get("http://localhost:8080/2").await?.text().await
    }

    let reqs = vec![req(), req()];

    let mut futures = futures::stream::FuturesUnordered::from_iter(reqs);
    while let Some(v) = futures.next().await {
        if v.is_ok() {
            return v;
        }
    }
    panic!("all futures failed");
}

#[tokio::main]
async fn main() {
    let result1 = scenario_1().await.unwrap();
    println!("{}", result1);

    let result2 = scenario_2().await.unwrap();
    println!("{}", result2);
}
