use std::time::Duration;
use tokio::sync::mpsc;
use tokio::time::{Instant, sleep, timeout};
use tokio::time::error::Elapsed;

fn url(port: u16, path: &str) -> String {
    format!("http://localhost:{}/{}", port, path)
}

pub async fn scenario_1(port: u16) -> String {

    async fn req(port: u16) -> Result<String, reqwest::Error> {
        reqwest::get(url(port, "1")).await?.text().await
    }

    tokio::select! {
        Ok(result) = req(port) => result,
        Ok(result) = req(port) => result,
        else => panic!("all failed")
    }
}

pub async fn scenario_2(port: u16) -> String {

    async fn req(port: u16) -> Result<String, reqwest::Error> {
        reqwest::get(url(port, "2")).await?.text().await
    }

    tokio::select! {
        Ok(result) = req(port) => result,
        Ok(result) = req(port) => result,
        else => panic!("all failed")
    }
}

// no way to do this with `tokio::select!`
// requires `ulimit -n 16000`
pub async fn scenario_3(port: u16) -> String {
    let client = reqwest::Client::new();

    let (tx, mut rx) = mpsc::channel(1);

    async fn req(port: u16, client: reqwest::Client) -> Result<String, reqwest::Error> {
        client.get(url(port, "3")).send().await?.text().await
    }

    for _ in 0..10_000 {
        let cloned_client = client.clone();
        let tx = tx.clone();
        tokio::spawn(async move {
            let result = req(port, cloned_client).await;
            tx.send(result).await
        });
    }

    rx.recv().await.unwrap().unwrap()
}

pub async fn scenario_4(port: u16) -> String {

    async fn req(port: u16) -> Result<String, reqwest::Error> {
        reqwest::get(url(port, "4")).await?.text().await
    }

    enum TimeoutOrRequestError {
        Timeout(Elapsed),
        RequestError(reqwest::Error),
    }

    async fn req_with_timeout(port: u16) -> Result<String, TimeoutOrRequestError> {
        let timeouted_result = timeout(Duration::from_secs(1), req(port));
        match timeouted_result.await {
            Ok(result) =>
                result.map_err(TimeoutOrRequestError::RequestError),
            Err(elapsed) =>
                Err(TimeoutOrRequestError::Timeout(elapsed)),
        }
    }

    tokio::select! {
        Ok(result) = req(port) => result,
        Ok(result) = req_with_timeout(port) => result,
        else => panic!("all failed")
    }
}

pub async fn scenario_5(port: u16) -> String {

    async fn req(port: u16) -> Result<String, reqwest::Error> {
        let response = reqwest::get(url(port, "5")).await?;
        response.error_for_status()?.text().await
    }

    tokio::select! {
        Ok(result) = req(port) => result,
        Ok(result) = req(port) => result,
        else => panic!("all failed")
    }
}

pub async fn scenario_6(port: u16) -> String {

    async fn req(port: u16) -> Result<String, reqwest::Error> {
        let response = reqwest::get(url(port, "6")).await?;
        response.error_for_status()?.text().await
    }

    tokio::select! {
        Ok(result) = req(port) => result,
        Ok(result) = req(port) => result,
        Ok(result) = req(port) => result,
        else => panic!("all failed")
    }
}

pub async fn scenario_7(port: u16) -> String {

    async fn req(port: u16) -> Result<String, reqwest::Error> {
        reqwest::get(url(port, "7")).await?.text().await
    }

    async fn hedge_req(port: u16) -> Result<String, reqwest::Error> {
        sleep(Duration::from_secs(3)).await;
        req(port).await
    }

    tokio::select! {
        Ok(result) = req(port) => result,
        Ok(result) = hedge_req(port) => result,
        else => panic!("all failed")
    }
}

pub async fn scenario_8(port: u16) -> String {

    async fn req_open(port: u16) -> Result<String, reqwest::Error> {
        let response = reqwest::get(url(port, "8?open")).await?;
        response.error_for_status()?.text().await
    }

    async fn req_use(port: u16, id: String) -> Result<String, reqwest::Error> {
        let response = reqwest::get(url(port, format!("8?use={}", id).as_str())).await?;
        response.error_for_status()?.text().await
    }

    async fn req_close(port: u16, id: String) -> Result<String, reqwest::Error> {
        let response = reqwest::get(url(port, format!("8?close={}", id).as_str())).await?;
        response.error_for_status()?.text().await
    }

    async fn req(port: u16) -> Result<String, reqwest::Error> {
        let id = req_open(port).await?;
        let resp = req_use(port, id.clone()).await;
        let _ = req_close(port, id).await;
        resp
    }

    tokio::select! {
        Ok(result) = req(port) => result,
        Ok(result) = req(port) => result,
        else => panic!("all failed")
    }
}

pub async fn scenario_9(port: u16) -> String {

    async fn req(port: u16) -> Result<(String, Instant), reqwest::Error> {
        let response = reqwest::get(url(port, "9")).await?;
        let text = response.error_for_status()?.text().await?;
        let now = Instant::now();
        Ok((text, now))
    }

    let responses_tuple = tokio::join!(
        req(port),
        req(port),
        req(port),
        req(port),
        req(port),
        req(port),
        req(port),
        req(port),
        req(port),
        req(port),
    );

    let responses = vec![
        responses_tuple.0,
        responses_tuple.1,
        responses_tuple.2,
        responses_tuple.3,
        responses_tuple.4,
        responses_tuple.5,
        responses_tuple.6,
        responses_tuple.7,
        responses_tuple.8,
        responses_tuple.9,
    ];

    let mut ok_responses: Vec<&(String, Instant)> = responses.iter().filter_map(|response| response.as_ref().ok()).collect();

    ok_responses.sort_by(|a, b| a.1.cmp(&b.1));

    ok_responses.iter().fold("".to_string(), |acc, response| acc + &response.0)
}
