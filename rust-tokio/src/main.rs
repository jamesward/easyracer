use std::time::Duration;
use tokio::time::{Instant, sleep, timeout};
use tokio::time::error::Elapsed;

async fn scenario_1() -> String {

    async fn req() -> Result<String, reqwest::Error> {
        reqwest::get("http://localhost:8080/1").await?.text().await
    }

    tokio::select! {
        Ok(result) = req() => result,
        Ok(result) = req() => result,
        else => panic!("all failed")
    }
}

async fn scenario_2() -> String {

    async fn req() -> Result<String, reqwest::Error> {
        reqwest::get("http://localhost:8080/2").await?.text().await
    }

    tokio::select! {
        Ok(result) = req() => result,
        Ok(result) = req() => result,
        else => panic!("all failed")
    }
}

// no way to do this with `tokio::select!`
async fn scenario_3() -> String {
    "notimplemented".to_string()
}

async fn scenario_4() -> String {

    async fn req() -> Result<String, reqwest::Error> {
        reqwest::get("http://localhost:8080/4").await?.text().await
    }

    enum TimeoutOrRequestError {
        Timeout(Elapsed),
        RequestError(reqwest::Error),
    }

    async fn req_with_timeout() -> Result<String, TimeoutOrRequestError> {
        let timeouted_result = timeout(Duration::from_secs(1), req());
        match timeouted_result.await {
            Ok(result) =>
                result.map_err(TimeoutOrRequestError::RequestError),
            Err(elapsed) =>
                Err(TimeoutOrRequestError::Timeout(elapsed)),
        }
    }

    tokio::select! {
        Ok(result) = req() => result,
        Ok(result) = req_with_timeout() => result,
        else => panic!("all failed")
    }
}

async fn scenario_5() -> String {

    async fn req() -> Result<String, reqwest::Error> {
        let response = reqwest::get("http://localhost:8080/5").await?;
        response.error_for_status()?.text().await
    }

    tokio::select! {
        Ok(result) = req() => result,
        Ok(result) = req() => result,
        else => panic!("all failed")
    }
}

async fn scenario_6() -> String {

    async fn req() -> Result<String, reqwest::Error> {
        let response = reqwest::get("http://localhost:8080/6").await?;
        response.error_for_status()?.text().await
    }

    tokio::select! {
        Ok(result) = req() => result,
        Ok(result) = req() => result,
        Ok(result) = req() => result,
        else => panic!("all failed")
    }
}

async fn scenario_7() -> String {

    async fn req() -> Result<String, reqwest::Error> {
        reqwest::get("http://localhost:8080/7").await?.text().await
    }

    async fn hedge_req() -> Result<String, reqwest::Error> {
        sleep(Duration::from_secs(3)).await;
        req().await
    }

    tokio::select! {
        Ok(result) = req() => result,
        Ok(result) = hedge_req() => result,
        else => panic!("all failed")
    }
}

async fn scenario_8() -> String {

    async fn req_open() -> Result<String, reqwest::Error> {
        let response = reqwest::get("http://localhost:8080/8?open").await?;
        response.error_for_status()?.text().await
    }

    async fn req_use(id: String) -> Result<String, reqwest::Error> {
        let response = reqwest::get(format!("http://localhost:8080/8?use={}", id)).await?;
        response.error_for_status()?.text().await
    }

    async fn req_close(id: String) -> Result<String, reqwest::Error> {
        let response = reqwest::get(format!("http://localhost:8080/8?close={}", id)).await?;
        response.error_for_status()?.text().await
    }

    async fn req() -> Result<String, reqwest::Error> {
        let id = req_open().await?;
        let resp = req_use(id.clone()).await;
        let _ = req_close(id).await;
        resp
    }

    tokio::select! {
        Ok(result) = req() => result,
        Ok(result) = req() => result,
        else => panic!("all failed")
    }
}

async fn scenario_9() -> String {

    async fn req() -> Result<(String, Instant), reqwest::Error> {
        let response = reqwest::get("http://localhost:8080/9").await?;
        let text = response.error_for_status()?.text().await?;
        let now = Instant::now();
        Ok((text, now))
    }

    let responses_tuple = tokio::join!(
        req(),
        req(),
        req(),
        req(),
        req(),
        req(),
        req(),
        req(),
        req(),
        req(),
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

#[tokio::main]
async fn main() {
    println!("{}", scenario_1().await);
    println!("{}", scenario_2().await);
    println!("{}", scenario_3().await);
    println!("{}", scenario_4().await);
    println!("{}", scenario_5().await);
    println!("{}", scenario_6().await);
    println!("{}", scenario_7().await);
    println!("{}", scenario_8().await);
    println!("{}", scenario_9().await);
}
