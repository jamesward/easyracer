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
    unimplemented!()
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
