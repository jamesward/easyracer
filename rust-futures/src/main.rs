use rust_futures::*;

#[tokio::main]
async fn main() {
    println!("{}", scenario_1(8080).await);
    println!("{}", scenario_2(8080).await);
    println!("{}", scenario_3(8080).await);
}
