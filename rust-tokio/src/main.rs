use rust_tokio::*;

#[tokio::main]
async fn main() {
    println!("{}", scenario_1(8080).await);
    println!("{}", scenario_2(8080).await);
    println!("{}", scenario_3(8080).await);
    println!("{}", scenario_4(8080).await);
    println!("{}", scenario_5(8080).await);
    println!("{}", scenario_6(8080).await);
    println!("{}", scenario_7(8080).await);
    println!("{}", scenario_8(8080).await);
    println!("{}", scenario_9(8080).await);
    println!("{}", scenario_10(8080).await);
}
