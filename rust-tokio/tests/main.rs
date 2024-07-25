use testcontainers::core::{ContainerPort, WaitFor};
use testcontainers::GenericImage;
use testcontainers::runners::AsyncRunner;
use rust_tokio::*;

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn all_scenarios() {
    {
        let container = GenericImage::new("ghcr.io/jamesward/easyracer", "latest")
            .with_wait_for(WaitFor::message_on_stdout("Started server"))
            .with_exposed_port(ContainerPort::Tcp(8080))
            .start()
            .await
            .unwrap();

        let port = container.get_host_port_ipv4(8080).await.unwrap();

        assert_eq!(scenario_1(port).await, "right");
        assert_eq!(scenario_2(port).await, "right");
        assert_eq!(scenario_3(port).await, "right");
        assert_eq!(scenario_4(port).await, "right");
        assert_eq!(scenario_5(port).await, "right");
        assert_eq!(scenario_6(port).await, "right");
        assert_eq!(scenario_7(port).await, "right");
        assert_eq!(scenario_8(port).await, "right");
        assert_eq!(scenario_9(port).await, "right");
        assert_eq!(scenario_10(port).await, "right");
    }
}
