import pytest
from testcontainers.core.generic import DockerContainer

import main


#from testcontainers.core.waiting_utils import wait_container_is_ready


@pytest.mark.asyncio
async def test_all():
    assert len(main.scenarios) == 10

    # todo: no way to set pull policy yet
    docker_container = DockerContainer("ghcr.io/jamesward/easyracer").with_exposed_ports(8080)
    with docker_container as easyracer:
        port = easyracer.get_exposed_port(8080)

        # todo: can't find the right way to wait on the container being ready
        import time
        time.sleep(2)

        #@wait_container_is_ready()
        async def connected():
            def assert_right(actual: str):
                assert actual == "right"
            for scenario in main.scenarios:
                num = scenario.__name__[8:]
                url = f"http://localhost:{port}/{num}"
                assert_right(await scenario(url))

        await connected()
