import pytest
import aiohttp
import main
from testcontainers.core.generic import DockerContainer
#from testcontainers.core.waiting_utils import wait_container_is_ready


@pytest.mark.asyncio
async def test_all():
    # todo: no way to set pull policy yet
    docker_container = DockerContainer("ghcr.io/jamesward/easyracer").with_exposed_ports(8080)
    with docker_container as easyracer:
        port = easyracer.get_exposed_port(8080)

        # todo: can't find the right way to wait on the container being ready
        import time
        time.sleep(2)

        #@wait_container_is_ready()
        async def connected():
            result1 = await main.scenario1(port)
            assert result1 == "right"

            result2 = await main.scenario2(port)
            assert result2 == "right"

            result3 = await main.scenario3(port)
            assert result3 == "right"

            result4 = await main.scenario4(port)
            assert result4 == "right"

            result5 = await main.scenario5(port)
            assert result5 == "right"

            result6 = await main.scenario6(port)
            assert result6 == "right"

            result7 = await main.scenario7(port)
            assert result7 == "right"

            result8 = await main.scenario8(port)
            assert result8 == "right"

            result9 = await main.scenario9(port)
            assert result9 == "right"

        await connected()
