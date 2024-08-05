import asyncio

import pytest
from reactivex.scheduler.eventloop import AsyncIOThreadSafeScheduler
from testcontainers.core.generic import DockerContainer

import main


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
            scheduler = AsyncIOThreadSafeScheduler(asyncio.get_event_loop())
            sem = asyncio.Semaphore(0)

            def assert_right(actual: str):
                assert actual == "right"
            for scenario in main.scenarios:
                scenario_observable = await scenario(lambda num: f"http://localhost:{port}/{num}")
                scenario_observable.subscribe(
                    on_next=lambda value: assert_right(value),
                    on_completed=lambda: sem.release(),
                    scheduler=scheduler
                )
                await sem.acquire()


        await connected()
