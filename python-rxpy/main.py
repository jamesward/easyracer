import asyncio
import datetime
import inspect
import uuid
from typing import Callable, Coroutine, Any

import aiohttp
import reactivex as rx
from reactivex import operators as ops
from reactivex.scheduler.eventloop import AsyncIOThreadSafeScheduler


# Note: Request creation code is intentionally not shared across scenarios
async def scenario1(make_url: Callable[[int], str]) -> rx.Observable[str]:
    url = make_url(int(inspect.currentframe().f_code.co_name[8:]))

    async def _req():
        async with aiohttp.ClientSession() as session:
            async with session.get(url) as response:
                return await response.text()

    def req(): return rx.from_future(asyncio.ensure_future(_req()))

    return rx.merge(req(), req()).pipe(ops.first())


async def scenario2(make_url: Callable[[int], str]):
    url = make_url(int(inspect.currentframe().f_code.co_name[8:]))

    async def http_get():
        async with aiohttp.ClientSession() as session:
            async with session.get(url) as response:
                return await response.text()

    def req(): return rx.from_future(asyncio.ensure_future(http_get())).pipe(ops.catch(rx.empty()))

    return rx.merge(req(), req()).pipe(ops.first())


async def scenario3(make_url: Callable[[int], str]):
    url = make_url(int(inspect.currentframe().f_code.co_name[8:]))

    async def _req():
        async with aiohttp.ClientSession() as session:
            async with session.get(url) as response:
                return await response.text()

    def req(): return rx.from_future(asyncio.ensure_future(_req()))

    return rx.merge(*[req() for req in [req] * 10_000]).pipe(ops.first())


async def scenario4(make_url: Callable[[int], str]):
    url = make_url(int(inspect.currentframe().f_code.co_name[8:]))

    async def _req():
        async with aiohttp.ClientSession() as session:
            async with session.get(url) as response:
                return await response.text()

    def req(): return rx.from_future(asyncio.ensure_future(_req()))

    def cancel(): raise asyncio.CancelledError()

    return rx.merge(
        req().pipe(
            ops.timeout(datetime.timedelta(seconds=1)),
            ops.catch(rx.from_callable(cancel)), # So that aiohttp request is cancelled
            ops.catch(rx.empty())
        ),
        req()
    ).pipe(ops.first())


async def scenario5(make_url: Callable[[int], str]):
    url = make_url(int(inspect.currentframe().f_code.co_name[8:]))

    async def _req():
        async with aiohttp.ClientSession() as session:
            async with session.get(url) as response:
                response.raise_for_status()
                return await response.text()

    def req(): return rx.from_future(asyncio.ensure_future(_req())).pipe(ops.catch(rx.empty()))

    return rx.merge(req(), req()).pipe(ops.first())


async def scenario6(make_url: Callable[[int], str]):
    url = make_url(int(inspect.currentframe().f_code.co_name[8:]))

    async def _req():
        async with aiohttp.ClientSession() as session:
            async with session.get(url) as response:
                response.raise_for_status()
                return await response.text()

    def req(): return rx.from_future(asyncio.ensure_future(_req())).pipe(ops.catch(rx.empty()))

    return rx.merge(req(), req(), req()).pipe(ops.first())


async def scenario7(make_url: Callable[[int], str]) -> rx.Observable[str]:
    url = make_url(int(inspect.currentframe().f_code.co_name[8:]))

    async def _req():
        async with aiohttp.ClientSession() as session:
            async with session.get(url) as response:
                return await response.text()

    def req(): return rx.from_future(asyncio.ensure_future(_req()))
    hedge_req = rx.of(()).pipe(
        ops.delay(datetime.timedelta(seconds=3)),
        ops.flat_map(lambda _: req())
    )

    return rx.merge(req(), hedge_req).pipe(ops.first())


async def scenario8(make_url: Callable[[int], str]) -> rx.Observable[str]:
    base_url = make_url(int(inspect.currentframe().f_code.co_name[8:]))

    async def _req(url: str):
        async with aiohttp.ClientSession() as session:
            async with session.get(url) as response:
                response.raise_for_status()
                return await response.text()

    def open_req(): return rx.from_future(asyncio.ensure_future(_req(f"{base_url}?open")))

    def use_req(res: str): return rx.from_future(asyncio.ensure_future(_req(f"{base_url}?use={res}")))

    def close_req(res: str): return rx.from_future(asyncio.ensure_future(_req(f"{base_url}?close={res}")))

    def res_req():
        return open_req().pipe(
            ops.flat_map(
                lambda res: use_req(res).pipe(
                    ops.catch(rx.of(None)),
                    ops.flat_map(
                        lambda result: close_req(res).pipe(
                            ops.flat_map(
                                lambda _: rx.empty() if result is None else rx.of(result)
                            )
                        )
                    )
                )
            )
        )

    return rx.merge(res_req(), res_req()).pipe(ops.first())


async def scenario9(make_url: Callable[[int], str]):
    url = make_url(int(inspect.currentframe().f_code.co_name[8:]))

    async def _req():
        async with aiohttp.ClientSession() as session:
            async with session.get(url) as response:
                response.raise_for_status()
                return await response.text()

    def req(): return rx.from_future(asyncio.ensure_future(_req())).pipe(ops.catch(rx.empty()))

    return rx.merge(*[req() for req in [req] * 10]).pipe(
        ops.reduce(lambda x, y: x + y)
    )


async def scenario10(make_url: Callable[[int], str]) -> rx.Observable[str]:
    base_url = make_url(int(inspect.currentframe().f_code.co_name[8:]))
    req_id = uuid.uuid4()

    async def _req(url: str):
        async with aiohttp.ClientSession() as session:
            async with session.get(url) as response:
                response.raise_for_status()
                return await response.text()

    def blocking(): return rx.repeat_value(()).pipe(
        ops.reduce(lambda accum, _: accum + 1, seed=0)
    )

    def blocker(): return rx.from_future(asyncio.ensure_future(_req(f"{base_url}?{req_id}")))

    def reporter():
        return rx.from_future(asyncio.ensure_future(_req(f"{base_url}?{req_id}={load}")))

    return rx.merge(res_req(), res_req()).pipe(ops.first())


scenarios: list[Callable[[Callable[[int], str]], Coroutine[Any, Any, rx.Observable[str]]]] = [
    scenario1,
    scenario2,
    scenario3,
    scenario4,
    scenario5,
    scenario6,
    scenario7,
    scenario8,
    scenario9,
]


async def main():
    scheduler = AsyncIOThreadSafeScheduler(asyncio.get_event_loop())
    sem = asyncio.Semaphore(0)

    for scenario in scenarios:
        scenario_observable = await scenario(lambda num: f"http://localhost:8080/{num}")
        scenario_observable.subscribe(
            on_next=lambda value: print(scenario.__name__, value),
            on_completed=lambda: sem.release(),
            scheduler=scheduler
        )
        await sem.acquire()


if __name__ == "__main__":
    asyncio.run(main())
