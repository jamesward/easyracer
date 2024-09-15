import asyncio
import datetime
import hashlib
import random
import uuid
from collections import namedtuple
from typing import Callable, Coroutine, Any

import aiohttp
import psutil
import reactivex as rx
from reactivex import operators as ops


# Add >> to reactivex.Observable
rx.Observable.__rshift__ = lambda self, op: self.pipe(op)


# Note: Request creation code is intentionally not shared across scenarios
def scenario1(url: str) -> rx.Observable[str]:
    async def _req():
        async with aiohttp.ClientSession() as session:
            async with session.get(url) as response:
                return await response.text()

    def req(): return rx.from_future(asyncio.ensure_future(_req()))

    return rx.merge(req(), req()) >> ops.first()
    # Or:
    # return rx.merge(req(), req()).pipe(ops.first())


def scenario2(url: str):
    async def http_get():
        async with aiohttp.ClientSession() as session:
            async with session.get(url) as response:
                return await response.text()

    def req(): return rx.from_future(asyncio.ensure_future(http_get())) >> ops.catch(rx.empty())

    return rx.merge(req(), req()) >> ops.first()
    # Or:
    # def req(): return rx.from_future(asyncio.ensure_future(http_get())).pipe(ops.catch(rx.empty()))
    #
    # return rx.merge(req(), req()).pipe(ops.first())


def scenario3(url: str):
    async def _req():
        async with aiohttp.ClientSession() as session:
            async with session.get(url) as response:
                return await response.text()

    def req(): return rx.from_future(asyncio.ensure_future(_req()))

    return rx.merge(*[req() for req in [req] * 10_000]) >> ops.first()
    # Or:
    # return rx.merge(*[req() for req in [req] * 10_000]).pipe(ops.first())


def scenario4(url: str):
    async def _req():
        async with aiohttp.ClientSession() as session:
            async with session.get(url) as response:
                return await response.text()

    def req(): return rx.from_future(asyncio.ensure_future(_req()))

    return rx.merge(
        req() >> ops.timeout(datetime.timedelta(seconds=1)) >> ops.catch(rx.empty()),
        req()
    ) >> ops.first()
    # Or:
    # return rx.merge(
    #     req().pipe(
    #         ops.timeout(datetime.timedelta(seconds=1)),
    #         ops.catch(rx.empty())
    #     ),
    #     req()
    # ).pipe(ops.first())


def scenario5(url: str):
    async def _req():
        async with aiohttp.ClientSession() as session:
            async with session.get(url) as response:
                response.raise_for_status()
                return await response.text()

    def req(): return rx.from_future(asyncio.ensure_future(_req())) >> ops.catch(rx.empty())

    return rx.merge(req(), req()) >> ops.first()
    # Or:
    # def req(): return rx.from_future(asyncio.ensure_future(_req())).pipe(ops.catch(rx.empty()))
    # 
    # return rx.merge(req(), req()).pipe(ops.first())


def scenario6(url: str):
    async def _req():
        async with aiohttp.ClientSession() as session:
            async with session.get(url) as response:
                response.raise_for_status()
                return await response.text()

    def req(): return rx.from_future(asyncio.ensure_future(_req())) >> ops.catch(rx.empty())

    return rx.merge(req(), req(), req()) >> ops.first()
    # Or:
    # def req(): return rx.from_future(asyncio.ensure_future(_req())).pipe(ops.catch(rx.empty()))
    # 
    # return rx.merge(req(), req(), req()).pipe(ops.first())


def scenario7(url: str) -> rx.Observable[str]:
    async def _req():
        async with aiohttp.ClientSession() as session:
            async with session.get(url) as response:
                return await response.text()

    def req(): return rx.from_future(asyncio.ensure_future(_req()))
    hedge_req = rx.of(()) >> ops.delay(datetime.timedelta(seconds=3)) >> ops.flat_map(lambda _: req())

    return rx.merge(req(), hedge_req) >> ops.first()
    # Or:
    # hedge_req = rx.of(()).pipe(
    #     ops.delay(datetime.timedelta(seconds=3)),
    #     ops.flat_map(lambda _: req())
    # )
    # 
    # return rx.merge(req(), hedge_req).pipe(ops.first())


def scenario8(base_url: str) -> rx.Observable[str]:
    async def _req(url: str):
        async with aiohttp.ClientSession() as session:
            async with session.get(url) as response:
                response.raise_for_status()
                return await response.text()

    def open_req(): return rx.from_future(asyncio.ensure_future(_req(f"{base_url}?open")))

    def use_req(res: str): return rx.from_future(asyncio.ensure_future(_req(f"{base_url}?use={res}")))

    def close_req(res: str): return rx.from_future(asyncio.ensure_future(_req(f"{base_url}?close={res}")))

    def res_req():
        return open_req() >> ops.flat_map(
            lambda res: use_req(res) >> ops.catch(rx.of(None)) >> ops.flat_map(
                lambda result: close_req(res) >> ops.flat_map(
                    lambda _: rx.empty() if result is None else rx.of(result)
                )
            )
        )

    return rx.merge(res_req(), res_req()) >> ops.first()
    # Or:
    # def res_req():
    #     return open_req().pipe(
    #         ops.flat_map(
    #             lambda res: use_req(res).pipe(
    #                 ops.catch(rx.of(None)),
    #                 ops.flat_map(
    #                     lambda result: close_req(res).pipe(
    #                         ops.flat_map(
    #                             lambda _: rx.empty() if result is None else rx.of(result)
    #                         )
    #                     )
    #                 )
    #             )
    #         )
    #     )
    # 
    # return rx.merge(res_req(), res_req()).pipe(ops.first())


def scenario9(url: str):
    async def _req():
        async with aiohttp.ClientSession() as session:
            async with session.get(url) as response:
                response.raise_for_status()
                return await response.text()

    def req(): return rx.from_future(asyncio.ensure_future(_req())) >> ops.catch(rx.empty())

    return rx.merge(*[req() for req in [req] * 10]) >> ops.reduce(lambda x, y: x + y)
    # Or:
    # def req(): return rx.from_future(asyncio.ensure_future(_req())).pipe(ops.catch(rx.empty()))
    # 
    # return rx.merge(*[req() for req in [req] * 10]).pipe(
    #     ops.reduce(lambda x, y: x + y)
    # )


def scenario10(base_url: str) -> rx.Observable[str]:
    req_id = uuid.uuid4()
    p = psutil.Process()
    Response = namedtuple("Response", ["status", "body_text"])

    async def _req(url: str):
        async with aiohttp.ClientSession() as session:
            async with session.get(url) as response:
                response.raise_for_status()
                return Response(response.status, await response.text())

    def _busy(bs: bytes) -> bytes:
        m = hashlib.sha512()
        m.update(bs)
        return m.digest()

    def blocking(): return rx.repeat_value(()) \
        >> ops.scan(lambda accum, _: _busy(accum), seed=random.randbytes(512)) \
        >> ops.map(lambda _: None)
    # Or:
    # def blocking(): return rx.repeat_value(()).pipe(
    #     ops.scan(lambda accum, _: _busy(accum), seed=random.randbytes(512)),
    #     ops.map(lambda _: None)
    # )

    def blocker(): return rx.from_future(asyncio.ensure_future(_req(f"{base_url}?{req_id}")))

    def reporter():
        def handle_response(response: Response):
            match response.status:
                case 200:
                    return rx.of(response.body_text)
                case 302:
                    return rx.of(()) >> ops.delay(datetime.timedelta(seconds=1)) >> ops.flat_map(reporter())
                    # Or:
                    # return rx.of(()).pipe(
                    #     ops.delay(datetime.timedelta(seconds=1)),
                    #     ops.flat_map(reporter())
                    # )

        with p.oneshot():
            load = p.cpu_percent()
            return rx.from_future(
                asyncio.ensure_future(_req(f"{base_url}?{req_id}={load}"))
            ) >> ops.flat_map(handle_response)
            # Or:
            # return rx.from_future(
            #     asyncio.ensure_future(_req(f"{base_url}?{req_id}={load}"))
            # ).pipe(
            #     ops.flat_map(handle_response)
            # )

    return rx.merge(
        rx.merge(
            blocking(), blocker()
        ) >> ops.first(lambda value: value is not None) >> ops.map(lambda _: None),
        reporter()
    ) >> ops.first(lambda value: value is not None)
    # Or:
    # return rx.merge(
    #     rx.merge(blocking(), blocker()).pipe(
    #         ops.first(lambda value: value is not None),
    #         ops.map(lambda _: None)
    #     ),
    #     reporter()
    # ).pipe(
    #     ops.first(lambda value: value is not None),
    # )


scenarios: list[Callable[[str], Coroutine[Any, Any, rx.Observable[str]]]] = [
    scenario1,
    scenario2,
    scenario3,
    scenario4,
    scenario5,
    scenario6,
    scenario7,
    scenario8,
    scenario9,
    scenario10,
]


async def main():
    for scenario in scenarios:
        num = scenario.__name__[8:]
        url = f"http://localhost:8080/{num}"
        await scenario(url) >> ops.do_action(
            lambda value: print(f"{scenario.__name__}: {value}")
        )


if __name__ == "__main__":
    asyncio.run(main())
